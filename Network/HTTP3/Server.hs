{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A server library for HTTP/3.
module Network.HTTP3.Server (
    -- * Runner
    run,
    runWith,

    -- * Common configuration
    Config,
    confHooks,
    confQEncoderConfig,
    confQDecoderConfig,
    confPositionReadMaker,
    confTimeoutManager,
    defaultConfig,
    allocSimpleConfig,
    freeSimpleConfig,
    defaultQEncoderConfig,
    ecMaxTableCapacity,
    ecHeaderBlockBufferSize,
    ecInstructionBufferSize,
    defaultQDecoderConfig,
    dcMaxTableCapacity,
    dcHuffmanBufferSize,
    Hooks,
    onControlFrameCreated,
    onHeadersFrameCreated,
    onControlStreamCreated,
    onEncoderStreamCreated,
    onDecoderStreamCreated,
    defaultHooks,

    -- * HTTP semantics
    module Network.HTTP.Semantics.Server,

    -- * Exceptions
    StreamResetIsReceived (..),
    ContentLengthMismatch (..),
    ConnectionSpecificField (..),
    FieldSectionTooLargeForPeer (..),
    ApplicationProtocolError (..),

    -- * Internal
    runIO,
    runIOWith,
    ServerIO (..),
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Control.Monad.STM as STM
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import GHC.Conc.Sync
import Network.HTTP.Semantics
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal
import Network.HTTP2.Server.Internal (ServerIO (..))
import Network.QUIC (Connection, ConnectionInfo (..), Stream, getConnectionInfo)
import qualified Network.QUIC as QUIC
import qualified System.TimeManager as T

import Imports
import Network.HTTP3.Config
import Network.HTTP3.Context
import Network.HTTP3.Error
import Network.HTTP3.Recv
import Network.HTTP3.Send
import Network.QPACK
import Network.QPACK.Internal

-- | Running an HTTP\/3 server.
run :: Connection -> Config -> Server -> IO ()
run = runServer Nothing

-- | Like 'run', but the connection goes away once the 'STM' action fires:
--   two GOAWAYs a PTO apart (RFC 9114, section 5.2), request streams from
--   the second one's ID on rejected, and the connection ended when the
--   requests taken before it are answered.
--
--   @since 0.1.10
runWith :: STM () -> Connection -> Config -> Server -> IO ()
runWith = runServer . Just

runServer :: Maybe (STM ()) -> Connection -> Config -> Server -> IO ()
runServer mstop conn conf server = withContext conn conf $ \ctx -> do
    myThreadId >>= \t -> labelThread t "H3 server: run"
    let serve = readerServer ctx $ \strm -> do
            state <- newIORef NotStarted
            -- 'processRequest' handles its own exceptions; only asynchronous ones
            -- and a connection error leave it, and the thread manager takes the
            -- asynchronous ones that it threw.
            forkManagedTimeoutFinally
                ctx
                "H3 server: processRequest"
                ( \th -> do
                    writeIORef state Started
                    processRequest ctx server strm th
                        `E.catch` \e -> case e of
                            QUIC.ApplicationProtocolErrorIsSent _ _ -> do
                                writeIORef state ConnectionClosed
                                E.throwIO e
                            _ -> E.throwIO e
                )
                (finish ctx strm state)
    maybe serve (\stop -> race_ (goingAway ctx (True <$ stop)) serve) mstop
  where
    -- A thread stopped before it began, as the connection goes, has not
    -- looked at the request: it is rejected, which tells the client it may
    -- retry (RFC 9114, section 4.1.1).  The stream used to end with a FIN
    -- and nothing on it, which the client could only take for malformed.
    --
    -- A thread that closed the connection leaves the stream alone: a FIN
    -- could reach the client before the CONNECTION_CLOSE, as an empty response.
    --
    -- The request is done with however that goes: a stream or connection
    -- already gone must not leave it in flight for a GOAWAY to wait on.
    finish ctx strm state =
        ( do
            s <- readIORef state
            case s of
                NotStarted -> do
                    QUIC.resetStream strm H3RequestRejected
                    QUIC.closeStreamWith strm H3NoError
                Started -> QUIC.closeStreamWith strm H3NoError
                ConnectionClosed -> return ()
        )
            `E.finally` requestDone ctx (QUIC.streamId strm)

-- | How far a request thread of 'run' got.
data RequestState = NotStarted | Started | ConnectionClosed

-- | Once told: GOAWAY, and the requests taken before it answered.
--
-- Told to stop, in two steps (RFC 9114, section 5.2): the largest
-- identifier first, and the last request taken once a PTO has passed, so
-- that requests already on their way are taken.  An application that is
-- done answers nothing more, so its GOAWAY is the last one at once.
goingAway :: Context -> STM Bool -> IO ()
goingAway ctx told = do
    twoSteps <- atomically told
    when twoSteps $ do
        goAwayAll ctx
        waitPTO ctx
    goAway ctx
    atomically $ drained ctx

runIO :: Connection -> Config -> (ServerIO Stream -> IO (IO ())) -> IO ()
runIO = runIOServer Nothing

-- | Like 'runIO', but the connection goes away once the 'STM' action
--   fires, as with 'runWith'.
--
--   @since 0.1.10
runIOWith
    :: STM () -> Connection -> Config -> (ServerIO Stream -> IO (IO ())) -> IO ()
runIOWith = runIOServer . Just

runIOServer
    :: Maybe (STM ())
    -> Connection
    -> Config
    -> (ServerIO Stream -> IO (IO ()))
    -> IO ()
runIOServer mstop conn conf action = withContext conn conf $ \ctx -> do
    info <- getConnectionInfo conn
    reqq <- newTQueueIO
    done <- newTVarIO False
    -- The bodies of the requests handed over, as 'run' keeps each in its
    -- handler: a Stream Cancellation is owed for one not read to its end
    -- (RFC 9204, section 4.4.2).  None was ever sent.
    srcs <- newTVarIO IntMap.empty
    let finish sid msrc = do
            forM_ msrc $ cancelUnlessReadToEnd ctx sid
            requestDone ctx sid
        sio =
            ServerIO
                { sioMySockAddr = localSockAddr info
                , sioPeerSockAddr = remoteSockAddr info
                , sioReadRequest = atomically $ readTQueue reqq
                , sioWriteResponse = \strm rsp -> do
                    let sid = QUIC.streamId strm
                    sendResponseIO ctx strm rsp `E.finally` do
                        msrc <- atomically $ do
                            m <- readTVar srcs
                            writeTVar srcs $ IntMap.delete sid m
                            return $ IntMap.lookup sid m
                        finish sid msrc
                , -- Not quic's 'mainDone': its sender ends once that is set
                  -- and nothing is queued, before the responses still to come.
                  sioDone = atomically $ writeTVar done True
                }
        put src (strm, req) = atomically $ do
            modifyTVar' srcs $ IntMap.insert (QUIC.streamId strm) src
            writeTQueue reqq (strm, req)
        -- The application being done ends the connection as being told to
        -- does.  This used to wait for streams for good, and so the
        -- connection was left open until the peer or the idle timeout ended it.
        appDone = readTVar done >>= check
    io <- action sio
    -- A thread per stream, as in 'run'.  Reading HEADERS on the accept loop
    -- let one stream that never sent them stop every stream after it.
    race_ (goingAway ctx $ (False <$ appDone) <|> maybe STM.retry (True <$) mstop) $
        concurrently_ io $
            readerServer ctx $ \strm ->
                forkManagedTimeout ctx "H3 server: processRequestIO" $ \_th -> do
                    let sid = QUIC.streamId strm
                    src <- newSource strm
                    handed <-
                        processRequestIO ctx (put src) strm src
                            `E.onException` finish sid (Just src)
                    unless handed $ finish sid (Just src)

readerServer :: Context -> (Stream -> IO ()) -> IO ()
readerServer ctx action = loop
  where
    loop = do
        accept ctx >>= process
        loop
    process strm
        | QUIC.isClientInitiatedUnidirectional sid =
            forkManaged ctx "H3 server: unidirectional handler" $
                unidirectional ctx strm
        | QUIC.isClientInitiatedBidirectional sid = do
            admitted <- admitRequest ctx sid
            -- At or above our GOAWAY: not processed, so the client may
            -- retry it elsewhere (RFC 9114, section 5.2).
            if admitted
                then action strm
                else do
                    QUIC.stopStream strm H3RequestRejected
                    QUIC.resetStream strm H3RequestRejected
        | QUIC.isServerInitiatedUnidirectional sid = return () -- error
        | otherwise = return ()
      where
        sid = QUIC.streamId strm

processRequest
    :: Context
    -> Server
    -> Stream
    -> T.Handle
    -> IO ()
processRequest ctx server strm th = E.handle reset $ do
    src <- newSource strm
    (`E.finally` cancelUnlessReadToEnd ctx sid src) $ do
        emvt <- E.try $ recvHeader ctx sid src
        case emvt of
            Left FieldSectionTooLarge -> refuseTooLarge ctx strm th
            Right Nothing -> QUIC.resetStream strm H3MessageError
            Right (Just ht) -> do
                mreq <- mkRequest ctx strm src ht
                case mreq of
                    -- Malformed; 'mkRequest' has reset the stream.
                    Nothing -> return ()
                    Just req -> do
                        let aux =
                                defaultAux
                                    { auxTimeHandle = th
                                    , auxMySockAddr = getMySockAddr ctx
                                    , auxPeerSockAddr = getPeerSockAddr ctx
                                    }
                        server req aux $ sendResponse ctx strm th
  where
    sid = QUIC.streamId strm
    reset se
        -- Killed: by the timeout, or with the connection.  Whatever was
        -- sent of the response is not the whole of it, and is reset here.
        -- It used to be passed on as it was, and the finalizer
        -- ('closeStream') ended the stream with a FIN: the client took a
        -- response cut short for a whole one.  A response that has gone out
        -- in full has had its FIN already, and 'resetStream' does nothing.
        | isAsyncException se = do
            QUIC.resetStream strm H3InternalError
            E.throwIO se
        | Just (_ :: DecodeError) <- E.fromException se =
            abort ctx QpackDecompressionFailed
        -- A connection error has been sent: the stream goes with it.
        | Just (QUIC.ApplicationProtocolErrorIsSent _ _) <- E.fromException se =
            E.throwIO se
        -- The response was more than the client will take, and the
        -- application did nothing about it.  Our failure, not the client's.
        | Just (_ :: FieldSectionTooLargeForPeer) <- E.fromException se =
            QUIC.resetStream strm H3InternalError
        -- The request is malformed (RFC 9114, section 4.1.2): the body
        -- reader says so, and the handler passes it on.
        | malformed se = QUIC.resetStream strm H3MessageError
        -- The client reset the stream, and the handler gave up on it.
        | Just (_ :: StreamResetIsReceived) <- E.fromException se =
            QUIC.resetStream strm H3RequestCancelled
        -- Anything else is the handler's own failure.  H3_MESSAGE_ERROR
        -- was sent for it, which says the request was malformed.
        | otherwise = QUIC.resetStream strm H3InternalError
    malformed se
        | Just (_ :: ContentLengthMismatch) <- E.fromException se = True
        | Just (_ :: ConnectionSpecificField) <- E.fromException se = True
        | Just FieldSectionTooLarge <- E.fromException se = True
        | otherwise = False

-- | Answers whether the request was handed to the application.
processRequestIO
    :: Context -> ((Stream, Request) -> IO ()) -> Stream -> Source -> IO Bool
processRequestIO ctx put strm src = E.handle reset $ do
    emvt <- E.try $ recvHeader ctx sid src
    case emvt of
        -- Closed here, as 'run' does in 'finish': only a closed stream is
        -- given back to the client's limit on request streams.
        Left FieldSectionTooLarge -> do
            withHandle ctx (refuseTooLarge ctx strm)
            False <$ QUIC.closeStreamWith strm H3NoError
        Right Nothing -> False <$ QUIC.resetStream strm H3MessageError
        Right (Just ht) -> do
            mreq <- mkRequest ctx strm src ht
            case mreq of
                Nothing -> return False
                Just req -> True <$ put (strm, req)
  where
    sid = QUIC.streamId strm
    reset se
        -- Killed by the timeout, or with the connection, before the request
        -- was handed over.
        | isAsyncException se = do
            QUIC.resetStream strm H3RequestIncomplete
            E.throwIO se
        | Just (_ :: DecodeError) <- E.fromException se =
            abort ctx QpackDecompressionFailed
        | Just (QUIC.ApplicationProtocolErrorIsSent _ _) <- E.fromException se =
            return False
        | otherwise = False <$ QUIC.resetStream strm H3MessageError

-- | Answering a request whose header section is more than we said we would
--   take.
--
-- RFC 9114, section 4.2.2: "A server that receives a larger field section
-- than it is willing to handle can send an HTTP 431 (Request Header Fields
-- Too Large) status code".  What is left of the request is not wanted, and
-- section 4.1.1 lets a server say so with STOP_SENDING and H3_NO_ERROR.
refuseTooLarge :: Context -> Stream -> T.Handle -> IO ()
refuseTooLarge ctx strm th = do
    QUIC.stopStream strm H3NoError
    sendHeader ctx strm th [(":status", "431")]
    QUIC.shutdownStream strm

-- | Build the 'Request', or reset the stream and answer 'Nothing' when the
-- message is malformed.
--
-- RFC 9114 section 4.1.2 makes a malformed request one the server "MUST treat
-- \[...\] as malformed", responding or resetting -- either way it is not a
-- request to serve.  The stream was being reset and then the request handed to
-- the application regardless, so a message the server had already rejected
-- still reached whatever was running behind it, on a stream it could no
-- longer answer on.
mkRequest
    :: Context
    -> Stream
    -> Source
    -> (TokenHeaderList, ValueTable)
    -> IO (Maybe Request)
mkRequest ctx strm src ht@(_, vt) = do
    let mMethod = getFieldValue tokenMethod vt
        mScheme = getFieldValue tokenScheme vt
        mAuthority = getFieldValue tokenAuthority vt
        mPath = getFieldValue tokenPath vt
        malformed = do
            QUIC.resetStream strm H3MessageError
            return Nothing
    case (mMethod, mScheme, mAuthority, mPath) of
        _ | isJust (connectionSpecificField ht) -> malformed
        _ | isJust (invalidContentLength vt) -> malformed
        (Just "CONNECT", _, Just _, _) -> Just <$> build
        (Just _, Just _, Just _, Just _) -> Just <$> build
        _ -> malformed
  where
    build = do
        let sid = QUIC.streamId strm
        (readB, refH) <- newBodyReader ctx sid src vt False
        return $ Request $ InpObj ht Nothing readB refH

sendResponse
    :: Context -> Stream -> T.Handle -> Response -> [PushPromise] -> IO ()
sendResponse ctx strm th (Response outobj) _pp = do
    sendHeader ctx strm th $ outObjHeaders outobj
    done <- sendBody ctx strm th outobj
    when done $ QUIC.shutdownStream strm

sendResponseIO
    :: Context -> Stream -> Response -> IO ()
sendResponseIO ctx strm (Response outobj) =
    void $ withHandle ctx $ \th -> do
        sendHeader ctx strm th $ outObjHeaders outobj
        -- A cancelled body has reset the stream, and this only releases it.
        _ <- sendBody ctx strm th outobj
        QUIC.closeStreamWith strm H3NoError
