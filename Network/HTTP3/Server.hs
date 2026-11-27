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
    QEncoderConfig,
    defaultQEncoderConfig,
    ecMaxTableCapacity,
    ecHeaderBlockBufferSize,
    ecInstructionBufferSize,
    ecUseHuffman,
    QDecoderConfig,
    defaultQDecoderConfig,
    dcMaxTableCapacity,
    dcHuffmanBufferSize,
    dcBlockedStreams,
    dcMaxFieldSectionSize,
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
    StreamTerminated (..),
    ApplicationProtocolError (..),

    -- * Internal
    runIO,
    runIOWith,
    ServerIO (..),
) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified System.ThreadManager as T
import qualified Control.Monad.STM as STM
import Data.IORef
import GHC.Conc.Sync
import Network.HTTP.Semantics
import qualified Network.HTTP.Semantics.Error as HE
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal
import qualified Network.HPACK as HPACK
import Network.HTTP2.Server.Internal (ServerIO (..))
import Network.QUIC (Connection, ConnectionInfo (..), Stream, getConnectionInfo)
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Types.Int64Map as IntMap

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
runServer mstop conn conf server = withContext conn conf (fromMaybe STM.retry mstop) (return ()) $ \ctx -> do
    myThreadId >>= \t -> labelThread t "H3 server: run"
    let serve = readerServer ctx $ \strm -> do
            state <- newIORef NotStarted
            -- 'processRequest' handles its own exceptions; only asynchronous ones
            -- and a connection error leave it, and the thread manager takes the
            -- asynchronous ones that it threw.
            forkAppFinally
                ctx
                "H3 server: processRequest"
                ( do
                    writeIORef state Started
                    processRequest ctx server strm
                        `E.catch` \e -> case e of
                            QUIC.ApplicationProtocolErrorIsSent _ _ -> do
                                writeIORef state ConnectionClosed
                                E.throwIO e
                            _ -> E.throwIO e
                )
                (finish ctx strm state)
    maybe serve (serveGoingAway ctx serve . (True <$)) mstop
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
                    closeOrStop ctx strm
                Started -> closeOrStop ctx strm
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
goingAway ctx told =
    -- The peer's normal close is told by the reader, through 'accept'.
    -- Thrown from here, it made runWith and runIOWith throw on a normal end.
    going `E.catch` \e -> case e of
        QUIC.ConnectionIsClosed _ -> atomically $ stoppingSTM ctx
        _ -> E.throwIO e
  where
    -- Ending with the context as well, whose end it may not be told of.
    going = do
        mtwoSteps <- atomically $ (Just <$> told) `orElse` (Nothing <$ stoppingSTM ctx)
        case mtwoSteps of
            Nothing -> return ()
            Just twoSteps -> do
                when twoSteps $ do
                    goAwayAll ctx
                    waitPTO ctx
                goAway ctx
                atomically $ drained ctx `orElse` stoppingSTM ctx

-- | Serving until the connection is over, or once told to go away, until
--   the requests taken are done.
serveGoingAway :: Context -> IO () -> STM Bool -> IO ()
serveGoingAway ctx serve told = do
    g <- forkLibWithResult ctx "H3 going away" $ goingAway ctx told
    s <- forkLibWithResult ctx "H3 server" serve
    void $ T.waitEitherManaged g s

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
runIOServer mstop conn conf action = withContext conn conf (fromMaybe STM.retry mstop) (return ()) $ \ctx -> do
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
                    translating (Just $ QUIC.streamId strm) (sendResponseIO ctx strm rsp) `E.finally` do
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
    -- Library threads, told to stop with a grace when the context ends;
    -- the application is the user's, killed then.  Under async, they were
    -- killed at once on leaving, and the application was waited for even
    -- once the connection was over.
    let reader = readerServer ctx $ \strm ->
            forkManaged ctx "H3 server: processRequestIO" $ do
                let sid = QUIC.streamId strm
                src <- newSourceOrStop ctx strm
                handed <-
                    processRequestIO ctx (put src) strm src
                        `E.onException` finish sid (Just src)
                unless handed $ finish sid (Just src)
    g <-
        forkLibWithResult ctx "H3 going away" $
            goingAway ctx $ (False <$ appDone) <|> maybe STM.retry (True <$) mstop
    a <- forkAppWithResult ctx "H3 runIO application" io
    r <- forkLibWithResult ctx "H3 runIO reader" reader
    -- Once the application is done, nobody reads the requests handed over
    -- any more: they are rejected, so that the client may retry them, and
    -- done with.  Left, they held the GOAWAY's drain until the idle timeout.
    let rejectLeft = do
            m <- atomically $ (Just <$> readTQueue reqq) `orElse` (Nothing <$ stoppingSTM ctx)
            forM_ m $ \(strm, _) -> do
                let sid = QUIC.streamId strm
                QUIC.resetStream strm H3RequestRejected
                msrc <- atomically $ do
                    srcm <- readTVar srcs
                    writeTVar srcs $ IntMap.delete sid srcm
                    return $ IntMap.lookup sid srcm
                finish sid msrc
                rejectLeft
    forkManaged ctx "H3 runIO rejecting what is left" $ do
        told <- atomically $ (True <$ appDone) `orElse` (False <$ stoppingSTM ctx)
        when told rejectLeft
    let ended m = T.waitManagedSTM m >>= either throwSTM (\_ -> return ())
        failed m = T.waitManagedSTM m >>= either throwSTM (\_ -> STM.retry)
        wait3 = atomically $ ended g `orElse` ended r `orElse` failed a
    -- Again once if told it is blocked indefinitely, as async waits.
    wait3 `E.catch` \E.BlockedIndefinitelyOnSTM -> wait3

-- | A request stream ended before its HEADERS: RFC 9114, section 4.1,
--   says H3_REQUEST_INCOMPLETE, and a reset of the client's is answered as
--   cancelled.  H3_MESSAGE_ERROR was sent for either.
endedEarly :: Stream -> IO ()
endedEarly strm = do
    mreset <- QUIC.resetReceived strm
    QUIC.resetStream strm $ maybe H3RequestIncomplete (const H3RequestCancelled) mreset

readerServer :: Context -> (Stream -> IO ()) -> IO ()
readerServer ctx action = loop
  where
    loop = accept ctx >>= mapM_ (\strm -> process strm >> loop)
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
    -> IO ()
processRequest ctx server strm = E.handle reset $ do
    src <- newSource strm
    (`E.finally` cancelUnlessReadToEnd ctx sid src) $ do
        emvt <- E.try $ recvHeader ctx sid src
        case emvt of
            Left FieldSectionTooLarge -> refuseTooLarge ctx strm
            Right Nothing -> endedEarly strm
            Right (Just ht) -> do
                mreq <- mkRequest ctx strm src ht
                case mreq of
                    -- Malformed; 'mkRequest' has reset the stream.
                    Nothing -> return ()
                    Just req -> do
                        let aux =
                                defaultAux
                                    { auxMySockAddr = getMySockAddr ctx
                                    , auxPeerSockAddr = getPeerSockAddr ctx
                                    , auxSendInformational = sendInformational ctx strm
                                    }
                        -- No limit on the handler, but the client must
                        -- still be there: the connection is kept alive.
                        keepingAlive ctx $ server req aux $ sendResponse ctx strm
  where
    sid = QUIC.streamId strm
    reset se
        -- Killed, with the connection.  Whatever was
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
        -- The same in the form the body reader gives.  The stream is reset,
        -- since a FIN could reach the client before the CONNECTION_CLOSE.
        | Just (HE.ConnectionErrorIsSent _ _) <- E.fromException se = do
            QUIC.resetStream strm H3InternalError
            E.throwIO se
        -- The body reader said why, as an 'HE.HTTPError': the request is
        -- malformed, or the client reset the stream.  On this stream only:
        -- an upstream's, a proxy's, may not even have an HTTP/3 code.
        | Just (HE.StreamErrorIsSent code sid' _) <- E.fromException se
        , sid' == sid
        , HE.errorSpace code == HE.HTTP3 =
            QUIC.resetStream strm $
                QUIC.ApplicationProtocolError $
                    fromIntegral $
                        HE.errorValue code
        | Just (HE.StreamErrorIsReceived _ sid') <- E.fromException se
        , sid' == sid =
            QUIC.resetStream strm H3RequestCancelled
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
        -- The stream or the connection went under the handler: the peer's
        -- doing, not to be printed.  quic's own failure is printed.
        | Just e <- E.fromException se, notBadThing e =
            QUIC.resetStream strm H3InternalError
        -- An upstream's end, as over HTTP/2: not printed.
        | Just (_ :: HE.HTTPError) <- E.fromException se =
            QUIC.resetStream strm H3InternalError
        -- Anything else is the handler's own failure.  H3_MESSAGE_ERROR
        -- was sent for it, which says the request was malformed.  It is
        -- passed on to be printed, as over HTTP/2.
        | otherwise = do
            QUIC.resetStream strm H3InternalError
            E.throwIO se
    -- quic's own failure, not the connection taken down from outside.
    notBadThing (QUIC.BadThingHappen e) = isAsyncException e
    notBadThing _ = True
    malformed se
        | Just (_ :: ContentLengthMismatch) <- E.fromException se = True
        | Just (_ :: ConnectionSpecificField) <- E.fromException se = True
        | Just FieldSectionTooLarge <- E.fromException se = True
        -- A field the header section must not have, found by the decoder.
        | Just (_ :: HPACK.DecodeError) <- E.fromException se = True
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
            refuseTooLarge ctx strm
            False <$ closeOrStop ctx strm
        Right Nothing -> False <$ endedEarly strm
        Right (Just ht) -> do
            mreq <- mkRequest ctx strm src ht
            case mreq of
                Nothing -> return False
                Just req -> True <$ put (strm, req)
  where
    sid = QUIC.streamId strm
    reset se
        -- Killed, or stopping with the context, before the
        -- request was handed over: rejected, so that the client may retry
        -- it, as 'run' does (RFC 9114, section 4.1.1).
        | isAsyncException se || isJust (E.fromException se :: Maybe Stopping) = do
            QUIC.resetStream strm H3RequestRejected
            E.throwIO se
        | Just (_ :: DecodeError) <- E.fromException se =
            abort ctx QpackDecompressionFailed
        | Just (QUIC.ApplicationProtocolErrorIsSent _ _) <- E.fromException se =
            return False
        -- quic's own failure is printed, as in 'run'.
        | Just (QUIC.BadThingHappen e) <- E.fromException se
        , not (isAsyncException e) = do
            QUIC.resetStream strm H3InternalError
            E.throwIO se
        -- The stream or the connection went under it: the peer's doing.
        | Just (_ :: QUIC.QUICException) <- E.fromException se =
            False <$ QUIC.resetStream strm H3InternalError
        | otherwise = False <$ QUIC.resetStream strm H3MessageError

-- | Answering a request whose header section is more than we said we would
--   take.
--
-- RFC 9114, section 4.2.2: "A server that receives a larger field section
-- than it is willing to handle can send an HTTP 431 (Request Header Fields
-- Too Large) status code".  What is left of the request is not wanted, and
-- section 4.1.1 lets a server say so with STOP_SENDING and H3_NO_ERROR.
--
-- Given up once stopping, with 'Stopping': a peer which grants the stream
-- no credit would otherwise hold a library thread.
refuseTooLarge :: Context -> Stream -> IO ()
refuseTooLarge ctx strm = do
    QUIC.stopStream strm H3NoError
    bss <- encodeHeaderFrame ctx strm [(":status", "431")]
    giving $ QUIC.sendStreamManyWith strm bss $ stoppingSTM ctx
    giving $ QUIC.shutdownStreamWith strm $ stoppingSTM ctx
  where
    giving act = act >>= either (\_ -> E.throwIO Stopping) return

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
    :: Context -> Stream -> Response -> [PushPromise] -> IO ()
sendResponse ctx strm (Response outobj) _pp = do
    sendHeader ctx strm $ outObjHeaders outobj
    done <- sendBody ctx strm outobj
    when done $ QUIC.shutdownStream strm

sendResponseIO
    :: Context -> Stream -> Response -> IO ()
sendResponseIO ctx strm (Response outobj) =
    -- A response that fails is reset, as in 'run'.  It was left as it was,
    -- neither ended nor released, and the client waited for the rest.
    ( do
        sendHeader ctx strm $ outObjHeaders outobj
        -- A cancelled body has reset the stream, and this only releases it.
        _ <- sendBody ctx strm outobj
        closeOrStop ctx strm
    )
        `E.onException` QUIC.resetStream strm H3InternalError

-- | Closing a request stream with H3_NO_ERROR, giving up waiting for the
--   FIN once we are stopping: the stream is closed and released either way.
closeOrStop :: Context -> Stream -> IO ()
closeOrStop ctx strm = void $ QUIC.closeStreamWith strm H3NoError $ stoppingSTM ctx
