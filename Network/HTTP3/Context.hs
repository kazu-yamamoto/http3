{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Context (
    Context,
    withContext,
    unidirectional,
    cancelStream,
    isH3Server,
    isH3Client,
    accept,
    qpackEncode,
    qpackDecode,
    withHandle,
    newStream,
    closeStream,
    pReadMaker,
    abort,
    getHooks,
    Hooks (..), -- re-export
    getMySockAddr,
    getPeerSockAddr,
    getMaxFieldSectionSize,
    getPeerMaxFieldSectionSize,
    forkManaged,
    forkManagedTimeout,
    forkManagedTimeoutFinally,
    isAsyncException,
    admitRequest,
    requestDone,
    goAway,
    goAwayAll,
    waitPTO,
    drained,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forM_, void)
import Data.IORef
import Data.Maybe (isJust)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Network.ByteOrder (withWriteBuffer)
import Network.HTTP.Semantics.Client
import Network.QUIC
import Network.QUIC.Internal (Microseconds (..), connLDCC, encodeInt', getPTO, isClient, isServer)
import Network.Socket (SockAddr)
import qualified System.ThreadManager as T
import qualified System.TimeManager as TM
import System.Timeout (timeout)

import Network.HTTP3.Config
import Network.HTTP3.Control
import Network.HTTP3.Error
import Network.HTTP3.Frame
import Network.HTTP3.Stream
import Network.QPACK
import Network.QPACK.Internal

data Context = Context
    { ctxConnection :: Connection
    , ctxQEncoder :: QEncoder
    , ctxQDecoder :: QDecoder
    , ctxUniSwitch :: H3StreamType -> InstructionHandler
    , ctxPReadMaker :: PositionReadMaker
    , ctxThreadManager :: T.ThreadManager
    , ctxTimeoutManager :: TM.Manager
    -- ^ Owning a thread and timing it out are separate, so the
    --   thread manager carries no timer and this is kept here.
    , ctxHooks :: Hooks
    , ctxMySockAddr :: SockAddr
    , ctxPeerSockAddr :: SockAddr
    , ctxMaxFieldSectionSize :: Int
    -- ^ What we told the peer we would accept, and so the most of any one
    -- frame we are willing to hold in memory while it arrives.
    , ctxCancelStream :: StreamId -> IO ()
    , ctxPeerMaxFieldSectionSize :: IO Int
    -- ^ The peer's SETTINGS_MAX_FIELD_SECTION_SIZE, or 'maxBound' until its
    -- SETTINGS have arrived
    -- ^ Sending a Stream Cancellation on our QPACK decoder stream
    , ctxSendControl :: [H3Frame] -> IO ()
    , ctxGoAway :: TVar (Maybe StreamId)
    -- ^ Server only: the ID of our GOAWAY, from which no request is taken.
    , ctxLastRequest :: TVar (Maybe StreamId)
    -- ^ Server only: the largest request stream taken.
    , ctxInFlight :: TVar IntSet
    -- ^ Server only: the request streams taken and not yet done with.
    }

-- | Running an action with a context, and seeing this connection's own
--   threads out before returning: they are using the connection, which
--   the caller tears down once this returns.
--
-- The wait is capped because one of the threads waited for runs the
-- application's handler: an application which swallows the asynchronous
-- exception would otherwise keep this from ever returning.  quic waits
-- for its own threads with no cap, because those are all its own and are
-- known to end.
withContext :: Connection -> Config -> (Context -> IO a) -> IO a
withContext conn conf action = do
    ctx <- newContext conn conf
    let mgr = ctxThreadManager ctx
    T.stopAfter mgr (action ctx) $ \_ ->
        void $ timeout stopTimeout $ T.waitUntilAllGone mgr

-- | How long the threads are given to end after they have been told to.
--   Unwinding takes milliseconds; this is for one that does not stop.
stopTimeout :: Int
stopTimeout = 5000000

newContext :: Connection -> Config -> IO Context
newContext conn conf = do
    (sendEI, sendDI, ctxSendControl) <- setupUnidirectional conn conf
    (ctxQEncoder, handleDI, dyntblE) <- newQEncoder (confQEncoderConfig conf) sendEI
    -- newQDecoder passes dyntbl for decoder to handleEI internally
    (ctxQDecoder, handleEI) <- newQDecoder (confQDecoderConfig conf) sendDI
    let ctxMaxFieldSectionSize = dcMaxFieldSectionSize $ confQDecoderConfig conf
    cst <- newControlState
    ctl <- controlStream conn ctxMaxFieldSectionSize dyntblE cst <$> newIORef IInit
    let ctxPeerMaxFieldSectionSize = getHeaderSize dyntblE
    seen <- newIORef []
    info <- getConnectionInfo conn
    let handleDI' recv = handleDI recv `E.catch` abortWith QpackDecoderStreamError
        handleEI' recv = handleEI recv `E.catch` abortWith QpackEncoderStreamError
        ctxUniSwitch = switch conn seen ctl handleEI' handleDI'
        ctxPReadMaker = confPositionReadMaker conf
        ctxHooks = confHooks conf
        ctxMySockAddr = localSockAddr info
        ctxPeerSockAddr = remoteSockAddr info
    let ctxCancelStream sid
            -- RFC 9204, section 4.4.2: "A decoder with a maximum dynamic table
            -- capacity equal to zero MAY omit sending Stream Cancellations",
            -- since the encoder then has nothing to release.
            | dcMaxTableCapacity (confQDecoderConfig conf) == 0 = return ()
            | otherwise =
                -- The stream is being given up on, and so, quite possibly, is
                -- the connection; there is nobody to report a failure to.
                (encodeDecoderInstructions [StreamCancellation sid] >>= sendDI)
                    `E.catch` ignoreSync
    ctxThreadManager <- T.newThreadManager
    ctxGoAway <- newTVarIO Nothing
    ctxLastRequest <- newTVarIO Nothing
    ctxInFlight <- newTVarIO IntSet.empty
    let ctxTimeoutManager = confTimeoutManager conf
    let ctxConnection = conn
    return Context{..}
  where
    ignoreSync :: E.SomeException -> IO ()
    ignoreSync se
        | isAsyncException se = E.throwIO se
        | otherwise = return ()
    abortWith :: ApplicationProtocolError -> E.SomeException -> IO ()
    abortWith aerr se
        | isAsyncException se = E.throwIO se
        | otherwise = connectionError conn aerr

isAsyncException :: E.Exception e => e -> Bool
isAsyncException e =
    case E.fromException (E.toException e) of
        Just (E.SomeAsyncException _) -> True
        Nothing -> False

-- | The handler for a unidirectional stream the peer opened, by its type.
--
-- The control stream and the two QPACK streams are critical: a peer may open
-- each of them only once (RFC 9114, section 6.2.1; RFC 9204, section 4.2),
-- and may not close any of them.  The control stream handler sees to the
-- closing itself; the QPACK ones are library code that simply returns when
-- the stream ends.
switch
    :: Connection
    -> IORef [H3StreamType]
    -- ^ The critical streams the peer has opened so far
    -> InstructionHandler
    -> InstructionHandler
    -> InstructionHandler
    -> H3StreamType
    -> InstructionHandler
switch conn seen ctl handleEI handleDI styp = case styp of
    H3ControlStreams -> once ctl
    QPACKEncoderStream -> once $ closing handleEI
    QPACKDecoderStream -> once $ closing handleDI
    -- RFC 9114, section 6.2.2: "Only servers can push; if a server receives
    -- a client-initiated push stream, this MUST be treated as a connection
    -- error of type H3_STREAM_CREATION_ERROR."  And section 4.6: a client
    -- "MUST treat receipt of a push stream as a connection error of type
    -- H3_ID_ERROR when no MAX_PUSH_ID frame has been sent", which this client
    -- never does.
    H3PushStreams
        | isServer conn -> \_ -> connectionError conn H3StreamCreationError
        | otherwise -> \_ -> connectionError conn H3IdError
    -- 'unidirectional' does not get here with an unknown type.
    H3StreamTypeUnknown _ -> \_ -> return ()
  where
    once, closing :: InstructionHandler -> InstructionHandler
    once handler recv = do
        dup <- atomicModifyIORef' seen $ \ts -> (styp : ts, styp `elem` ts)
        if dup
            then connectionError conn H3StreamCreationError
            else handler recv
    closing handler recv = do
        handler recv
        connectionError conn H3ClosedCriticalStream

isH3Server :: Context -> Bool
isH3Server Context{..} = isServer ctxConnection

isH3Client :: Context -> Bool
isH3Client Context{..} = isClient ctxConnection

accept :: Context -> IO Stream
accept Context{..} = acceptStream ctxConnection

qpackEncode :: Context -> QEncoder
qpackEncode Context{..} = ctxQEncoder

qpackDecode :: Context -> QDecoder
qpackDecode Context{..} = ctxQDecoder

cancelStream :: Context -> StreamId -> IO ()
cancelStream Context{..} = ctxCancelStream

unidirectional :: Context -> Stream -> IO ()
unidirectional Context{..} strm = do
    -- The type is a variable-length integer (RFC 9114, section 6.2), so one,
    -- two, four or eight octets -- not the single one it used to be read as,
    -- which cut anything from 0x40 up in half and handed the tail of the type
    -- to a handler as though it were the stream's contents.
    mtyp <- recvQInt (recvStream strm)
    case mtyp of
        -- The peer opened a unidirectional stream and closed it without
        -- saying what it was for.  Nothing to dispatch to; this used to be a
        -- pattern match failure.
        --
        -- Closed here, as below, because only a stream we close is counted
        -- as done with, and so given back to the peer's limit on the
        -- unidirectional streams it may open.  One left unclosed is lost to
        -- it for good.
        Nothing -> closeStream strm
        Just i -> case toH3StreamType i of
            -- RFC 9114, section 6.2: "Recipients of unknown stream types MUST
            -- either abort reading of the stream or discard incoming data
            -- without further processing.  If reading is aborted, the
            -- recipient SHOULD use the H3_STREAM_CREATION_ERROR error code".
            -- Neither was done: whatever the peer sent was left where it
            -- arrived, for as long as the connection lasted.
            H3StreamTypeUnknown _ -> do
                stopStream strm H3StreamCreationError
                closeStream strm
            styp -> ctxUniSwitch styp (recvStream strm)

withHandle :: Context -> (TM.Handle -> IO ()) -> IO ()
withHandle Context{..} action = void $ TM.withHandle ctxTimeoutManager (return ()) action

newStream :: Context -> IO Stream
newStream Context{..} = stream ctxConnection

pReadMaker :: Context -> PositionReadMaker
pReadMaker = ctxPReadMaker

forkManaged :: Context -> String -> IO () -> IO ()
forkManaged Context{..} label =
    T.forkManaged ctxThreadManager label . endingOnConnectionError

forkManagedTimeout :: Context -> String -> (TM.Handle -> IO ()) -> IO ()
forkManagedTimeout Context{..} label body =
    T.forkManagedTimeout ctxThreadManager ctxTimeoutManager label $
        endingOnConnectionError . body

forkManagedTimeoutFinally
    :: Context -> String -> (TM.Handle -> IO ()) -> IO () -> IO ()
forkManagedTimeoutFinally Context{..} label body =
    T.forkManagedTimeoutFinally ctxThreadManager ctxTimeoutManager label $
        endingOnConnectionError . body

-- | A thread that has sent a connection error ('connectionError') ends there
-- quietly: the connection is going, and the error has been told to the peer.
--
-- So does one waiting on a connection the peer has closed with no error:
-- quic tells the wait with 'ConnectionIsClosed'.  The thread reading the
-- peer's control stream used to die of it, reported as uncaught.
endingOnConnectionError :: IO () -> IO ()
endingOnConnectionError io =
    io `E.catch` \e -> case e of
        ApplicationProtocolErrorIsSent _ _ -> return ()
        ConnectionIsClosed _ -> return ()
        _ -> E.throwIO e

-- | Closing the connection with this error, and stopping here: see
-- 'connectionError'.
abort :: Context -> ApplicationProtocolError -> IO a
abort ctx = connectionError (ctxConnection ctx)

getHooks :: Context -> Hooks
getHooks = ctxHooks

getMySockAddr :: Context -> SockAddr
getMySockAddr = ctxMySockAddr

getPeerSockAddr :: Context -> SockAddr
getPeerSockAddr = ctxPeerSockAddr

getMaxFieldSectionSize :: Context -> Int
getMaxFieldSectionSize = ctxMaxFieldSectionSize

getPeerMaxFieldSectionSize :: Context -> IO Int
getPeerMaxFieldSectionSize = ctxPeerMaxFieldSectionSize

----------------------------------------------------------------
-- GOAWAY of ours (RFC 9114, section 5.2)

-- | Taking a request stream, unless our GOAWAY has gone out at or below
--   it.  One that is not taken must be rejected, not processed.
admitRequest :: Context -> StreamId -> IO Bool
admitRequest Context{..} sid = atomically $ do
    lim <- readTVar ctxGoAway
    if maybe False (sid >=) lim
        then return False
        else do
            modifyTVar' ctxLastRequest $ Just . maybe sid (max sid)
            modifyTVar' ctxInFlight $ IntSet.insert sid
            return True

-- | Done with a request stream taken by 'admitRequest'.
requestDone :: Context -> StreamId -> IO ()
requestDone Context{..} sid = atomically $ modifyTVar' ctxInFlight $ IntSet.delete sid

-- | Sending GOAWAY once, with the first request stream not taken: the
--   streams taken so far are still answered.
goAway :: Context -> IO ()
goAway ctx@Context{..} = do
    mlim <- atomically $ do
        old <- readTVar ctxGoAway
        case old of
            Just _ -> return Nothing
            Nothing -> do
                lim <- maybe 0 (+ 4) <$> readTVar ctxLastRequest
                writeTVar ctxGoAway $ Just lim
                return $ Just lim
    forM_ mlim $ sendGoAway ctx

-- | The first of two GOAWAYs: the largest identifier, which stops the
--   client sending requests and takes every one already on its way.
goAwayAll :: Context -> IO ()
goAwayAll ctx = sendGoAway ctx $ 2 ^ (62 :: Int) - 4

-- | Waiting for the connection's probe timeout, which is at least a round
--   trip.
waitPTO :: Context -> IO ()
waitPTO Context{..} = do
    Microseconds pto <- getPTO $ connLDCC ctxConnection
    threadDelay pto

sendGoAway :: Context -> StreamId -> IO ()
sendGoAway Context{..} lim = do
    payload <- withWriteBuffer 8 $ \wbuf -> encodeInt' wbuf $ fromIntegral lim
    ctxSendControl [H3Frame H3FrameGoaway payload]

-- | Waiting until our GOAWAY is out and no request taken is left.
drained :: Context -> STM ()
drained Context{..} = do
    readTVar ctxGoAway >>= check . isJust
    readTVar ctxInFlight >>= check . IntSet.null
