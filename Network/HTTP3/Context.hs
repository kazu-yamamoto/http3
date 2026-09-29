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
    forkManaged,
    forkManagedTimeout,
    forkManagedTimeoutFinally,
    isAsyncException,
) where

import qualified Control.Exception as E
import Control.Monad (void)
import Data.IORef
import Network.HTTP.Semantics.Client
import Network.QUIC
import Network.QUIC.Internal (connDebugLog, isClient, isServer)
import Network.Socket (SockAddr)
import qualified System.ThreadManager as T

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
    , ctxHooks :: Hooks
    , ctxMySockAddr :: SockAddr
    , ctxPeerSockAddr :: SockAddr
    , ctxMaxFieldSectionSize :: Int
    -- ^ What we told the peer we would accept, and so the most of any one
    -- frame we are willing to hold in memory while it arrives.
    , ctxCancelStream :: StreamId -> IO ()
    -- ^ Sending a Stream Cancellation on our QPACK decoder stream
    }

withContext :: Connection -> Config -> (Context -> IO a) -> IO a
withContext conn conf action = do
    ctx <- newContext conn conf
    T.stopAfter (ctxThreadManager ctx) (action ctx) (\_ -> return ())

newContext :: Connection -> Config -> IO Context
newContext conn conf = do
    (sendEI, sendDI) <- setupUnidirectional conn conf
    (ctxQEncoder, handleDI, dyntblE) <- newQEncoder (confQEncoderConfig conf) sendEI
    -- newQDecoder passes dyntbl for decoder to handleEI internally
    (ctxQDecoder, handleEI) <- newQDecoder (confQDecoderConfig conf) sendDI
    let ctxMaxFieldSectionSize = dcMaxFieldSectionSize $ confQDecoderConfig conf
    ctl <- controlStream conn ctxMaxFieldSectionSize dyntblE <$> newIORef IInit
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
    ctxThreadManager <- T.newThreadManager $ confTimeoutManager conf
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
        | otherwise = abortConnection conn aerr ""

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
        | isServer conn -> \_ -> abortConnection conn H3StreamCreationError ""
        | otherwise -> \_ -> abortConnection conn H3IdError ""
    _ -> \_ -> connDebugLog conn "switch unknown stream type"
  where
    once, closing :: InstructionHandler -> InstructionHandler
    once handler recv = do
        dup <- atomicModifyIORef' seen $ \ts -> (styp : ts, styp `elem` ts)
        if dup
            then abortConnection conn H3StreamCreationError ""
            else handler recv
    closing handler recv = do
        handler recv
        abortConnection conn H3ClosedCriticalStream ""

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
        Nothing -> return ()
        Just i -> ctxUniSwitch (toH3StreamType i) (recvStream strm)

withHandle :: Context -> (T.Handle -> IO ()) -> IO ()
withHandle Context{..} action = void $ T.withHandle ctxThreadManager (return ()) action

newStream :: Context -> IO Stream
newStream Context{..} = stream ctxConnection

pReadMaker :: Context -> PositionReadMaker
pReadMaker = ctxPReadMaker

forkManaged :: Context -> String -> IO () -> IO ()
forkManaged Context{..} = T.forkManaged ctxThreadManager

forkManagedTimeout :: Context -> String -> (T.Handle -> IO ()) -> IO ()
forkManagedTimeout Context{..} =
    T.forkManagedTimeout ctxThreadManager

forkManagedTimeoutFinally
    :: Context -> String -> (T.Handle -> IO ()) -> IO () -> IO ()
forkManagedTimeoutFinally Context{..} =
    T.forkManagedTimeoutFinally ctxThreadManager

abort :: Context -> ApplicationProtocolError -> IO ()
abort ctx aerr = abortConnection (ctxConnection ctx) aerr ""

getHooks :: Context -> Hooks
getHooks = ctxHooks

getMySockAddr :: Context -> SockAddr
getMySockAddr = ctxMySockAddr

getPeerSockAddr :: Context -> SockAddr
getPeerSockAddr = ctxPeerSockAddr

getMaxFieldSectionSize :: Context -> Int
getMaxFieldSectionSize = ctxMaxFieldSectionSize
