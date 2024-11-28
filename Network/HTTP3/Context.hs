{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Context (
    Context,
    withContext,
    unidirectional,
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
    forkManaged,
    forkManagedTimeout,
    forkManagedTimeoutFinally,
    isAsyncException,
) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IORef
import Network.HTTP.Semantics.Client
import Network.QUIC
import Network.QUIC.Internal (connDebugLog, isClient, isServer)
import Network.Socket (SockAddr)
import qualified System.ThreadManager as T

import Network.HTTP3.Config
import Network.HTTP3.Control
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
    }

withContext :: Connection -> Config -> (Context -> IO a) -> IO a
withContext conn conf action = do
    ctx <- newContext conn conf
    T.stopAfter (ctxThreadManager ctx) (action ctx) (\_ -> return ())

newContext :: Connection -> Config -> IO Context
newContext conn conf = do
    ctl <- controlStream conn <$> newIORef IInit
    (enc, handleDI) <- newQEncoder defaultQEncoderConfig
    (dec, handleEI) <- newQDecoder defaultQDecoderConfig
    info <- getConnectionInfo conn
    let handleDI' recv = handleDI recv `E.catch` abortWith QpackDecoderStreamError
        handleEI' recv = handleEI recv `E.catch` abortWith QpackEncoderStreamError
        sw = switch conn ctl handleEI' handleDI'
        preadM = confPositionReadMaker conf
        hooks = confHooks conf
        mysa = localSockAddr info
        peersa = remoteSockAddr info
    mgr <- T.newThreadManager $ confTimeoutManager conf
    return $
        Context
            { ctxConnection = conn
            , ctxQEncoder = enc
            , ctxQDecoder = dec
            , ctxUniSwitch = sw
            , ctxPReadMaker = preadM
            , ctxThreadManager = mgr
            , ctxHooks = hooks
            , ctxMySockAddr = mysa
            , ctxPeerSockAddr = peersa
            }
  where
    abortWith :: ApplicationProtocolError -> E.SomeException -> IO ()
    abortWith aerr se
        | isAsyncException se = E.throwIO se
        | otherwise = abortConnection conn aerr ""

isAsyncException :: E.Exception e => e -> Bool
isAsyncException e =
    case E.fromException (E.toException e) of
        Just (E.SomeAsyncException _) -> True
        Nothing -> False

switch
    :: Connection
    -> InstructionHandler
    -> InstructionHandler
    -> InstructionHandler
    -> H3StreamType
    -> InstructionHandler
switch conn ctl handleEI handleDI styp
    | styp == H3ControlStreams = ctl
    | styp == QPACKEncoderStream = handleEI
    | styp == QPACKDecoderStream = handleDI
    | otherwise = \_ -> connDebugLog conn "switch unknown stream type"

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

unidirectional :: Context -> Stream -> IO ()
unidirectional Context{..} strm = do
    w8 : _ <- BS.unpack <$> recvStream strm 1 -- fixme: variable length
    let typ = toH3StreamType $ fromIntegral w8
    ctxUniSwitch typ (recvStream strm)

withHandle :: Context -> (T.Handle -> IO a) -> IO a
withHandle Context{..} = T.withHandle ctxThreadManager (return ())

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
getPeerSockAddr = ctxMySockAddr
