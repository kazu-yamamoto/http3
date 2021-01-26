{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Context (
    Context
  , newContext
  , clearContext
  , unidirectional
  , isH3Server
  , isH3Client
  , accept
  , qpackEncode
  , qpackDecode
  , registerThread
  , timeoutClose
  , newStream
  , QUIC.closeStream
  , pReadMaker
  , addThreadId
  , abort
  , getHooks
  , Hooks(..) -- re-export
  ) where

import qualified Control.Exception as E
import Control.Concurrent
import qualified Data.ByteString as BS
import Data.IORef
import Network.HTTP2.Internal (PositionReadMaker)
import Network.QUIC (Connection, Stream)
import qualified Network.QUIC as QUIC
import Network.QUIC.Internal (isServer, isClient)
import System.Mem.Weak
import qualified System.TimeManager as T

import Network.HTTP3.Config
import Network.HTTP3.Stream
import Network.QPACK
import Network.QPACK.Internal

data Context = Context {
    ctxConnection :: Connection
  , ctxQEncoder   :: QEncoder
  , ctxQDecoder   :: QDecoder
  , ctxUniSwitch  :: H3StreamType -> InstructionHandler
  , ctxPReadMaker :: PositionReadMaker
  , ctxManager    :: T.Manager
  , ctxHooks      :: Hooks
  , ctxThreads    :: IORef [Weak ThreadId]
  }

newContext :: Connection -> Config -> InstructionHandler -> IO Context
newContext conn conf ctl = do
    (enc, handleDI) <- newQEncoder defaultQEncoderConfig
    (dec, handleEI) <- newQDecoder defaultQDecoderConfig
    let handleDI' recv = handleDI recv `E.catch` abortWith QpackDecoderStreamError
        handleEI' recv = handleEI recv `E.catch` abortWith QpackEncoderStreamError
        sw = switch conn ctl handleEI' handleDI'
        preadM = confPositionReadMaker conf
        timmgr = confTimeoutManager conf
        hooks  = confHooks conf
    Context conn enc dec sw preadM timmgr hooks <$> newIORef []
  where
    abortWith aerr se
      | Just E.ThreadKilled <- E.fromException se = return ()
      | otherwise = QUIC.abortConnection conn aerr

clearContext :: Context -> IO ()
clearContext ctx = clearThreads ctx

switch :: Connection -> InstructionHandler -> InstructionHandler -> InstructionHandler -> H3StreamType -> InstructionHandler
switch conn ctl handleEI handleDI styp
  | styp == H3ControlStreams   = ctl
  | styp == QPACKEncoderStream = handleEI
  | styp == QPACKDecoderStream = handleDI
  | otherwise                  = \_ -> QUIC.connDebugLog conn "switch unknown stream type"

isH3Server :: Context -> Bool
isH3Server Context{..} = isServer ctxConnection

isH3Client :: Context -> Bool
isH3Client Context{..} = isClient ctxConnection

accept :: Context -> IO Stream
accept Context{..} = QUIC.acceptStream ctxConnection

qpackEncode :: Context -> QEncoder
qpackEncode Context{..} = ctxQEncoder

qpackDecode :: Context -> QDecoder
qpackDecode Context{..} = ctxQDecoder

unidirectional :: Context -> Stream -> IO ThreadId
unidirectional Context{..} strm = do
    w8:_ <- BS.unpack <$> QUIC.recvStream strm 1 -- fixme: variable length
    let typ = toH3StreamType $ fromIntegral w8
    forkIO $ ctxUniSwitch typ (QUIC.recvStream strm)

registerThread :: Context -> IO T.Handle
registerThread Context{..} = T.registerKillThread ctxManager (return ())

timeoutClose :: Context -> IO () -> IO (IO ())
timeoutClose Context{..} closer = do
    th <- T.register ctxManager closer
    return $ T.tickle th

newStream :: Context -> IO Stream
newStream Context{..} = QUIC.stream ctxConnection

pReadMaker :: Context -> PositionReadMaker
pReadMaker = ctxPReadMaker

addThreadId :: Context -> ThreadId -> IO ()
addThreadId Context{..} tid = do
    wtid <- mkWeakThreadId tid
    atomicModifyIORef' ctxThreads $ \ts -> (wtid:ts, ())

clearThreads :: Context -> IO ()
clearThreads Context{..} = do
    wtids <- readIORef ctxThreads
    mapM_ kill wtids
    writeIORef ctxThreads []
  where
    kill wtid = do
        mtid <- deRefWeak wtid
        case mtid of
          Nothing  -> return ()
          Just tid -> killThread tid

abort :: Context -> QUIC.ApplicationProtocolError -> IO ()
abort ctx = QUIC.abortConnection $ ctxConnection ctx

getHooks :: Context -> Hooks
getHooks = ctxHooks
