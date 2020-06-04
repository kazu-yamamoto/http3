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
  , pReadMaker
  ) where

import Control.Concurrent
import qualified Data.ByteString as BS
import Network.HTTP2.Internal (PositionReadMaker)
import Network.QUIC (Connection, Stream, QUICError)
import qualified Network.QUIC as QUIC
import Network.QUIC.Internal (isServer, isClient)
import qualified System.TimeManager as T

import Imports
import Network.HTTP3.Config
import Network.HTTP3.Stream
import Network.QPACK

data Context = Context {
    ctxConnection :: Connection
  , ctxQEncoder   :: QEncoder
  , ctxQDecoder   :: QDecoder
  , ctxUniSwitch  :: H3StreamType -> InstructionHandler
  , ctxCleanup    :: Cleanup
  , ctxPReadMaker :: PositionReadMaker
  , ctxManager    :: T.Manager
  }

newContext :: Connection -> Config -> InstructionHandler -> IO Context
newContext conn conf ctl = do
    (enc, handleDI, cleanE) <- newQEncoder defaultQEncoderConfig
    (dec, handleEI, cleanD) <- newQDecoder defaultQDecoderConfig
    let sw = switch ctl handleEI handleDI
        clean = cleanE >> cleanD
        preadM = confPositionReadMaker conf
    Context conn enc dec sw clean preadM <$> T.initialize 30000000

clearContext :: Context -> IO ()
clearContext Context{..} = do
    ctxCleanup
    T.stopManager ctxManager

switch :: InstructionHandler -> InstructionHandler -> InstructionHandler -> H3StreamType -> InstructionHandler
switch ctl handleEI handleDI styp
  | styp == H3ControlStreams   = ctl
  | styp == QPACKEncoderStream = handleEI
  | styp == QPACKDecoderStream = handleDI
  | otherwise                  = \_ -> putStrLn "switch unknown stream type"

isH3Server :: Context -> Bool
isH3Server Context{..} = isServer ctxConnection

isH3Client :: Context -> Bool
isH3Client Context{..} = isClient ctxConnection

accept :: Context -> IO (Either QUICError Stream)
accept Context{..} = QUIC.acceptStream ctxConnection

qpackEncode :: Context -> QEncoder
qpackEncode Context{..} = ctxQEncoder

qpackDecode :: Context -> QDecoder
qpackDecode Context{..} = ctxQDecoder

unidirectional :: Context -> Stream -> IO ()
unidirectional Context{..} strm = do
    [w8] <- BS.unpack <$> QUIC.recvStream strm 1 -- fixme: variable length
    let typ = toH3StreamType $ fromIntegral w8
    void $ forkIO $ ctxUniSwitch typ (QUIC.recvStream strm)

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
