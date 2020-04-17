{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Context (
    Context
  , newContext
  , lookupUniMap
  , registerUniMap
  , switchUnidirectional
  , isH3Server
  , isH3Client
  , send
  , recv
  , qpackEncode
  , qpackDecode
  ) where

import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Network.QUIC
import Network.QUIC.Connection

import Imports
import Network.HTTP3.Stream
import Network.QPACK

data Context = Context {
    ctxConnection :: Connection
  , ctxQEncoder   :: QEncoder
  , ctxQDecoder   :: QDecoder
  , ctxUniSwitch  :: H3StreamType -> Handle
  , ctxUniMap     :: IORef (IntMap H3StreamType)
  }

newContext :: Connection -> Handle -> QEncoder -> HandleInstruction -> QDecoder -> HandleInstruction -> IO Context
newContext conn ctl enc handleDI dec handleEI = do
    let sw = switch ctl handleEI handleDI
    Context conn enc dec sw <$> newIORef I.empty

type Handle = ByteString -> IO ()

switch :: Handle -> Handle -> Handle -> H3StreamType -> Handle
switch ctl handleEI handleDI styp
  | styp == H3ControlStreams   = ctl
  | styp == QPACKEncoderStream = handleEI
  | styp == QPACKDecoderStream = handleDI
  | otherwise                  = \_ -> putStrLn "switch unknown stream type"

lookupUniMap :: Context -> StreamId -> IO (Maybe H3StreamType)
lookupUniMap Context{..} sid = I.lookup sid <$> readIORef ctxUniMap

registerUniMap :: Context -> StreamId -> H3StreamType -> IO ()
registerUniMap Context{..} sid typ = do
    m <- I.insert sid typ <$> readIORef ctxUniMap
    writeIORef ctxUniMap m

switchUnidirectional :: Context -> H3StreamType -> ByteString -> IO ()
switchUnidirectional Context{..} typ bs = ctxUniSwitch typ bs

isH3Server :: Context -> Bool
isH3Server Context{..} = isServer ctxConnection

isH3Client :: Context -> Bool
isH3Client Context{..} = isClient ctxConnection

send :: Context -> StreamId -> ByteString -> Fin -> IO ()
send Context{..} = sendStream ctxConnection

recv :: Context -> IO (StreamId, ByteString, Fin)
recv Context{..} = recvStream ctxConnection

qpackEncode :: Context -> QEncoder
qpackEncode Context{..} = ctxQEncoder

qpackDecode :: Context -> QDecoder
qpackDecode Context{..} = ctxQDecoder
