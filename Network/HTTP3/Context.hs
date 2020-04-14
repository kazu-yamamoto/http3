module Network.HTTP3.Context where

import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import Network.QUIC

import Imports
import Network.HTTP3.Stream
import Network.QPACK

data Context = Context {
    ctxConnection :: Connection
  , ctxEncoder   :: Encoder
  , ctxDecoder   :: Decoder
  , ctxSwitch    :: H3StreamType -> Handle
  , ctxMap       :: IORef (IntMap H3StreamType)
  }

newContext :: Connection -> Handle -> Encoder -> HandleInstruction -> Decoder -> HandleInstruction -> IO Context
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
