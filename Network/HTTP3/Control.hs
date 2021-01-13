{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP3.Control (
    setupUnidirectional
  , controlStream
  ) where

import qualified Data.ByteString as BS
import Data.IORef
import Network.QUIC

import Imports
import Network.HTTP3.Frame
import Network.HTTP3.Settings
import Network.HTTP3.Stream
import Network.QPACK
-- import Network.QUIC.Types.Integer

mkType :: H3StreamType -> ByteString
mkType = BS.singleton . fromIntegral . fromH3StreamType

setupUnidirectional :: Connection -> IO ()
setupUnidirectional conn = do
    let st0 = mkType H3ControlStreams
        st1 = mkType QPACKEncoderStream
        st2 = mkType QPACKDecoderStream
    settings <- encodeH3Settings [(QpackBlockedStreams,100),(QpackMaxTableCapacity,4096),(SettingsMaxHeaderListSize,32768)]
    let bss0 = encodeH3Frames [H3Frame H3FrameSettings settings]
    s0 <- unidirectionalStream conn
    s1 <- unidirectionalStream conn
    s2 <- unidirectionalStream conn
    -- fixme
    sendStreamMany s0 (st0 : bss0)
    sendStream s1 st1
    sendStream s2 st2

controlStream :: IORef IFrame -> InstructionHandler
controlStream ref recv = loop
  where
    loop = do
        bs <- recv 1024
        when (bs /= "") $ do
            readIORef ref >>= parse bs >>= writeIORef ref
            loop
    parse bs st0 = do
        case parseH3Frame st0 bs of
          IDone typ _payload leftover -> do
              -- putStrLn $ "control: " ++ show typ
              case typ of
                H3FrameCancelPush -> return () -- print $ decodeInt payload
                H3FrameSettings   -> return () -- decodeH3Settings payload >>= print
                H3FrameGoaway     -> return () -- print $ decodeInt payload
                H3FrameMaxPushId  -> return () -- print $ decodeInt payload
                _                 -> return () -- greasing
              parse leftover IInit
          st1 -> return st1
