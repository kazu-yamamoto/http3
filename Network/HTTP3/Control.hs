{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP3.Control (
    setupUnidirectional
  , controlStream
  ) where

import qualified Data.ByteString as BS
import Data.IORef
import Network.QUIC

import Imports
import qualified Network.HTTP3.Config as H3
import Network.HTTP3.Frame
import Network.HTTP3.Settings
import Network.HTTP3.Stream
import Network.HTTP3.Error
import Network.QPACK

mkType :: H3StreamType -> ByteString
mkType = BS.singleton . fromIntegral . fromH3StreamType

setupUnidirectional :: Connection -> H3.Config -> IO ()
setupUnidirectional conn conf = do
    let st0 = mkType H3ControlStreams
        st1 = mkType QPACKEncoderStream
        st2 = mkType QPACKDecoderStream
    settings <- encodeH3Settings [(QpackBlockedStreams,100),(QpackMaxTableCapacity,4096),(SettingsMaxHeaderListSize,32768)]
    let frames = [H3Frame H3FrameSettings settings]
        frames' = H3.onControlFrameCreated (H3.confHooks conf) frames
    let bss0 = encodeH3Frames frames'
    s0 <- unidirectionalStream conn
    s1 <- unidirectionalStream conn
    s2 <- unidirectionalStream conn
    -- fixme
    sendStreamMany s0 (st0 : bss0)
    sendStream s1 st1
    sendStream s2 st2

controlStream :: Connection -> IORef IFrame -> InstructionHandler
controlStream conn ref recv = loop0
  where
    loop0 = do
        bs <- recv 1024
        when (bs /= "") $ do
            readIORef ref >>= parse0 bs >>= writeIORef ref
            loop
    loop = do
        bs <- recv 1024
        when (bs /= "") $ do
            readIORef ref >>= parse bs >>= writeIORef ref
            loop
    parse0 bs st0 = do
        case parseH3Frame st0 bs of
          IDone typ _payload leftover -> do
              case typ of
                H3FrameSettings   -> return () -- decodeH3Settings payload >>= print
                _                 -> abortConnection conn H3MissingSettings
              parse leftover IInit
          st1 -> return st1

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
