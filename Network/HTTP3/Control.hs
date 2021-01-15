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
        if bs == "" then
            abortConnection conn H3ClosedCriticalStream
          else do
            (done, st1) <- readIORef ref >>= parse0 bs
            writeIORef ref st1
            if done then loop0 else loop
    loop = do
        bs <- recv 1024
        if bs == "" then
            abortConnection conn H3ClosedCriticalStream
          else do
            readIORef ref >>= parse bs >>= writeIORef ref
            loop
    parse0 bs st0 = do
        case parseH3Frame st0 bs of
          IDone typ _payload leftover -> do
              case typ of
                H3FrameSettings   -> return ()
                _                 -> abortConnection conn H3MissingSettings
              st1 <- parse leftover IInit
              return (True, st1)
          st1 -> return (False, st1)

    parse bs st0 = do
        case parseH3Frame st0 bs of
          IDone typ _payload leftover -> do
              case typ of
                H3FrameCancelPush -> return ()
                H3FrameSettings   -> abortConnection conn H3FrameUnexpected
                H3FrameGoaway     -> return ()
                H3FrameMaxPushId  -> return ()
                _ | permittedInControlStream typ -> return ()
                  | otherwise                    -> abortConnection conn H3FrameUnexpected
              parse leftover IInit
          st1 -> return st1
