{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP3.Control (
    setupUnidirectional,
    controlStream,
) where

import qualified Data.ByteString as BS
import Data.IORef
import Network.QUIC

import Imports
import qualified Network.HTTP3.Config as H3
import Network.HTTP3.Error
import Network.HTTP3.Frame
import Network.HTTP3.Settings
import Network.HTTP3.Stream
import Network.QPACK

mkType :: H3StreamType -> ByteString
mkType = BS.singleton . fromIntegral . fromH3StreamType

setupUnidirectional :: Connection -> H3.Config -> IO ()
setupUnidirectional conn conf = do
    settings <-
        encodeH3Settings
            [ (SettingsQpackBlockedStreams, 100)
            , (SettingsQpackMaxTableCapacity, 4096)
            , (SettingsMaxFieldSectionSize, 32768)
            ] -- fixme
    let framesC = H3.onControlFrameCreated hooks [H3Frame H3FrameSettings settings]
    let bssC = encodeH3Frames framesC
    sC <- unidirectionalStream conn
    sE <- unidirectionalStream conn
    sD <- unidirectionalStream conn
    -- fixme
    sendStreamMany sC (stC : bssC)
    sendStream sE stE
    sendStream sD stD
    H3.onControlStreamCreated hooks sC
    H3.onEncoderStreamCreated hooks sE
    H3.onDecoderStreamCreated hooks sD
  where
    stC = mkType H3ControlStreams
    stE = mkType QPACKEncoderStream
    stD = mkType QPACKDecoderStream
    hooks = H3.confHooks conf

controlStream :: Connection -> IORef IFrame -> InstructionHandler
controlStream conn ref recv = loop0
  where
    loop0 = do
        bs <- recv 1024
        if bs == ""
            then abortConnection conn H3ClosedCriticalStream ""
            else do
                (done, st1) <- readIORef ref >>= parse0 bs
                writeIORef ref st1
                if done then loop else loop0
    loop = do
        bs <- recv 1024
        if bs == ""
            then abortConnection conn H3ClosedCriticalStream ""
            else do
                readIORef ref >>= parse bs >>= writeIORef ref
                loop
    parse0 bs st0 = do
        case parseH3Frame st0 bs of
            IDone typ payload leftover -> do
                case typ of
                    H3FrameSettings -> checkSettings conn payload
                    _ -> abortConnection conn H3MissingSettings ""
                st1 <- parse leftover IInit
                return (True, st1)
            st1 -> return (False, st1)

    parse bs st0 = do
        case parseH3Frame st0 bs of
            IDone typ _payload leftover -> do
                case typ of
                    H3FrameCancelPush -> return ()
                    H3FrameSettings -> abortConnection conn H3FrameUnexpected ""
                    H3FrameGoaway -> return ()
                    H3FrameMaxPushId -> return ()
                    _
                        | permittedInControlStream typ -> return ()
                        | otherwise -> abortConnection conn H3FrameUnexpected ""
                parse leftover IInit
            st1 -> return st1

checkSettings :: Connection -> ByteString -> IO ()
checkSettings conn payload = do
    h3settings <- decodeH3Settings payload
    loop (0 :: Int) h3settings
  where
    loop _ [] = return ()
    loop flags ((k@(H3SettingsKey i), _v) : ss)
        | flags `testBit` i = abortConnection conn H3SettingsError ""
        | otherwise = do
            let flags' = flags `setBit` i
            case k of
                SettingsQpackMaxTableCapacity -> loop flags' ss
                SettingsMaxFieldSectionSize -> loop flags' ss
                SettingsQpackBlockedStreams -> loop flags' ss
                _
                    -- HTTP/2 settings
                    | i <= 0x6 -> abortConnection conn H3SettingsError ""
                    | otherwise -> return ()
