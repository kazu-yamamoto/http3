{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

setupUnidirectional
    :: Connection
    -> H3.Config
    -> IO (EncodedEncoderInstruction -> IO (), EncodedDecoderInstruction -> IO ())
setupUnidirectional conn conf@H3.Config{..} = do
    settings <-
        encodeH3Settings
            [ (SettingsQpackBlockedStreams, 100)
            , (SettingsQpackMaxTableCapacity, dcDynamicTableSize confQDecoderConfig)
            , (SettingsMaxFieldSectionSize, 32768)
            ]
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
    return (sendStream sE, sendStream sD)
  where
    stC = mkType H3ControlStreams
    stE = mkType QPACKEncoderStream
    stD = mkType QPACKDecoderStream
    hooks = H3.confHooks conf

-- DynamicTable for Encoder
controlStream
    :: Connection -> TableOperation -> IORef IFrame -> InstructionHandler
controlStream conn tblop ref recv = loop0
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
                    H3FrameSettings -> checkSettings conn tblop payload
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

checkSettings :: Connection -> TableOperation -> ByteString -> IO ()
checkSettings conn tblop payload = do
    h3settings <- decodeH3Settings payload
    loop (0 :: Int) h3settings
  where
    loop _ [] = return ()
    loop flags ((k@(H3SettingsKey i), v) : ss)
        | flags `testBit` i = abortConnection conn H3SettingsError ""
        | otherwise = do
            let flags' = flags `setBit` i
            case k of
                SettingsQpackMaxTableCapacity -> do
                    setCapacity tblop v
                    loop flags' ss
                -- FIXME
                SettingsMaxFieldSectionSize -> loop flags' ss
                SettingsQpackBlockedStreams -> do
                    setBlockedStreams tblop v
                    loop flags' ss
                _
                    -- HTTP/2 settings
                    | i <= 0x6 -> abortConnection conn H3SettingsError ""
                    | otherwise -> return ()
