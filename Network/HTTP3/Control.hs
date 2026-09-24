{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Control (
    setupUnidirectional,
    controlStream,
) where

import qualified Data.ByteString as BS
import Data.IORef
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
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
    let QDecoderConfig{..} = confQDecoderConfig
    settings <-
        encodeH3Settings
            [ (SettingsQpackBlockedStreams, dcBlockedSterams)
            , (SettingsQpackMaxTableCapacity, dcMaxTableCapacity)
            , (SettingsMaxFieldSectionSize, dcMaxFieldSectionSize)
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
    :: Connection -> Int -> TableOperation -> IORef IFrame -> InstructionHandler
controlStream conn lim tblop ref recv = loop0
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
        case parseH3Frame lim st0 bs of
            st1'
                -- DATA is the one frame the cap does not cover, and it has no
                -- business on a control stream, so refuse it before any of it
                -- is buffered rather than after.
                | Just H3FrameData <- frameTypeOf st1' -> do
                    abortConnection conn H3FrameUnexpected ""
                    return (False, IInit)
            ITooLong _ _ -> do
                abortConnection conn H3ExcessiveLoad ""
                return (False, IInit)
            IDone typ payload leftover -> do
                case typ of
                    H3FrameSettings -> checkSettings conn tblop payload
                    _ -> abortConnection conn H3MissingSettings ""
                st1 <- parse leftover IInit
                return (True, st1)
            st1 -> return (False, st1)

    parse bs st0 = do
        case parseH3Frame lim st0 bs of
            st1'
                | Just H3FrameData <- frameTypeOf st1' -> do
                    abortConnection conn H3FrameUnexpected ""
                    return IInit
            ITooLong _ _ -> do
                abortConnection conn H3ExcessiveLoad ""
                return IInit
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
    loop IntSet.empty h3settings
  where
    loop :: IntSet -> H3Settings -> IO ()
    loop _ [] = return ()
    loop seen ((k@(H3SettingsKey i), v) : ss)
        -- RFC 9114 section 7.2.4.1 lets a receiver refuse a repeated setting
        -- identifier, and this does.  A set rather than bits in an Int:
        -- identifiers are variable-length integers, so everything from 64 up
        -- fell off the end of the word and could repeat unnoticed.
        | i `IntSet.member` seen = abortConnection conn H3SettingsError ""
        | otherwise = do
            let seen' = IntSet.insert i seen
            case k of
                SettingsQpackMaxTableCapacity -> do
                    setCapacity tblop v
                    loop seen' ss
                -- This value is not used yet.
                -- RFC 9114: "A server that receives a larger field
                -- section than it is willing to handle can send an
                -- HTTP 431 (Request Header Fields Too Large) status
                -- code ([RFC6585])."
                SettingsMaxFieldSectionSize -> do
                    setHeaderSize tblop v
                    loop seen' ss
                SettingsQpackBlockedStreams -> do
                    setBlockedStreams tblop v
                    loop seen' ss
                _
                    -- HTTP/2 settings
                    | i <= 0x6 -> abortConnection conn H3SettingsError ""
                    -- Unknown, so ignored -- but the rest of the frame is
                    -- not.  This used to stop here, which meant a peer
                    -- following section 7.2.4.1's advice to send a reserved
                    -- identifier had every setting after it dropped.
                    | otherwise -> loop seen' ss
