{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Control (
    setupUnidirectional,
    controlStream,
    ControlState,
    newControlState,
    connectionError,
) where

import Control.Concurrent.MVar
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IORef
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Network.QUIC
import Network.QUIC.Internal (isServer)

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
    -> IO
        ( EncodedEncoderInstruction -> IO ()
        , EncodedDecoderInstruction -> IO ()
        , [H3Frame] -> IO ()
        )
setupUnidirectional conn conf@H3.Config{..} = do
    let dc = confQDecoderConfig
    settings <-
        encodeH3Settings
            [ (SettingsQpackBlockedStreams, dcBlockedSterams dc)
            , (SettingsQpackMaxTableCapacity, dcMaxTableCapacity dc)
            , (SettingsMaxFieldSectionSize, dcMaxFieldSectionSize dc)
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
    -- Every request stream writes acknowledgements and cancellations to the
    -- decoder stream, and an instruction must not be split by another one.
    -- quic sends a write in two pieces when flow control stops it partway.
    lockD <- newMVar ()
    return
        ( critical $ sendStream sE
        , critical $ \bs -> withMVar lockD $ \_ -> sendStream sD bs
        , critical $ sendStreamMany sC . encodeH3Frames
        )
  where
    -- The sending part of one of our QPACK streams is closed only when the
    -- peer asked for that with STOP_SENDING, which it must not do (RFC
    -- 9204, section 4.2; RFC 9114, section 6.2.1): the closure of a
    -- critical stream is a connection error of type H3_CLOSED_CRITICAL_STREAM.
    -- The write used to throw StreamIsClosed into whichever thread made it,
    -- and nothing caught it there.
    critical send bs =
        send bs `E.catch` \e -> case e of
            StreamIsClosed -> connectionError conn H3ClosedCriticalStream
            _ -> E.throwIO e
    stC = mkType H3ControlStreams
    stE = mkType QPACKEncoderStream
    stD = mkType QPACKDecoderStream
    hooks = H3.confHooks conf

-- | Closing the connection with this error, and stopping here.
--
-- 'abortConnection' only throws to the connection's main thread, so code
-- after it went on running in this one: a frame refused was then handled.
connectionError :: Connection -> ApplicationProtocolError -> IO a
connectionError conn aerr = do
    abortConnection conn aerr ""
    E.throwIO $ ApplicationProtocolErrorIsSent aerr ""

-- | The last GOAWAY and MAX_PUSH_ID the peer sent on its control stream,
--   and what to do with a GOAWAY.
data ControlState = ControlState
    { lastGoaway :: IORef (Maybe Int64)
    , lastMaxPushId :: IORef (Maybe Int64)
    , onGoaway :: Int64 -> IO ()
    }

newControlState :: (Int64 -> IO ()) -> IO ControlState
newControlState hook = ControlState <$> newIORef Nothing <*> newIORef Nothing <*> pure hook

-- DynamicTable for Encoder
controlStream
    :: Connection
    -> Int
    -> TableOperation
    -> ControlState
    -> IORef IFrame
    -> InstructionHandler
controlStream conn lim tblop cst ref recv = loop0
  where
    loop0 = do
        bs <- recv 1024
        if bs == ""
            then connectionError conn H3ClosedCriticalStream
            else do
                (done, st1) <- readIORef ref >>= parse0 bs
                writeIORef ref st1
                if done then loop else loop0
    loop = do
        bs <- recv 1024
        if bs == ""
            then connectionError conn H3ClosedCriticalStream
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
                    connectionError conn H3FrameUnexpected
            ITooLong _ _ -> do
                connectionError conn H3ExcessiveLoad
            IDone typ payload leftover -> do
                case typ of
                    H3FrameSettings -> checkSettings conn tblop payload
                    _ -> connectionError conn H3MissingSettings
                st1 <- parse leftover IInit
                return (True, st1)
            st1 -> return (False, st1)

    parse bs st0 = do
        case parseH3Frame lim st0 bs of
            st1'
                | Just H3FrameData <- frameTypeOf st1' -> do
                    connectionError conn H3FrameUnexpected
            ITooLong _ _ -> do
                connectionError conn H3ExcessiveLoad
            IDone typ payload leftover -> do
                case typ of
                    H3FrameCancelPush -> singleInt payload >>= cancelPush
                    H3FrameSettings -> connectionError conn H3FrameUnexpected
                    H3FrameGoaway -> singleInt payload >>= goaway
                    H3FrameMaxPushId -> singleInt payload >>= maxPushId
                    _
                        | permittedInControlStream typ -> return ()
                        | otherwise -> connectionError conn H3FrameUnexpected
                parse leftover IInit
            st1 -> return st1

    -- RFC 9114, section 7.1: the payload is exactly the fields the frame
    -- has, here one variable-length integer, or H3_FRAME_ERROR.
    singleInt payload = case parseQInt QInit payload of
        QDone i "" -> return i
        _ -> connectionError conn H3FrameError
    server = isServer conn
    -- Section 7.2.3: a push ID beyond what is allowed is H3_ID_ERROR.  A
    -- client allows none, since ours sends no MAX_PUSH_ID; a server, those
    -- up to the client's MAX_PUSH_ID.
    cancelPush pid = do
        allowed <-
            if server
                then readIORef (lastMaxPushId cst)
                else return Nothing
        unless (maybe False (pid <=) allowed) $ connectionError conn H3IdError
    -- Section 5.2: from a server, the ID of a client-initiated bidirectional
    -- stream; and in either direction, no larger than one sent before.
    goaway i = do
        when (not server && i `mod` 4 /= 0) $ connectionError conn H3IdError
        prev <- readIORef (lastGoaway cst)
        when (maybe False (i >) prev) $ connectionError conn H3IdError
        writeIORef (lastGoaway cst) (Just i)
        onGoaway cst i
    -- Section 7.2.7: only a server takes MAX_PUSH_ID, and it may not go down.
    maxPushId i = do
        unless server $ connectionError conn H3FrameUnexpected
        prev <- readIORef (lastMaxPushId cst)
        when (maybe False (i <) prev) $ connectionError conn H3IdError
        writeIORef (lastMaxPushId cst) (Just i)

checkSettings :: Connection -> TableOperation -> ByteString -> IO ()
checkSettings conn tblop payload = do
    mh3settings <- decodeH3Settings payload
    case mh3settings of
        -- RFC 9114 section 7.1: a payload that "terminates before the end of
        -- the identified fields MUST be treated as a connection error of type
        -- H3_FRAME_ERROR".
        Nothing -> connectionError conn H3FrameError
        Just h3settings -> loop IntSet.empty h3settings
  where
    loop :: IntSet -> H3Settings -> IO ()
    loop _ [] = return ()
    loop seen ((k@(H3SettingsKey i), v) : ss)
        -- RFC 9114 section 7.2.4.1 lets a receiver refuse a repeated setting
        -- identifier, and this does.  A set rather than bits in an Int:
        -- identifiers are variable-length integers, so everything from 64 up
        -- fell off the end of the word and could repeat unnoticed.
        | i `IntSet.member` seen = connectionError conn H3SettingsError
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
                    | i <= 0x6 -> connectionError conn H3SettingsError
                    -- Unknown, so ignored -- but the rest of the frame is
                    -- not.  This used to stop here, which meant a peer
                    -- following section 7.2.4.1's advice to send a reserved
                    -- identifier had every setting after it dropped.
                    | otherwise -> loop seen' ss
