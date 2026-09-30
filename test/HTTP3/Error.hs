{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HTTP3.Error (
    h3ErrorSpec,
) where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (forM_)
import Data.ByteString ()
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Network.HTTP.Types
import qualified Network.HTTP3.Client as H3
import Network.HTTP3.Internal
import Network.QPACK (QDecoderConfig (..), QEncoderConfig (..))
import Network.QPACK.Internal
import Network.QUIC
import Network.QUIC.Client
import Network.QUIC.Internal hiding (timeout)
import System.Timeout
import Test.Hspec

----------------------------------------------------------------

type Millisecond = Int

runC
    :: ClientConfig -> H3.ClientConfig -> H3.Config -> Millisecond -> IO (Maybe ())
runC qcc cconf conf ms = timeout us $ run qcc $ \conn -> do
    info <- getConnectionInfo conn
    case alpn info of
        Just proto | "hq" `BS.isPrefixOf` proto -> do
            waitEstablished conn
            putStrLn $
                "Warning: "
                    ++ C8.unpack proto
                    ++ " is negotiated. Skipping this test. Use \"h3spec -s HTTP/3\" next time."
            E.throwIO $ ApplicationProtocolErrorIsReceived H3InternalError ""
        _ -> H3.run conn cconf conf client
  where
    us = ms * 1000
    client :: H3.Client ()
    client sendRequest _aux = do
        let req = H3.requestNoBody methodGet "/" []
        ret <- sendRequest req $ \_rsp -> return ()
        threadDelay 100000
        return ret

-- | Like 'runC', but sending a request of our own choosing.
runCReq
    :: H3.Request
    -> ClientConfig
    -> H3.ClientConfig
    -> H3.Config
    -> Millisecond
    -> IO (Maybe ())
runCReq req qcc cconf conf ms = timeout us $ run qcc $ \conn -> do
    info <- getConnectionInfo conn
    case alpn info of
        Just proto | "hq" `BS.isPrefixOf` proto -> do
            waitEstablished conn
            E.throwIO $ ApplicationProtocolErrorIsReceived H3InternalError ""
        _ -> H3.run conn cconf conf client
  where
    us = ms * 1000
    client sendRequest _aux = do
        ret <- sendRequest req $ \_rsp -> return ()
        threadDelay 100000
        return ret

-- | The error cases for an HTTP/3 server.
--
-- These also make h3spec, which runs them against a server somewhere else.
-- The last argument says how, from what the spec is run with, to read how
-- many requests the server has handed its application.  Only a server in the
-- same process can say; given 'Nothing', a test does not check that a
-- request the server refused never got that far.
h3ErrorSpec
    :: ClientConfig
    -> H3.ClientConfig
    -> Millisecond
    -> (a -> Maybe (IO Int))
    -> SpecWith a
h3ErrorSpec qcc cconf ms served = do
    conf0 <- runIO H3.allocSimpleConfig
    describe "HTTP/3 servers" $ do
        it
            "MUST send H3_FRAME_UNEXPECTED if DATA is received before HEADERS [HTTP/3 4.1]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnHeadersFrameCreated requestIllegalData
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it "MUST send H3_MESSAGE_ERROR if a pseudo-header is duplicated [HTTP/3 4.1.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader3
                qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
            runC qcc' cconf conf ms
                `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it
            "MUST send H3_MESSAGE_ERROR if mandatory pseudo-header fields are absent [HTTP/3 4.1.3]"
            $ \x -> do
                let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader0
                    qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
                refusedNotServed (served x) $
                    runC qcc' cconf conf ms
                        `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it
            "MUST send H3_MESSAGE_ERROR if prohibited pseudo-header fields are present[HTTP/3 4.1.3]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader1
                    qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
                runC qcc' cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it
            "MUST send H3_MESSAGE_ERROR if pseudo-header fields exist after fields [HTTP/3 4.1.3]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader2
                    qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
                runC qcc' cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it
            "MUST treat content that does not match content-length as malformed [HTTP/3 4.1.2]"
            $ \_ -> do
                -- content-length says five octets and the request carries
                -- none.  /drain reads the body, which is where the count is
                -- checked; a body nobody reads is never counted.
                let req = H3.requestNoBody methodPost "/drain" [("content-length", "5")]
                    qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
                runCReq req qcc' cconf conf0 ms
                    `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        forM_ connectionSpecific $ \field@(name, _) ->
            it
                ( "MUST treat a request with "
                    ++ C8.unpack (CI.original name)
                    ++ " as malformed [HTTP/3 4.2]"
                )
                $ \x -> do
                    let req = H3.requestNoBody methodGet "/" [field]
                        qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
                    refusedNotServed (served x) $
                        runCReq req qcc' cconf conf0 ms
                            `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it
            "MUST treat a request whose trailers carry connection as malformed [HTTP/3 4.2]"
            $ \_ -> do
                -- /drain reads the body, and so the trailers after it.
                let req0 = H3.requestBuilder methodPost "/drain" [] "hello"
                    req = H3.setRequestTrailersMaker req0 connectionTrailer
                    qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
                runCReq req qcc' cconf conf0 ms
                    `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it "MUST accept TE with trailers [HTTP/3 4.2]" $ \_ -> do
            let req = H3.requestNoBody methodGet "/" [("te", "trailers")]
            runCReq req qcc cconf conf0 ms `shouldReturn` Just ()
        it
            "MUST send H3_MISSING_SETTINGS if the first control frame is not SETTINGS [HTTP/3 6.2.1]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnControlFrameCreated startWithNonSettings
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3MissingSettings]
        it
            "MUST send H3_FRAME_UNEXPECTED if a DATA frame is received on a control stream [HTTP/3 7.2.1]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnControlFrameCreated controlData
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it
            "MUST send H3_FRAME_UNEXPECTED if a HEADERS frame is received on a control stream [HTTP/3 7.2.2]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnControlFrameCreated controlHeaders
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it
            "MUST send H3_FRAME_UNEXPECTED if a second SETTINGS frame is received [HTTP/3 7.2.4]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnControlFrameCreated doubleSettings
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        {- this is MAY
                it "MUST send H3_SETTINGS_ERROR if duplicate setting identifiers exist [HTTP/3 7.2.4]" $ \_ -> do
                    let conf = addHook conf0 $ setOnControlFrameCreated illegalSettings0
                    runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3SettingsError]
        -}
        it
            "MUST send H3_SETTINGS_ERROR if HTTP/2 settings are included [HTTP/3 7.2.4.1]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnControlFrameCreated illegalSettings1
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3SettingsError]
        it
            "MUST send H3_FRAME_ERROR if a SETTINGS frame stops mid-parameter [HTTP/3 7.1]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnControlFrameCreated truncatedSettings
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3FrameError]
        it
            "MUST NOT stop reading settings at a reserved identifier [HTTP/3 7.2.4.1]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnControlFrameCreated greaseThenHttp2Setting
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3SettingsError]
        it
            "treats a repeated setting identifier as an error whatever its value [HTTP/3 7.2.4.1]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnControlFrameCreated duplicateLargeSetting
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3SettingsError]
        it
            "MUST send H3_FRAME_UNEXPECTED if CANCEL_PUSH is received in a request stream [HTTP/3 7.2.5]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnHeadersFrameCreated requestCancelPush
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it
            "MUST send QPACK_DECOMPRESSION_FAILED if an invalid static table index exits in a field line representation [QPACK 3.1]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader4
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [QpackDecompressionFailed]
        it
            "MUST send QPACK_DECOMPRESSION_FAILED if a field line references a dynamic table entry that is not there [QPACK 2.1.2]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader5
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [QpackDecompressionFailed]
        it
            "MUST NOT buffer a frame longer than SETTINGS_MAX_FIELD_SECTION_SIZE [HTTP/3 7.1]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnControlStreamCreated overLongFrame
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3ExcessiveLoad]
        it
            "MUST send QPACK_ENCODER_STREAM_ERROR if a new dynamic table capacity value exceeds the limit [QPACK 4.1.3]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnEncoderStreamCreated largeTableCapacity
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [QpackEncoderStreamError]
        it
            "MUST send H3_CLOSED_CRITICAL_STREAM if a control stream is closed [QPACK 4.2]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnControlStreamCreated closeStream
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3ClosedCriticalStream]
        it
            "MUST send H3_STREAM_CREATION_ERROR if a second control stream is opened [HTTP/3 6.2.1]"
            $ \_ -> do
                -- A stream type of 0x00, then an empty SETTINGS frame.
                opened <- newIORef False
                let conf = addHook conf0 $ setOnControlStreamCreated $ openAnother opened "\x00\x04\x00"
                expectIfOpened opened (runC qcc cconf conf ms) [H3StreamCreationError]
        it
            "MUST send H3_STREAM_CREATION_ERROR if a client opens a push stream [HTTP/3 6.2.2]"
            $ \_ -> do
                -- A stream type of 0x01, then push ID 0.
                opened <- newIORef False
                let conf = addHook conf0 $ setOnControlStreamCreated $ openAnother opened "\x01\x00"
                expectIfOpened opened (runC qcc cconf conf ms) [H3StreamCreationError]
        it
            "MUST send H3_STREAM_CREATION_ERROR if a second encoder stream is opened [QPACK 4.2]"
            $ \_ -> do
                opened <- newIORef False
                let conf = addHook conf0 $ setOnEncoderStreamCreated $ openAnother opened "\x02"
                expectIfOpened opened (runC qcc cconf conf ms) [H3StreamCreationError]
        it
            "MUST send H3_STREAM_CREATION_ERROR if a second decoder stream is opened [QPACK 4.2]"
            $ \_ -> do
                opened <- newIORef False
                let conf = addHook conf0 $ setOnDecoderStreamCreated $ openAnother opened "\x03"
                expectIfOpened opened (runC qcc cconf conf ms) [H3StreamCreationError]
        it
            "MUST send H3_CLOSED_CRITICAL_STREAM if an encoder stream is closed [QPACK 4.2]"
            $ \_ -> do
                let conf =
                        noEncoderTable $ addHook conf0 $ setOnEncoderStreamCreated closeStream
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3ClosedCriticalStream]
        it
            "MUST send H3_CLOSED_CRITICAL_STREAM if a decoder stream is closed [QPACK 4.2]"
            $ \_ -> do
                let conf =
                        noDecoderTable $ addHook conf0 $ setOnDecoderStreamCreated closeStream
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3ClosedCriticalStream]
        it
            "MUST send H3_CLOSED_CRITICAL_STREAM if an encoder stream ends inside an instruction [QPACK 4.2]"
            $ \_ -> do
                -- The first octet of a Set Dynamic Table Capacity that goes on.
                let conf = noEncoderTable $ addHook conf0 $ setOnEncoderStreamCreated $ \strm -> do
                        sendStream strm "\x3f"
                        closeStream strm
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [H3ClosedCriticalStream]
        it
            "MUST send QPACK_DECODER_STREAM_ERROR if Insert Count Increment is 0 [QPACK 4.4.3]"
            $ \_ -> do
                let conf = addHook conf0 $ setOnDecoderStreamCreated zeroInsertCountIncrement
                runC qcc cconf conf ms
                    `shouldThrow` applicationProtocolErrorsIn [QpackDecoderStreamError]

----------------------------------------------------------------

-- | Making a request the server must refuse and, where we can see what the
-- server hands its application, checking that it handed over nothing: a
-- stream used to be reset and the request handed over anyway.
refusedNotServed :: Maybe (IO Int) -> IO () -> IO ()
refusedNotServed Nothing refused = refused
refusedNotServed (Just count) refused = do
    before' <- count
    refused
    threadDelay 200000
    count `shouldReturn` before'

-- | A client whose encoder will not use the dynamic table, and so has
-- nothing to write on its encoder stream.  For the tests that close that
-- stream: a client that went on writing on it would see the closure itself
-- and close the connection, and the test is about what the server does.
noEncoderTable :: H3.Config -> H3.Config
noEncoderTable conf =
    conf
        { H3.confQEncoderConfig =
            (H3.confQEncoderConfig conf){ecMaxTableCapacity = 0}
        }

-- | A client whose decoder offers no dynamic table, and so has nothing to
-- write on its decoder stream: no acknowledgements, since the server cannot
-- refer to a table, and no Stream Cancellations, which are not sent then.
-- For the test that closes that stream, as 'noEncoderTable'.
noDecoderTable :: H3.Config -> H3.Config
noDecoderTable conf =
    conf
        { H3.confQDecoderConfig =
            (H3.confQDecoderConfig conf){dcMaxTableCapacity = 0}
        }

addHook :: H3.Config -> (H3.Hooks -> H3.Hooks) -> H3.Config
addHook conf modify = conf'
  where
    hooks = H3.confHooks conf
    hooks' = modify hooks
    conf' = conf{H3.confHooks = hooks'}

setOnControlFrameCreated :: ([H3Frame] -> [H3Frame]) -> H3.Hooks -> H3.Hooks
setOnControlFrameCreated f hooks = hooks{H3.onControlFrameCreated = f}

setOnHeadersFrameCreated :: ([H3Frame] -> [H3Frame]) -> H3.Hooks -> H3.Hooks
setOnHeadersFrameCreated f hooks = hooks{H3.onHeadersFrameCreated = f}

setOnControlStreamCreated :: (Stream -> IO ()) -> H3.Hooks -> H3.Hooks
setOnControlStreamCreated f hooks = hooks{H3.onControlStreamCreated = f}

setOnEncoderStreamCreated :: (Stream -> IO ()) -> H3.Hooks -> H3.Hooks
setOnEncoderStreamCreated f hooks = hooks{H3.onEncoderStreamCreated = f}

setOnDecoderStreamCreated :: (Stream -> IO ()) -> H3.Hooks -> H3.Hooks
setOnDecoderStreamCreated f hooks = hooks{H3.onDecoderStreamCreated = f}

----------------------------------------------------------------

startWithNonSettings :: [H3Frame] -> [H3Frame]
startWithNonSettings fs = H3Frame H3FrameMaxPushId "\x01" : fs

doubleSettings :: [H3Frame] -> [H3Frame]
doubleSettings fs = fs ++ [H3Frame H3FrameSettings ""]

controlData :: [H3Frame] -> [H3Frame]
controlData fs = fs ++ [H3Frame H3FrameData ""]

controlHeaders :: [H3Frame] -> [H3Frame]
controlHeaders fs = fs ++ [H3Frame H3FrameHeaders ""]

requestCancelPush :: [H3Frame] -> [H3Frame]
requestCancelPush fs = H3Frame H3FrameCancelPush "" : fs

requestIllegalData :: [H3Frame] -> [H3Frame]
requestIllegalData fs = H3Frame H3FrameData "" : fs

-- [(":method","GET")
-- ,(":scheme","https")
-- ,(":path","/")
-- ] -- the absence of mandatory pseudo-header fields
illegalHeader0 :: [H3Frame] -> [H3Frame]
illegalHeader0 _ = [H3Frame H3FrameHeaders "\x00\x00\xd1\xd7\xc1"]

-- [(":method","GET")
-- ,(":scheme","https")
-- ,(":authority","127.0.0.1")
-- ,(":path","/")
-- ,(":foo","bar") -- the presence of prohibited fields or pseudo-header fields,
-- ]
illegalHeader1 :: [H3Frame] -> [H3Frame]
illegalHeader1 _ =
    [ H3Frame
        H3FrameHeaders
        "\x00\x00\xd1\xd7\x50\x09\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\xc1\x24\x3a\x66\x6f\x6f\x03\x62\x61\x72"
    ]

-- [(":method","GET")
-- ,(":scheme","https")
-- ,(":authority","127.0.0.1")
-- ,("foo","bar")
-- ,(":path","/") -- pseudo-header fields after fields
-- ]
illegalHeader2 :: [H3Frame] -> [H3Frame]
illegalHeader2 _ =
    [ H3Frame
        H3FrameHeaders
        "\x00\x00\xd1\xd7\x50\x09\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\x23\x66\x6f\x6f\x03\x62\x61\x72\xc1"
    ]

-- [(":method","GET")
-- ,(":scheme","https")
-- ,(":authority","127.0.0.1")
-- ,(":path","/")
-- ,(":method","GET")]
illegalHeader3 :: [H3Frame] -> [H3Frame]
illegalHeader3 _ =
    [ H3Frame
        H3FrameHeaders
        "\x00\x00\xd1\xd7\x50\x09\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\xc1\xd1"
    ]

-- [(":method","GET")
-- ,(":scheme","https")
-- ,(":authority","127.0.0.1")
-- ,(":path","/")] ++ static index 99
illegalHeader4 :: [H3Frame] -> [H3Frame]
illegalHeader4 _ =
    [ H3Frame
        H3FrameHeaders
        "\x00\x00\xd1\xd7\x50\x09\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\xc1\xff\x24"
    ]

-- [(":method","GET")
-- ,(":scheme","https")
-- ,(":authority","127.0.0.1")
-- ,(":path","/")] ++ dynamic index 0
--
-- The Required Insert Count in the prefix is 0 and nothing has been inserted,
-- so there is no entry 0 to name.  The decoder used to fold the index back
-- into the table and hand over whatever slot it landed on.
illegalHeader5 :: [H3Frame] -> [H3Frame]
illegalHeader5 _ =
    [ H3Frame
        H3FrameHeaders
        "\x00\x00\xd1\xd7\x50\x09\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\xc1\x80"
    ]

{-
-- [(SettingsQpackBlockedStreams,100)
-- ,(SettingsQpackMaxTableCapacity,4096)
-- ,(SettingsMaxFieldSectionSize,32768)
-- ,(SettingsQpackBlockedStreams,100)] -- duplicated
illegalSettings0 :: [H3Frame]-> [H3Frame]
illegalSettings0 _ = [H3Frame H3FrameSettings "\x07\x40\x64\x01\x50\x00\x06\x80\x00\x80\x00\x07\x40\x64"]
-}

-- [(SettingsQpackBlockedStreams,100)
-- ,(H3SettingsKey 0x2,200) -- HTTP/2 Settings
-- ,(SettingsQpackMaxTableCapacity,4096)
-- ,(SettingsMaxFieldSectionSize,32768)]
-- An identifier with no value behind it.
--
-- Each parameter is a pair of variable-length integers; this frame stops
-- between them, which section 7.1 makes H3_FRAME_ERROR.  Reading off the end
-- used to raise BufferOverrun, which nothing here catches, so the peer was
-- told nothing at all.
truncatedSettings :: [H3Frame] -> [H3Frame]
truncatedSettings _ = [H3Frame H3FrameSettings "\x01"]

-- [(H3SettingsKey 0x21,0) -- reserved, to be ignored
-- ,(H3SettingsKey 0x2,0)]  -- HTTP/2 Settings, which must be refused
--
-- 0x21 is the first of the identifiers section 7.2.4.1 reserves and tells
-- endpoints they SHOULD send.  Reading used to stop at it, so the HTTP/2
-- setting behind it went unseen -- along with anything else a peer put there.
greaseThenHttp2Setting :: [H3Frame] -> [H3Frame]
greaseThenHttp2Setting _ = [H3Frame H3FrameSettings "\x21\x00\x02\x00"]

-- [(H3SettingsKey 0x40,0)
-- ,(H3SettingsKey 0x40,0)] -- the same identifier twice
--
-- 64 in the two-octet form.  The duplicate check was bits in an Int, so
-- identifiers this large repeated unnoticed.
duplicateLargeSetting :: [H3Frame] -> [H3Frame]
duplicateLargeSetting _ = [H3Frame H3FrameSettings "\x40\x40\x00\x40\x40\x00"]

illegalSettings1 :: [H3Frame] -> [H3Frame]
illegalSettings1 _ =
    [ H3Frame
        H3FrameSettings
        "\x07\x40\x64\x02\x40\xc8\x01\x50\x00\x06\x80\x00\x80\x00"
    ]

----------------------------------------------------------------

-- | The fields RFC 9114, section 4.2, names as connection-specific, and TE
-- with a value other than "trailers".
connectionSpecific :: [(HeaderName, BS.ByteString)]
connectionSpecific =
    [ ("connection", "close")
    , ("keep-alive", "timeout=5")
    , ("proxy-connection", "keep-alive")
    , ("transfer-encoding", "chunked")
    , ("upgrade", "websocket")
    , ("te", "gzip")
    ]

-- | Trailers of a single Connection field.
connectionTrailer :: H3.TrailersMaker
connectionTrailer Nothing = return $ H3.Trailers [("connection", "close")]
connectionTrailer (Just _) = return $ H3.NextTrailersMaker connectionTrailer

-- | Opening a unidirectional stream of our own next to the one given, and
-- sending it these octets: a stream type and whatever follows it.
--
-- A client needs three unidirectional streams, and a server may let it have
-- no more than that: quic's default does.  Then the stream cannot be opened
-- at all, and this gives up after a tenth of a second rather than waiting
-- for room that will not come, saying so in the 'IORef'.
openAnother :: IORef Bool -> BS.ByteString -> Stream -> IO ()
openAnother opened bs strm = do
    mstrm <- timeout 100000 $ unidirectionalStream $ streamConnection strm
    forM_ mstrm $ \strm' -> do
        sendStream strm' bs
        writeIORef opened True

-- | Expecting one of the errors when 'openAnother' could open its stream,
-- and leaving the test pending when it could not: against a server that
-- lets a client have only the streams it needs, a second control or QPACK
-- stream cannot be opened, and there is nothing to check.
expectIfOpened
    :: IORef Bool -> IO (Maybe ()) -> [ApplicationProtocolError] -> IO ()
expectIfOpened opened action errs = do
    r <- E.try action
    ok <- readIORef opened
    if not ok
        then
            pendingWith
                "the server allows no unidirectional stream beyond the three a client needs"
        else case r of
            Left e
                | applicationProtocolErrorsIn errs e -> return ()
                | otherwise -> expectationFailure $ "unexpected exception: " ++ show e
            Right _ -> expectationFailure "did not get expected exception"

-- A GOAWAY frame announcing 2^30 octets and then sending none of them.
--
-- A frame length is a variable-length integer, so a peer can claim up to
-- 2^62-1 and have the other end hold whatever it sends towards that.  Nothing
-- has to arrive for the claim to be refused.
overLongFrame :: Stream -> IO ()
overLongFrame strm = sendStream strm "\x07\xc0\x00\x00\x00\x40\x00\x00\x00"

-- SetDynamicTableCapacity 10000000000
largeTableCapacity :: Stream -> IO ()
largeTableCapacity strm = sendStream strm "\x3f\xe1\xc7\xaf\xa0\x25"

-- InsertCountIncrement 0
zeroInsertCountIncrement :: Stream -> IO ()
zeroInsertCountIncrement strm = sendStream strm "\x00"

----------------------------------------------------------------

addQUICHook :: ClientConfig -> (Hooks -> Hooks) -> ClientConfig
addQUICHook cc modify = cc'
  where
    cc' = cc{ccHooks = modify $ ccHooks cc}

setOnResetStreamReceived
    :: (Stream -> ApplicationProtocolError -> IO ()) -> Hooks -> Hooks
setOnResetStreamReceived f hooks = hooks{onResetStreamReceived = f}

----------------------------------------------------------------

applicationProtocolError :: QUICException -> Bool
applicationProtocolError (ApplicationProtocolErrorIsReceived ae _) = ae `elem` [H3GeneralProtocolError, H3InternalError]
applicationProtocolError _ = False

applicationProtocolErrorsIn
    :: [ApplicationProtocolError] -> QUICException -> Bool
applicationProtocolErrorsIn aes qe@(ApplicationProtocolErrorIsReceived ae _) = (ae `elem` aes) || applicationProtocolError qe
applicationProtocolErrorsIn _ _ = False
