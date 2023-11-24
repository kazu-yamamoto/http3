{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HTTP3.Error (
    h3ErrorSpec
  ) where

import Control.Concurrent
import Data.ByteString ()
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import qualified Network.HTTP3.Client as H3
import Network.HTTP3.Internal
import Network.QPACK.Internal
import Network.QUIC
import Network.QUIC.Client
import Network.QUIC.Internal hiding (timeout)
import Test.Hspec
import qualified UnliftIO.Exception as E
import UnliftIO.Timeout

----------------------------------------------------------------

type Millisecond = Int

runC :: ClientConfig -> H3.ClientConfig -> H3.Config -> Millisecond -> IO (Maybe ())
runC qcc cconf conf ms = timeout us $ run qcc $ \conn -> do
    info <- getConnectionInfo conn
    case alpn info of
      Just proto | "hq" `BS.isPrefixOf` proto -> do
                       waitEstablished conn
                       putStrLn $ "Warning: " ++ C8.unpack proto ++ " is negotiated. Skipping this test. Use \"h3spec -s HTTP/3\" next time."
                       E.throwIO $ ApplicationProtocolErrorIsReceived H3InternalError ""
      _                                       -> H3.run conn cconf conf client
  where
    us = ms * 1000
    client :: H3.Client ()
    client sendRequest _aux = do
        let req = H3.requestNoBody methodGet "/" []
        ret <- sendRequest req $ \_rsp -> return ()
        threadDelay 100000
        return ret

h3ErrorSpec :: ClientConfig -> H3.ClientConfig -> Millisecond -> SpecWith a
h3ErrorSpec qcc cconf ms = do
    conf0 <- runIO H3.allocSimpleConfig
    describe "HTTP/3 servers" $ do
        it "MUST send H3_FRAME_UNEXPECTED if DATA is received before HEADERS [HTTP/3 4.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated requestIllegalData
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it "MUST send H3_MESSAGE_ERROR if a pseudo-header is duplicated [HTTP/3 4.1.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader3
                qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
            runC qcc' cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it "MUST send H3_MESSAGE_ERROR if mandatory pseudo-header fields are absent [HTTP/3 4.1.3]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader0
                qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
            runC qcc' cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it "MUST send H3_MESSAGE_ERROR if prohibited pseudo-header fields are present[HTTP/3 4.1.3]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader1
                qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
            runC qcc' cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it "MUST send H3_MESSAGE_ERROR if pseudo-header fields exist after fields [HTTP/3 4.1.3]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader2
                qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \_strm aerr -> E.throwIO (ApplicationProtocolErrorIsReceived aerr "")
            runC qcc' cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it "MUST send H3_MISSING_SETTINGS if the first control frame is not SETTINGS [HTTP/3 6.2.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlFrameCreated startWithNonSettings
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3MissingSettings]
        it "MUST send H3_FRAME_UNEXPECTED if a DATA frame is received on a control stream [HTTP/3 7.2.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlFrameCreated controlData
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it "MUST send H3_FRAME_UNEXPECTED if a HEADERS frame is received on a control stream [HTTP/3 7.2.2]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlFrameCreated controlHeaders
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it "MUST send H3_FRAME_UNEXPECTED if a second SETTINGS frame is received [HTTP/3 7.2.4]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlFrameCreated doubleSettings
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
{- this is MAY
        it "MUST send H3_SETTINGS_ERROR if duplicate setting identifiers exist [HTTP/3 7.2.4]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlFrameCreated illegalSettings0
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3SettingsError]
-}
        it "MUST send H3_SETTINGS_ERROR if HTTP/2 settings are included [HTTP/3 7.2.4.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlFrameCreated illegalSettings1
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3SettingsError]
        it "MUST send H3_FRAME_UNEXPECTED if CANCEL_PUSH is received in a request stream [HTTP/3 7.2.5]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated requestCancelPush
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it "MUST send QPACK_DECOMPRESSION_FAILED if an invalid static table index exits in a field line representation [QPACK 3.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader4
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [QpackDecompressionFailed]
        it "MUST send QPACK_ENCODER_STREAM_ERROR if a new dynamic table capacity value exceeds the limit [QPACK 4.1.3]" $ \_ -> do
            let conf = addHook conf0 $ setOnEncoderStreamCreated largeTableCapacity
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [QpackEncoderStreamError]
        it "MUST send H3_CLOSED_CRITICAL_STREAM if a control stream is closed [QPACK 4.2]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlStreamCreated closeStream
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [H3ClosedCriticalStream]
        it "MUST send QPACK_DECODER_STREAM_ERROR if Insert Count Increment is 0 [QPACK 4.4.3]" $ \_ -> do
            let conf = addHook conf0 $ setOnDecoderStreamCreated zeroInsertCountIncrement
            runC qcc cconf conf ms `shouldThrow` applicationProtocolErrorsIn [QpackDecoderStreamError]

----------------------------------------------------------------

addHook :: H3.Config -> (H3.Hooks -> H3.Hooks) -> H3.Config
addHook conf modify = conf'
  where
    hooks = H3.confHooks conf
    hooks' = modify hooks
    conf' = conf { H3.confHooks = hooks' }

setOnControlFrameCreated :: ([H3Frame] -> [H3Frame]) -> H3.Hooks -> H3.Hooks
setOnControlFrameCreated f hooks = hooks { H3.onControlFrameCreated = f }

setOnHeadersFrameCreated :: ([H3Frame] -> [H3Frame]) -> H3.Hooks -> H3.Hooks
setOnHeadersFrameCreated f hooks = hooks { H3.onHeadersFrameCreated = f }

setOnControlStreamCreated :: (Stream -> IO ()) -> H3.Hooks -> H3.Hooks
setOnControlStreamCreated f hooks = hooks { H3.onControlStreamCreated = f }

setOnEncoderStreamCreated :: (Stream -> IO ()) -> H3.Hooks -> H3.Hooks
setOnEncoderStreamCreated f hooks = hooks { H3.onEncoderStreamCreated = f }

setOnDecoderStreamCreated :: (Stream -> IO ()) -> H3.Hooks -> H3.Hooks
setOnDecoderStreamCreated f hooks = hooks { H3.onDecoderStreamCreated = f }

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
-- ,(":autority","127.0.0.1")
-- ,(":path","/")
-- ,(":foo","bar") -- the presence of prohibited fields or pseudo-header fields,
-- ]
illegalHeader1 :: [H3Frame] -> [H3Frame]
illegalHeader1 _ = [H3Frame H3FrameHeaders "\x00\x00\xd1\xd7\x27\x02\x3a\x61\x75\x74\x6f\x72\x69\x74\x79\x09\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\xc1\x24\x3a\x66\x6f\x6f\x03\x62\x61\x72"]

-- [(":method","GET")
-- ,(":scheme","https")
-- ,(":autority","127.0.0.1")
-- ,("foo","bar")
-- ,(":path","/") -- pseudo-header fields after fields
-- ]
illegalHeader2 :: [H3Frame] -> [H3Frame]
illegalHeader2 _ = [H3Frame H3FrameHeaders "\x00\x00\xd1\xd7\x27\x02\x3a\x61\x75\x74\x6f\x72\x69\x74\x79\x09\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\x23\x66\x6f\x6f\x03\x62\x61\x72\xc1"]

-- [(":method","GET")
-- ,(":scheme","https")
-- ,(":authority","127.0.0.1")
-- ,(":path","/")
-- ,(":method","GET")]
illegalHeader3 :: [H3Frame] -> [H3Frame]
illegalHeader3 _ = [H3Frame H3FrameHeaders "\x00\x00\xd1\xd7\x50\x09\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\xc1\xd1"]

-- [(":method","GET")
-- ,(":scheme","https")
-- ,(":authority","127.0.0.1")
-- ,(":path","/")] ++ static index 99
illegalHeader4 :: [H3Frame] -> [H3Frame]
illegalHeader4 _ = [H3Frame H3FrameHeaders "\x00\x00\xd1\xd7\x50\x09\x31\x32\x37\x2e\x30\x2e\x30\x2e\x31\xc1\xff\x24"]

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
illegalSettings1 :: [H3Frame]-> [H3Frame]
illegalSettings1 _ = [H3Frame H3FrameSettings "\x07\x40\x64\x02\x40\xc8\x01\x50\x00\x06\x80\x00\x80\x00"]

----------------------------------------------------------------

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
    cc' = cc { ccHooks = modify $ ccHooks cc }

setOnResetStreamReceived :: (Stream -> ApplicationProtocolError -> IO ()) -> Hooks -> Hooks
setOnResetStreamReceived f hooks = hooks { onResetStreamReceived = f }

----------------------------------------------------------------

applicationProtocolError :: QUICException -> Bool
applicationProtocolError (ApplicationProtocolErrorIsReceived ae _) = ae `elem` [H3GeneralProtocolError,H3InternalError]
applicationProtocolError _ = False

applicationProtocolErrorsIn :: [ApplicationProtocolError] -> QUICException -> Bool
applicationProtocolErrorsIn aes qe@(ApplicationProtocolErrorIsReceived ae _) = (ae `elem` aes) || applicationProtocolError qe
applicationProtocolErrorsIn _   _                           = False
