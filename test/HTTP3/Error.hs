{-# LANGUAGE OverloadedStrings #-}

module HTTP3.Error (
    h3ErrorSpec
  ) where

import Control.Concurrent
import qualified Control.Exception as E
import Data.ByteString ()
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import qualified Network.HTTP3.Client as H3
import Network.HTTP3.Internal
import Network.QPACK.Internal
import qualified Network.QUIC as QUIC
import System.Timeout
import Test.Hspec

----------------------------------------------------------------

runC :: QUIC.ClientConfig -> H3.ClientConfig -> H3.Config -> IO (Maybe ())
runC qcc cconf conf = timeout 2000000 $ QUIC.runQUICClient qcc $ \conn -> do
    info <- QUIC.getConnectionInfo conn
    case QUIC.alpn info of
      Just proto | "hq" `BS.isPrefixOf` proto -> do
                       QUIC.waitEstablished conn
                       putStrLn $ "Warning: " ++ C8.unpack proto ++ " is negotiated. Skipping this test. Use \"h3spec -s HTTP/3\" next time."
                       E.throwIO $ QUIC.ApplicationProtocolErrorIsReceived H3InternalError ""
      _                                       -> H3.run conn cconf conf client
  where
    client sendRequest = do
        let req = H3.requestNoBody methodGet "/" []
        ret <- sendRequest req $ \_rsp -> return ()
        threadDelay 100000
        return ret

h3ErrorSpec :: QUIC.ClientConfig -> H3.ClientConfig -> SpecWith a
h3ErrorSpec qcc cconf = do
    conf0 <- runIO H3.allocSimpleConfig
    describe "HTTP/3 servers" $ do
        it "MUST send H3_FRAME_UNEXPECTED if DATA is received before HEADERS [HTTP/3 4.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated requestIllegalData
            runC qcc cconf conf `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it "MUST send H3_MESSAGE_ERROR if a pseudo-header is duplicated [HTTP/3 4.1.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader3
                qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \strm aerr -> QUIC.exitConnectionByStream strm (QUIC.ApplicationProtocolErrorIsReceived aerr "")
            runC qcc' cconf conf `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it "MUST send H3_MESSAGE_ERROR if mandatory pseudo-header fields are absent [HTTP/3 4.1.3]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader0
                qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \strm aerr -> QUIC.exitConnectionByStream strm (QUIC.ApplicationProtocolErrorIsReceived aerr "")
            runC qcc' cconf conf `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it "MUST send H3_MESSAGE_ERROR if prohibited pseudo-header fields are present[HTTP/3 4.1.3]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader1
                qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \strm aerr -> QUIC.exitConnectionByStream strm (QUIC.ApplicationProtocolErrorIsReceived aerr "")
            runC qcc' cconf conf `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it "MUST send H3_MESSAGE_ERROR if pseudo-header fields exist after fields [HTTP/3 4.1.3]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader2
                qcc' = addQUICHook qcc $ setOnResetStreamReceived $ \strm aerr -> QUIC.exitConnectionByStream strm (QUIC.ApplicationProtocolErrorIsReceived aerr "")
            runC qcc' cconf conf `shouldThrow` applicationProtocolErrorsIn [H3MessageError]
        it "MUST send H3_MISSING_SETTINGS if the first control frame is not SETTINGS [HTTP/3 6.2.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlFrameCreated startWithNonSettings
            runC qcc cconf conf `shouldThrow` applicationProtocolErrorsIn [H3MissingSettings]
        it "MUST send H3_FRAME_UNEXPECTED if a DATA frame is received on a control stream [HTTP/3 7.2.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlFrameCreated controlData
            runC qcc cconf conf `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it "MUST send H3_FRAME_UNEXPECTED if a HEADERS frame is received on a control stream [HTTP/3 7.2.2]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlFrameCreated controlHeaders
            runC qcc cconf conf `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it "MUST send H3_FRAME_UNEXPECTED if a second SETTINGS frame is received [HTTP/3 7.2.4]" $ \_ -> do
            let conf = addHook conf0 $ setOnControlFrameCreated doubleSettings
            runC qcc cconf conf `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it "MUST send H3_FRAME_UNEXPECTED if CANCEL_PUSH is received in a request stream [HTTP/3 7.2.5]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated requestCancelPush
            runC qcc cconf conf `shouldThrow` applicationProtocolErrorsIn [H3FrameUnexpected]
        it "MUST send QPACK_DECOMPRESSION_FAILED if an invalid static table index exits in a field line representation [QPACK 3.1]" $ \_ -> do
            let conf = addHook conf0 $ setOnHeadersFrameCreated illegalHeader4
            runC qcc cconf conf `shouldThrow` applicationProtocolErrorsIn [QpackDecompressionFailed]

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

----------------------------------------------------------------

startWithNonSettings :: [H3Frame] -> [H3Frame]
startWithNonSettings fs = H3Frame H3FrameData "" : fs

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

----------------------------------------------------------------

addQUICHook :: QUIC.ClientConfig -> (QUIC.Hooks -> QUIC.Hooks) -> QUIC.ClientConfig
addQUICHook cc modify = cc'
  where
    conf = QUIC.ccConfig cc
    hooks = QUIC.confHooks conf
    hooks' = modify hooks
    conf' = conf { QUIC.confHooks = hooks' }
    cc' = cc { QUIC.ccConfig = conf' }

setOnResetStreamReceived :: (QUIC.Stream -> ApplicationProtocolError -> IO ()) -> QUIC.Hooks -> QUIC.Hooks
setOnResetStreamReceived f hooks = hooks { QUIC.onResetStreamReceived = f }

----------------------------------------------------------------

applicationProtocolError :: QUIC.QUICException -> Bool
applicationProtocolError (QUIC.ApplicationProtocolErrorIsReceived ae _) = ae `elem` [H3GeneralProtocolError,H3InternalError]
applicationProtocolError _ = False

applicationProtocolErrorsIn :: [QUIC.ApplicationProtocolError] -> QUIC.QUICException -> Bool
applicationProtocolErrorsIn aes qe@(QUIC.ApplicationProtocolErrorIsReceived ae _) = (ae `elem` aes) || applicationProtocolError qe
applicationProtocolErrorsIn _   _                           = False
