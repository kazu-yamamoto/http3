{-# LANGUAGE OverloadedStrings #-}

module HTTP3.Error (
    h3ErrorSpec
  ) where

import Control.Concurrent
import Data.ByteString ()
import Network.HTTP.Types
import qualified Network.HTTP3.Client as H3
import Network.HTTP3.Internal
import qualified Network.QUIC as QUIC
import System.Timeout
import Test.Hspec

----------------------------------------------------------------

runC :: QUIC.ClientConfig -> H3.ClientConfig -> H3.Config -> IO (Maybe ())
runC qcc cconf conf = timeout 2000000 $ QUIC.runQUICClient qcc $ \conn ->
    H3.run conn cconf conf client
  where
    client sendRequest = do
        let req = H3.requestNoBody methodGet "/" []
        ret <- sendRequest req $ \rsp -> print $ H3.responseStatus rsp
        threadDelay 100000
        return ret

h3ErrorSpec :: QUIC.ClientConfig -> H3.ClientConfig -> SpecWith a
h3ErrorSpec qcc cconf = do
    conf0 <- runIO H3.allocSimpleConfig
    describe "H3 servers" $ do
        it "MUST send H3_MISSING_SETTINGS if the first control frame is not H3_MISSING_SETTINGS [HTTP/3 6.2.1]" $ \_ -> do
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

----------------------------------------------------------------

addHook :: H3.Config -> (H3.Hooks -> H3.Hooks) -> H3.Config
addHook conf modify = conf'
  where
    hooks = H3.confHooks conf
    hooks' = modify hooks
    conf' = conf { H3.confHooks = hooks' }

setOnControlFrameCreated :: ([H3Frame] -> [H3Frame]) -> H3.Hooks -> H3.Hooks
setOnControlFrameCreated f hooks = hooks { H3.onControlFrameCreated = f }

----------------------------------------------------------------

startWithNonSettings :: [H3Frame] -> [H3Frame]
startWithNonSettings fs = H3Frame H3FrameData "" : fs

doubleSettings :: [H3Frame] -> [H3Frame]
doubleSettings fs = fs ++ [H3Frame H3FrameSettings ""]

controlData :: [H3Frame] -> [H3Frame]
controlData fs = fs ++ [H3Frame H3FrameData ""]

controlHeaders :: [H3Frame] -> [H3Frame]
controlHeaders fs = fs ++ [H3Frame H3FrameHeaders ""]

----------------------------------------------------------------

applicationProtocolError :: QUIC.QUICException -> Bool
applicationProtocolError (QUIC.ApplicationProtocolErrorIsReceived ae _) = ae `elem` [H3GeneralProtocolError,H3InternalError]
applicationProtocolError _ = False

applicationProtocolErrorsIn :: [QUIC.ApplicationProtocolError] -> QUIC.QUICException -> Bool
applicationProtocolErrorsIn aes qe@(QUIC.ApplicationProtocolErrorIsReceived ae _) = (ae `elem` aes) || applicationProtocolError qe
applicationProtocolErrorsIn _   _                           = False
