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

testH3ClientConfig :: H3.ClientConfig
testH3ClientConfig = H3.ClientConfig "https" "127.0.0.1" -- fixme

runC :: QUIC.ClientConfig -> H3.Config -> IO (Maybe ())
runC qcc conf = timeout 2000000 $ QUIC.runQUICClient qcc $ \conn ->
    H3.run conn testH3ClientConfig conf client
  where
    client sendRequest = do
        let req = H3.requestNoBody methodGet "/" []
        ret <- sendRequest req $ \rsp -> print $ H3.responseStatus rsp
        threadDelay 100000
        return ret

h3ErrorSpec :: QUIC.ClientConfig -> SpecWith a
h3ErrorSpec qcc = do
    conf0 <- runIO H3.allocSimpleConfig
    describe "H3 servers" $ do
        it "MUST send H3_MISSING_SETTINGS if the first control frame is not H3_MISSING_SETTINGS [HTTP/3 6.2.1]" $ \_ -> do
            let conf = conf0 {
                    H3.confHooks = (H3.confHooks conf0) {
                          H3.onControlFrameCreated = \fs -> H3Frame H3FrameData "" : fs
                        }
                    }
            runC qcc conf `shouldThrow` applicationProtocolErrorsIn [H3MissingSettings]


applicationProtocolError :: QUIC.QUICException -> Bool
applicationProtocolError (QUIC.ApplicationProtocolErrorIsReceived ae _) = ae `elem` [H3GeneralProtocolError,H3InternalError]
applicationProtocolError _ = False

applicationProtocolErrorsIn :: [QUIC.ApplicationProtocolError] -> QUIC.QUICException -> Bool
applicationProtocolErrorsIn aes qe@(QUIC.ApplicationProtocolErrorIsReceived ae _) = (ae `elem` aes) || applicationProtocolError qe
applicationProtocolErrorsIn _   _                           = False
