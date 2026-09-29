{-# LANGUAGE OverloadedStrings #-}

module HTTP3.Config (
    makeTestServerConfig,
    testClientConfig,
    testH3ClientConfig,
    waitForSettings,
) where

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import qualified Data.List as L
import qualified Network.HTTP3.Client as H3
import Network.TLS (Credentials (..), credentialLoadX509)

import Network.QUIC.Client
import Network.QUIC.Internal

makeTestServerConfig :: IO ServerConfig
makeTestServerConfig = do
    cred <-
        either error id
            <$> credentialLoadX509 "test/servercert.pem" "test/serverkey.pem"
    let credentials = Credentials [cred]
    return
        testServerConfig
            { scCredentials = credentials
            , scALPN = Just chooseALPN
            }

testServerConfig :: ServerConfig
testServerConfig =
    defaultServerConfig
        { scAddresses = [("127.0.0.1", 8003)]
        , -- Room for more unidirectional streams than the three a client
          -- needs, so that a test can open a second control or QPACK stream
          -- and see it refused.  The default of three leaves no room for one.
          scParameters =
            (scParameters defaultServerConfig){initialMaxStreamsUni = 10}
        }

testClientConfig :: ClientConfig
testClientConfig =
    defaultClientConfig
        { ccPortName = "8003"
        , ccValidate = False
        }

chooseALPN :: Version -> [ByteString] -> IO ByteString
chooseALPN _ver protos = return $ case mh3idx of
    Nothing -> case mhqidx of
        Nothing -> ""
        Just _ -> "hq"
    Just h3idx -> case mhqidx of
        Nothing -> "h3"
        Just hqidx -> if h3idx < hqidx then "h3" else "hq"
  where
    mh3idx = "h3" `L.elemIndex` protos
    mhqidx = "hq" `L.elemIndex` protos

testH3ClientConfig :: H3.ClientConfig
testH3ClientConfig = H3.defaultClientConfig{H3.authority = "127.0.0.1"}

-- | Giving the peer's SETTINGS time to take effect, after a round trip.
--
-- A response is no sign that they have: they come on the control stream,
-- and that is read by a thread of its own, so the response on a request
-- stream can get to the application first.  A test that depends on them --
-- a limit the peer announced, or a dynamic table it allowed -- used to fail
-- a time or two in a hundred.  There is nothing to wait on, so this sleeps.
waitForSettings :: IO ()
waitForSettings = threadDelay 100000
