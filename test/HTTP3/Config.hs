{-# LANGUAGE OverloadedStrings #-}

module HTTP3.Config (
    makeTestServerConfig,
    testClientConfig,
    testH3ClientConfig,
    waitForSettings,
) where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.List as L
import qualified Network.HTTP3.Client as H3
import qualified Network.Socket as NS
import Network.TLS (Credentials (..), credentialLoadX509)
import System.IO.Unsafe (unsafePerformIO)

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

-- | The UDP port of this test process, one the OS had free.
--
-- It used to be 8003 for every run, and two runs at once -- from two
-- working trees, say -- reached each other's servers: a test failed, or
-- passed, against another run's server.
testPort :: NS.PortNumber
testPort = unsafePerformIO $
    E.bracket (NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol) NS.close $ \s -> do
        NS.bind s $ NS.SockAddrInet 0 $ NS.tupleToHostAddress (127, 0, 0, 1)
        NS.socketPort s
{-# NOINLINE testPort #-}

testServerConfig :: ServerConfig
testServerConfig =
    defaultServerConfig
        { scAddresses = [("127.0.0.1", testPort)]
        , -- Room for more unidirectional streams than the three a client
          -- needs, so that a test can open a second control or QPACK stream
          -- and see it refused.  The default of three leaves no room for one.
          scParameters =
            (scParameters defaultServerConfig){initialMaxStreamsUni = 10}
        }

testClientConfig :: ClientConfig
testClientConfig =
    defaultClientConfig
        { ccPortName = show testPort
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
