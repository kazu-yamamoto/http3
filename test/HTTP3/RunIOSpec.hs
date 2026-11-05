{-# LANGUAGE OverloadedStrings #-}

module HTTP3.RunIOSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Client as QUICC
import qualified Network.QUIC.Server as QUICS
import System.Timeout (timeout)
import Test.Hspec hiding (runIO)

import HTTP3.Config
import HTTP3.Server (teardown)

spec :: Spec
spec =
    describe "H3 server with runIO" $
        it "serves a request while an earlier stream has sent no HEADERS" $
            E.bracket setupIO teardown $ \_ ->
                timeout 3000000 stalledThenRequest `shouldReturn` Just ()

setupIO :: IO ThreadId
setupIO = do
    sc <- makeTestServerConfig
    tid <- forkIO $ QUICS.run sc $ \conn ->
        E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
            runIO conn conf $ \sio -> return $ forever $ do
                (strm, _req) <- sioReadRequest sio
                sioWriteResponse sio strm $ responseBuilder ok200 [] "hello"
    threadDelay 500000 -- give enough time to the server
    return tid

-- | A bidirectional stream carrying only the type of a HEADERS frame, and
-- then an ordinary request on the same connection.
stalledThenRequest :: IO ()
stalledThenRequest = QUICC.run testClientConfig $ \conn -> do
    strm <- QUIC.stream conn
    QUIC.sendStream strm "\x01"
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
            sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                C.responseStatus rsp `shouldBe` Just ok200
