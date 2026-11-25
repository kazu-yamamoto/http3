{-# LANGUAGE OverloadedStrings #-}

module HTTP3.CloseSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Network.HQ.Server as HQ
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
spec = do
    describe "H3 server whose client closes normally" $ do
        -- The client returning closes with H3_NO_ERROR: a normal end, and
        -- not a failure of the server.
        it "returns from run" $
            returnsNormally $ \conn conf ->
                run conn conf $ \_req _aux sendResponse ->
                    sendResponse (responseBuilder ok200 [] "hello") []
        it "returns from runIO" $
            returnsNormally $ \conn conf ->
                runIO conn conf $ \sio -> return $ forever $ do
                    (strm, _req) <- sioReadRequest sio
                    sioWriteResponse sio strm $ responseBuilder ok200 [] "hello"
    describe "HQ server whose client closes normally" $
        it "returns from run" $
            returnsNormally' hqRequest $ \conn -> do
                E.bracket HQ.allocSimpleConfig HQ.freeSimpleConfig $ \conf ->
                    HQ.run conn conf $ \_req _aux sendResponse ->
                        sendResponse (responseBuilder ok200 [] "hello") []

-- | Serving one request with the given server, and how it returned.
returnsNormally :: (QUIC.Connection -> Config -> IO ()) -> IO ()
returnsNormally serve =
    returnsNormally' (Just <$> oneRequest) $ \conn ->
        E.bracket allocSimpleConfig freeSimpleConfig $ serve conn

returnsNormally' :: IO (Maybe Status) -> (QUIC.Connection -> IO ()) -> IO ()
returnsNormally' client serve = do
    returned <- newEmptyMVar
    E.bracket (setupServer returned) teardown $ \_ -> do
        timeout 3000000 client `shouldReturn` Just (Just ok200)
        timeout 3000000 (takeMVar returned) `shouldReturn` Just (Right ())
  where
    setupServer returned = do
        sc <- makeTestServerConfig
        tid <- forkIO $ QUICS.run sc $ \conn -> do
            r <- E.try $ serve conn
            putMVar returned $ either (\e -> Left $ show (e :: E.SomeException)) Right r
        threadDelay 500000 -- give enough time to the server
        return tid

-- | An HQ request for "/", answered in full.
hqRequest :: IO (Maybe Status)
hqRequest = QUICC.run testClientConfig $ \conn -> do
    strm <- QUIC.stream conn
    QUIC.sendStream strm "GET /\r\n"
    QUIC.shutdownStream strm
    let drain acc = do
            bs <- QUIC.recvStream strm 1024
            if BS.null bs then return acc else drain (acc <> bs)
    body <- drain ""
    return $ if body == "hello" then Just ok200 else Nothing

oneRequest :: IO Status
oneRequest = QUICC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
            sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                return $ maybe (error "no status") id $ C.responseStatus rsp
