{-# LANGUAGE OverloadedStrings #-}

module HQ.ServerSpec where

import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString)
import qualified Network.HQ.Server as HQ
import Network.HTTP.Types
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Client as QUICC
import qualified Network.QUIC.Server as QUICS
import System.Timeout (timeout)
import Test.Hspec

import HTTP3.Config
import HTTP3.Server (teardown)

spec :: Spec
spec = beforeAll setupHQ $
    afterAll teardown $
        describe "HQ server" $ do
            it "answers a request" $ \_ ->
                request "GET /\r\n" `shouldReturn` Just ("hello", Nothing)
            it "sends a body whose first chunk fills nothing" $ \_ ->
                fmap (BS.length . fst) <$> request "GET /large\r\n"
                    `shouldReturn` Just largeSize
            it "takes the path alone from a request line with a version" $ \_ ->
                request "GET /path HTTP/1.0\r\n" `shouldReturn` Just ("/path", Nothing)
            it "takes a request line ending in LF alone" $ \_ ->
                request "GET /path\n" `shouldReturn` Just ("/path", Nothing)
            it "resets the stream of a request that is not GET" $ \_ ->
                fmap snd <$> request "POST /path\r\n" `shouldReturn` Just (Just resetCode)
            it "resets the stream of a response its handler failed to finish" $ \_ ->
                fmap snd <$> request "GET /fail\r\n" `shouldReturn` Just (Just resetCode)
            it "resets the stream of a request line that goes on and on" $ \_ ->
                fmap snd <$> request (BS.replicate 100000 0x61)
                    `shouldReturn` Just (Just resetCode)

resetCode :: QUIC.ApplicationProtocolError
resetCode = QUIC.ApplicationProtocolError 0x102

setupHQ :: IO ThreadId
setupHQ = do
    sc <- makeTestServerConfig
    tid <- forkIO $ QUICS.run sc $ \conn ->
        E.bracket HQ.allocSimpleConfig HQ.freeSimpleConfig $ \conf ->
            HQ.run conn conf hqServer
    threadDelay 500000 -- give enough time to the server
    return tid

-- | Large enough for 'byteString' to insert it as a chunk of its own: the
-- builder then fills nothing in its first step.
largeSize :: Int
largeSize = 20000

-- | "/" answers hello, "/fail" fails after part of its response, "/large"
-- answers 'largeSize' octets, and anything else answers with its path.
hqServer :: HQ.Server
hqServer req _aux sendResponse = case HQ.requestPath req of
    Just "/" -> sendResponse (HQ.responseBuilder ok200 [] "hello") []
    Just "/fail" -> sendResponse (HQ.responseStreaming ok200 [] failing) []
    Just "/large" ->
        sendResponse
            (HQ.responseBuilder ok200 [] (byteString (BS.replicate largeSize 0x62)))
            []
    Just path -> sendResponse (HQ.responseBuilder ok200 [] (byteString path)) []
    Nothing -> sendResponse (HQ.responseNoBody badRequest400 []) []
  where
    failing :: (Builder -> IO ()) -> IO () -> IO ()
    failing write flush = do
        write "partial"
        flush
        E.throwIO $ userError "failing"

-- | Sending the octets as a request, and reading the response: what came,
-- and the code of a reset if there was one.
request
    :: BS.ByteString
    -> IO (Maybe (BS.ByteString, Maybe QUIC.ApplicationProtocolError))
request bs = timeout 3000000 $ QUICC.run testClientConfig $ \conn -> do
    strm <- QUIC.stream conn
    QUIC.sendStream strm bs
    QUIC.shutdownStream strm
    let loop acc = do
            x <- QUIC.recvStream strm 1024
            if BS.null x then return acc else loop (acc <> x)
    body <- loop "" `E.catch` \(E.SomeException _) -> return ""
    reset <- QUIC.resetReceived strm
    return (body, reset)
