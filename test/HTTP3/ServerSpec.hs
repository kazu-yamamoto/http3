{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module HTTP3.ServerSpec where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Crypto.Hash (Context, SHA1) -- cryptonite
import qualified Crypto.Hash as CH
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as C8
import Data.IP ()
import Network.HPACK
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.QUIC as QUIC
import Network.Socket ()
import Network.TLS (credentialLoadX509, Credentials(..))
import Test.Hspec

host :: String
host = "127.0.0.1"

port :: String
port = "44333"

spec :: Spec
spec = do
    describe "server" $ do
        it "handles normal cases" $
            E.bracket (forkIO runServer) killThread $ \_ -> do
                threadDelay 10000
                runClient

runServer :: IO ()
runServer = do
    Right cred <- credentialLoadX509 "test/servercert.pem" "test/serverkey.pem"
    let creds = Credentials [cred]
    QUIC.runQUICServer (quicServerConf creds) (\conn -> run conn defaultConfig server)

server :: Server
server req _aux sendResponse = case requestMethod req of
  Just "GET"  -> case requestPath req of
                   Just "/" -> sendResponse responseHello []
                   _        -> sendResponse response404 []
  Just "POST" -> case requestPath req of
                   Just "/echo" -> sendResponse (responseEcho req) []
                   _        -> sendResponse responseHello []
  _           -> sendResponse response405 []

responseHello :: Response
responseHello = responseBuilder ok200 header body
  where
    header = [("Content-Type", "text/plain")]
    body = byteString "Hello, world!\n"

response404 :: Response
response404 = responseNoBody notFound404 []

response405 :: Response
response405 = responseNoBody methodNotAllowed405 []

responseEcho :: Request -> Response
responseEcho req = setResponseTrailersMaker h2rsp maker
  where
    h2rsp = responseStreaming ok200 header streamingBody
    header = [("Content-Type", "text/plain")]
    streamingBody write _flush = do
        loop
        mt <- getRequestTrailers req
        firstTrailerValue <$> mt `shouldBe` Just "b0870457df2b8cae06a88657a198d9b52f8e2b0a"
      where
        loop = do
            bs <- getRequestBodyChunk req
            unless (B.null bs) $ do
                void $ write $ byteString bs
                loop
    maker = trailersMaker (CH.hashInit :: Context SHA1)

-- Strictness is important for Context.
trailersMaker :: Context SHA1 -> Maybe ByteString -> IO NextTrailersMaker
trailersMaker ctx Nothing = return $ Trailers [("X-SHA1", sha1)]
  where
    !sha1 = C8.pack $ show $ CH.hashFinalize ctx
trailersMaker ctx (Just bs) = return $ NextTrailersMaker $ trailersMaker ctx'
  where
    !ctx' = CH.hashUpdate ctx bs

runClient :: IO ()
runClient = QUIC.runQUICClient quicClientConf $ \conn ->
    C.run conn cliconf defaultConfig client
  where
    cliconf = C.ClientConfig "https" (C8.pack host)
    client sendRequest = mapConcurrently_ ($ sendRequest) clients
    clients = [client0,client1,client2,client3]

client0 :: C.Client ()
client0 sendRequest = do
    let req = C.requestNoBody methodGet "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200

client1 :: C.Client ()
client1 sendRequest = do
    let req = C.requestNoBody methodGet "/something" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just notFound404

client2 :: C.Client ()
client2 sendRequest = do
    let req = C.requestNoBody methodPut "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just methodNotAllowed405

client3 :: C.Client ()
client3 sendRequest = do
    let req0 = C.requestFile methodPost "/echo" [] $ FileSpec "test/inputFile" 0 1012731
        req = C.setRequestTrailersMaker req0 maker
    sendRequest req $ \rsp -> do
        let comsumeBody = do
                bs <- C.getResponseBodyChunk rsp
                unless (B.null bs) comsumeBody
        comsumeBody
        mt <- C.getResponseTrailers rsp
        firstTrailerValue <$> mt `shouldBe` Just "b0870457df2b8cae06a88657a198d9b52f8e2b0a"
  where
    !maker = trailersMaker (CH.hashInit :: Context SHA1)

firstTrailerValue :: HeaderTable -> HeaderValue
firstTrailerValue = snd . Prelude.head . fst

quicClientConf :: QUIC.ClientConfig
quicClientConf = QUIC.defaultClientConfig {
    QUIC.ccServerName = host
  , QUIC.ccPortName   = port
  }

quicServerConf :: Credentials -> QUIC.ServerConfig
quicServerConf creds = QUIC.defaultServerConfig {
    QUIC.scAddresses    = [(read host, read port)]
  , QUIC.scConfig     = QUIC.defaultConfig {
        QUIC.confCredentials = creds
      }
  }
