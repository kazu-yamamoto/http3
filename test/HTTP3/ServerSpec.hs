{-# LANGUAGE OverloadedStrings #-}

module HTTP3.ServerSpec where

import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as B
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.QUIC as QUIC
import Test.Hspec

import HTTP3.Config
import HTTP3.Server

testH3ClientConfig :: C.ClientConfig
testH3ClientConfig = C.ClientConfig "https" "127.0.0.1" -- fixme

spec :: Spec
spec = beforeAll setup $ afterAll teardown h3spec

h3spec :: SpecWith a
h3spec = do
    describe "H3 server" $ do
        it "handles normal cases" $ \_ -> runClient

runClient :: IO ()
runClient = QUIC.runQUICClient testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
      C.run conn testH3ClientConfig conf client
  where
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
    maker = trailersMaker hashInit
