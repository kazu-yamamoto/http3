{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HTTP3.ServerSpec where

import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as B
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.QUIC.Client as QUIC
import Test.Hspec

import HTTP3.Config
import HTTP3.Server

spec :: Spec
spec = beforeAll (setup server) $ afterAll teardown h3spec

h3spec :: SpecWith a
h3spec = do
    describe "H3 server" $ do
        it "handles normal cases" $ \_ -> runClient

runClient :: IO ()
runClient = QUIC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf client
  where
    client :: C.Client ()
    client sendRequest _aux =
        foldr1
            concurrently_
            [ client0 sendRequest _aux
            , client1 sendRequest _aux
            , client2 sendRequest _aux
            , client3 sendRequest _aux
            ]

client0 :: C.Client ()
client0 sendRequest _aux = do
    let req = C.requestNoBody methodGet "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200

client1 :: C.Client ()
client1 sendRequest _aux = do
    let req = C.requestNoBody methodGet "/something" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just notFound404

client2 :: C.Client ()
client2 sendRequest _aux = do
    let req = C.requestNoBody methodPut "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just methodNotAllowed405

client3 :: C.Client ()
client3 sendRequest _aux = do
    let req0 = C.requestFile methodPost "/echo" [] $ FileSpec "test/inputFile" 0 1012731
        req = C.setRequestTrailersMaker req0 maker
    sendRequest req $ \rsp -> do
        let comsumeBody = do
                bs <- C.getResponseBodyChunk rsp
                unless (B.null bs) comsumeBody
        comsumeBody
        mt <- C.getResponseTrailers rsp
        firstTrailerValue <$> mt
            `shouldBe` Just "b0870457df2b8cae06a88657a198d9b52f8e2b0a"
  where
    maker = trailersMaker hashInit
