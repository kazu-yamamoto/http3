{-# LANGUAGE OverloadedStrings #-}

module HTTP3.HandlerSpec where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.QUIC.Client as QUIC
import System.Timeout (timeout)
import Test.Hspec

import HTTP3.Config
import HTTP3.Server

-- | A server with a short idle timeout.
spec :: Spec
spec = beforeAll (setupShortIdle server) $ afterAll teardown $ do
    describe "H3 server with a short idle timeout" $ do
        it "keeps the connection alive for a slow handler" $ \_ -> do
            -- The handler sends some of the body and then nothing for three
            -- idle timeouts.  It used to be killed, and the stream reset;
            -- the connection is kept alive now, as over HTTP/2.
            r <- timeout 5000000 $ QUIC.run testClientConfig $ \conn ->
                E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
                    C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
                        sendRequest (C.requestNoBody methodGet "/slow" []) $ \rsp -> do
                            let drain acc = do
                                    bs <- C.getResponseBodyChunk rsp
                                    if B.null bs then return acc else drain (acc <> bs)
                            drain ""
            r `shouldBe` Just "partial rest"
