{-# LANGUAGE OverloadedStrings #-}

module HTTP3.HandlerSpec where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Internal (
    ApplicationProtocolError (..),
    StreamResetIsReceived (..),
 )
import Network.HTTP3.Server
import qualified Network.QUIC.Client as QUIC
import System.Timeout (timeout)
import Test.Hspec

import HTTP3.Config
import HTTP3.Server

-- | A server whose handlers time out after a second of not sending.
spec :: Spec
spec = beforeAll (setupShortTimeout server) $ afterAll teardown $ do
    describe "H3 server with a short timeout" $ do
        it "resets a response its handler is killed part-way through" $ \_ -> do
            -- The handler sends some of the body and then nothing: the
            -- timeout kills it.  The stream used to be ended with a FIN, and
            -- the client took what it had for the whole body.
            r <- timeout 5000000 $ QUIC.run testClientConfig $ \conn ->
                E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
                    C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
                        E.try $ sendRequest (C.requestNoBody methodGet "/stall" []) $ \rsp -> do
                            let drain acc = do
                                    bs <- C.getResponseBodyChunk rsp
                                    if B.null bs then return acc else drain (acc <> bs)
                            drain ""
            r `shouldBe` Just (Left (StreamResetIsReceived H3InternalError))
