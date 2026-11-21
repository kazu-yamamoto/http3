{-# LANGUAGE OverloadedStrings #-}

module HQ.ClientSpec where

import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Network.HQ.Client as HQ
import Network.HTTP.Semantics (OutObj (..))
import Network.HTTP.Semantics.Client (requestNoBody)
import Network.HTTP.Semantics.Client.Internal (Request (..))
import qualified Network.HTTP.Semantics.Error as HE
import Network.HTTP.Types
import qualified Network.QUIC.Client as QUICC
import System.Timeout (timeout)
import Test.Hspec

import HQ.ServerSpec (setupHQ)
import HTTP3.Config
import HTTP3.Server (teardown)

spec :: Spec
spec = beforeAll setupHQ $
    afterAll teardown $
        describe "HQ client" $ do
            it "sends a GET request" $ \_ ->
                timeout 3000000 (send $ requestNoBody methodGet "/" [])
                    `shouldReturn` Just "hello"
            -- It was sent as GET.
            it "refuses a request that is not GET" $ \_ ->
                send (requestNoBody methodPost "/" []) `shouldThrow` unsupported
            -- It failed in 'fromJust'.
            it "refuses a request without a path" $ \_ -> do
                let Request outobj = requestNoBody methodGet "/" []
                    noPath = filter ((/= ":path") . fst) $ outObjHeaders outobj
                send (Request outobj{outObjHeaders = noPath}) `shouldThrow` unsupported
            -- The reset read as the end, and the body cut short as a whole.
            it "tells the application of a response the server resets" $ \_ ->
                timeout 3000000 (drainAll $ requestNoBody methodGet "/fail" [])
                    `shouldThrow` streamReset

send :: HQ.Request -> IO ByteString
send req = QUICC.run testClientConfig $ \conn ->
    E.bracket HQ.allocSimpleConfig HQ.freeSimpleConfig $ \conf ->
        HQ.run conn HQ.defaultClientConfig conf $ \sendRequest _aux ->
            sendRequest req HQ.getResponseBodyChunk

drainAll :: HQ.Request -> IO ByteString
drainAll req = QUICC.run testClientConfig $ \conn ->
    E.bracket HQ.allocSimpleConfig HQ.freeSimpleConfig $ \conf ->
        HQ.run conn HQ.defaultClientConfig conf $ \sendRequest _aux ->
            sendRequest req $ \rsp ->
                let loop acc = do
                        bs <- HQ.getResponseBodyChunk rsp
                        if BS.null bs then return acc else loop (acc <> bs)
                 in loop ""

streamReset :: Selector HE.HTTPError
streamReset HE.StreamErrorIsReceived{} = True
streamReset _ = False

unsupported :: Selector HQ.UnsupportedRequest
unsupported = const True
