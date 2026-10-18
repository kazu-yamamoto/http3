{-# LANGUAGE OverloadedStrings #-}

module HTTP3.InformationalSpec where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Internal (H3Frame (..), H3FrameType (..))
import Network.HTTP3.Server
import qualified Network.QUIC.Client as QUIC
import System.Timeout (timeout)
import Test.Hspec

import HTTP3.Config
import HTTP3.Server

-- | A server whose responses all begin with two informational responses,
-- 100 (Continue) and 103 (Early Hints), each a HEADERS frame of its own.
spec :: Spec
spec = beforeAll (setupWith early server 4096) $ afterAll teardown $ do
    describe "H3 client" $ do
        it "takes the final response past informational ones" $ \_ -> do
            -- The first header section was taken for the response, and the
            -- body reader took the next one for trailers: the client saw a
            -- 100 with no body, and the final response was lost.
            r <- timeout 3000000 $ QUIC.run testClientConfig $ \conn ->
                E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
                    C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
                        sendRequest (C.requestNoBody methodGet "/" []) $ \rsp -> do
                            let drain acc = do
                                    bs <- C.getResponseBodyChunk rsp
                                    if B.null bs then return acc else drain (acc <> bs)
                            body <- drain ""
                            return (C.responseStatus rsp, body)
            r `shouldBe` Just (Just ok200, "Hello, world!\n")
  where
    early conf =
        conf
            { confHooks =
                (confHooks conf){onHeadersFrameCreated = (informational ++)}
            }
    -- Required Insert Count 0 and Base 0, then one field line referring to
    -- the static table: :status 100 is index 63, :status 103 index 24.
    informational =
        [ H3Frame H3FrameHeaders "\x00\x00\xff\x00"
        , H3Frame H3FrameHeaders "\x00\x00\xd8"
        ]
