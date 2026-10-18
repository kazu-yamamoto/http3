{-# LANGUAGE OverloadedStrings #-}

module HTTP3.InformationalSpec where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Internal (
    ApplicationProtocolError (..),
    H3Frame (..),
    H3FrameType (..),
 )
import Network.HTTP3.Server
import qualified Network.QUIC as Q
import qualified Network.QUIC.Client as QUIC
import System.Timeout (timeout)
import Test.Hspec

import HTTP3.Config
import HTTP3.Server

spec :: Spec
spec = do
    -- Two informational responses, 100 (Continue) and 103 (Early Hints),
    -- each a HEADERS frame of its own, before every response.
    describe "H3 client with informational responses" $
        beforeAll (setupWith (early [continue, earlyHints]) server 4096) $
            afterAll teardown $
                it "takes the final response past informational ones" $ \_ -> do
                    -- The first header section was taken for the response,
                    -- and the body reader took the next one for trailers: the
                    -- client saw a 100 with no body, and the final response
                    -- was lost.
                    r <- get
                    case r of
                        Just (Right x) -> x `shouldBe` (Just ok200, "Hello, world!\n")
                        _ -> expectationFailure $ "unexpected: " ++ show r
    -- Twenty of them before every response.
    describe "H3 client with too many informational responses" $
        beforeAll (setupWith (early (replicate 20 earlyHints)) server 4096) $
            afterAll teardown $
                it "resets a response with too many informational ones" $ \_ -> do
                    -- RFC 9114 sets no limit, and a server could go on sending
                    -- them for ever.
                    r <- get
                    case r of
                        Just (Left (Q.ApplicationProtocolErrorIsSent H3ExcessiveLoad _)) ->
                            return ()
                        _ -> expectationFailure $ "unexpected: " ++ show r
  where
    get :: IO (Maybe (Either Q.QUICException (Maybe Status, B.ByteString)))
    get = timeout 3000000 $ E.try $ QUIC.run testClientConfig $ \conn ->
        E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
            C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
                sendRequest (C.requestNoBody methodGet "/" []) $ \rsp -> do
                    let drain acc = do
                            bs <- C.getResponseBodyChunk rsp
                            if B.null bs then return acc else drain (acc <> bs)
                    body <- drain ""
                    return (C.responseStatus rsp, body)
    early frames conf =
        conf
            { confHooks =
                (confHooks conf){onHeadersFrameCreated = (frames ++)}
            }
    -- Required Insert Count 0 and Base 0, then one field line referring to
    -- the static table: :status 100 is index 63, :status 103 index 24.
    continue = H3Frame H3FrameHeaders "\x00\x00\xff\x00"
    earlyHints = H3Frame H3FrameHeaders "\x00\x00\xd8"
