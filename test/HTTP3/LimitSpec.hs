{-# LANGUAGE OverloadedStrings #-}

module HTTP3.LimitSpec where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Internal (H3Frame (..), H3FrameType (..))
import Network.HTTP3.Server
import qualified Network.QUIC.Client as QUIC
import Test.Hspec

import HTTP3.Config
import HTTP3.Server

-- | A server that keeps to its SETTINGS_MAX_FIELD_SECTION_SIZE without
-- announcing it.
--
-- Our client keeps to the limit a server announces and sends nothing over
-- it, so a server that announces its limit never sees a section over it from
-- us.  Such a section has to be sent all the same to see what the server
-- does with it, and a client that has not been told of a limit sends it.
spec :: Spec
spec =
    describe "H3 server keeping to a limit it does not announce" $
        it "answers 431 to a request header section over its limit" $
            E.bracket (setupWith hideLimit server 4096) teardown $
                \_ -> runTooLargeClient

-- | SETTINGS with the dynamic table as the test server has it, and without
-- SETTINGS_MAX_FIELD_SECTION_SIZE: QPACK_MAX_TABLE_CAPACITY 4096 and
-- QPACK_BLOCKED_STREAMS 100.
hideLimit :: Config -> Config
hideLimit conf = conf{confHooks = (confHooks conf){onControlFrameCreated = map replace}}
  where
    replace (H3Frame H3FrameSettings _) =
        H3Frame H3FrameSettings "\x01\x50\x00\x07\x40\x64"
    replace frame = frame

-- | 150 copies of one field with a 300-octet value.  After the first two,
-- each is a reference to the dynamic table of an octet or so, so the HEADERS
-- frame comes to around 600 octets, well within the cap on its length.  As
-- the limit counts them, though, they are 335 octets each, 50250 in all
-- against the server's 32768.
runTooLargeClient :: IO ()
runTooLargeClient = QUIC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
            -- One round trip first, so that the server's SETTINGS are in and
            -- the encoder may use the dynamic table.  Without it every copy
            -- goes as a literal and the frame is over the cap.
            sendRequest (C.requestNoBody methodGet "/" []) $ \_ -> return ()
            let hdr = replicate 150 ("x-a", B.replicate 300 0x62)
                req = C.requestNoBody methodGet "/" hdr
            sendRequest req $ \rsp ->
                C.responseStatus rsp `shouldBe` Just requestHeaderFieldsTooLarge431
