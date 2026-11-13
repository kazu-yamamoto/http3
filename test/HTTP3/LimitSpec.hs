{-# LANGUAGE OverloadedStrings #-}

module HTTP3.LimitSpec where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import Data.Bits (shiftR, (.&.), (.|.))
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Internal (H3Frame (..), H3FrameType (..))
import Network.HTTP3.Server
import Network.QPACK (QDecoderConfig (..))
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
spec = do
    describe "H3 server keeping to a limit it does not announce" $ do
        it "answers 431 to a request header section over its limit" $
            E.bracket (setupWith hideLimit server 4096) teardown $
                \_ -> runTooLargeClient
        it "answers 431 to a HEADERS frame longer than its limit" $
            E.bracket (setupWith hideLimit server 4096) teardown $
                \_ -> runTooLongFrameClient
    describe "H3 server with a small field section limit" $
        it "takes SETTINGS longer than that limit" $
            E.bracket (setupWith smallLimit server 4096) teardown $
                \_ -> runLargeSettingsClient

-- | A server taking field sections of 300 octets at most, and a client whose
-- SETTINGS come to 450 octets or so.  The cap on control frames was the
-- field section limit, and the server closed the connection with
-- H3_EXCESSIVE_LOAD.
runLargeSettingsClient :: IO ()
runLargeSettingsClient = QUIC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf0 -> do
        let conf =
                conf0
                    { confHooks =
                        (confHooks conf0){onControlFrameCreated = map grease}
                    }
        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
            let hello =
                    sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                        C.responseStatus rsp `shouldBe` Just ok200
            -- The first is answered before the server reads our SETTINGS.
            hello
            waitForSettings
            hello
  where
    -- 150 reserved settings (RFC 9114, section 7.2.4.1), of two-octet IDs
    -- and the value 0.
    grease (H3Frame H3FrameSettings payload) =
        H3Frame H3FrameSettings $ payload <> B.concat (map reserved [0 .. 149])
    grease frame = frame
    reserved :: Int -> B.ByteString
    reserved n =
        let i = 0x1f * n + 0x21
         in B.pack [fromIntegral (0x40 .|. (i `shiftR` 8)), fromIntegral (i .&. 0xff), 0]

smallLimit :: Config -> Config
smallLimit conf =
    conf
        { confQDecoderConfig =
            (confQDecoderConfig conf){dcMaxFieldSectionSize = 300}
        }

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
            waitForSettings
            let hdr = replicate 150 ("x-a", B.replicate 300 0x62)
                req = C.requestNoBody methodGet "/" hdr
            sendRequest req $ \rsp ->
                C.responseStatus rsp `shouldBe` Just requestHeaderFieldsTooLarge431

-- | One field with a 40000-octet value of "~", which Huffman makes no shorter: the
-- HEADERS frame itself is longer than the server's 32768.  The section was
-- refused with the 431 above only once decoded, and a frame this long closed
-- the whole connection with H3_EXCESSIVE_LOAD instead.
runTooLongFrameClient :: IO ()
runTooLongFrameClient = QUIC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
            let req = C.requestNoBody methodGet "/" [("x-a", B.replicate 40000 0x7e)]
            sendRequest req $ \rsp ->
                C.responseStatus rsp `shouldBe` Just requestHeaderFieldsTooLarge431
