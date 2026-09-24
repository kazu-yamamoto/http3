{-# LANGUAGE OverloadedStrings #-}

module QPACK.HeaderBlockSpec where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Network.HPACK.Token (toToken)
import Network.QPACK
import Test.Hspec

spec :: Spec
spec = do
    describe "field section round trip" $ do
        it "decodes a header value larger than the decoder's scratch buffer" $ do
            -- The scratch buffer for Huffman decoding used to be a fixed 2048
            -- octets, so a value above that failed to decode however small the
            -- section carrying it was: 2100 octets here comes to well under
            -- 1.4K encoded, far inside the SETTINGS_MAX_FIELD_SECTION_SIZE we
            -- advertise.
            mapM_ roundTrip [1000, 2100, 8000, 20000]

roundTrip :: Int -> IO ()
roundTrip n = do
    (enc, _, _) <-
        newQEncoder
            defaultQEncoderConfig{ecHeaderBlockBufferSize = 262144}
            (\_ -> return ())
    (dec, _) <- newQDecoder defaultQDecoderConfig (\_ -> return ())
    let val = C8.replicate n 'a'
    blk <- enc 0 [(toToken ":status", "200"), (toToken "x-big", val)]
    (ths, _) <- dec 0 blk
    -- The encoded section stays well under the announced limit; it is only
    -- the decoded value that is large.
    BS.length blk `shouldSatisfy` (< dcMaxFieldSectionSize defaultQDecoderConfig)
    lookup (toToken "x-big") ths `shouldBe` Just val
