{-# LANGUAGE OverloadedStrings #-}

module QPACK.InstructionSpec where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Network.HTTP.Semantics
import Network.QPACK.Internal
import Test.Hspec

spec :: Spec
spec = do
    describe "encodeEncoderInstructions and decodeEncoderInstructions" $ do
        it "encode/decodes encode instructions properly" $ do
            let eis0 =
                    [ SetDynamicTableCapacity 4096
                    , InsertWithNameReference (Left 92) "Warp/4.3.2.1"
                    , InsertWithLiteralName tokenContentType "text/plain"
                    , Duplicate 40
                    ]
            bs1 <- encodeEncoderInstructions eis0 True
            (eis1, "") <- decodeEncoderInstructions' bs1
            eis1 `shouldBe` eis0
            bs2 <- encodeEncoderInstructions eis0 False
            (eis2, "") <- decodeEncoderInstructions' bs2
            eis2 `shouldBe` eis0
    describe "encoderInstructionLength" $ do
        it "tells the length of the first instruction once it can" $ do
            let eis =
                    [ SetDynamicTableCapacity 4096
                    , SetDynamicTableCapacity 1000000
                    , InsertWithNameReference (Left 92) "Warp/4.3.2.1"
                    , InsertWithNameReference (Right 1000) (BS.replicate 300 0xff)
                    , InsertWithLiteralName tokenContentType "text/plain"
                    , InsertWithLiteralName tokenContentType (BS.replicate 3000 0x61)
                    , Duplicate 40
                    , Duplicate 100000
                    ]
            forM_ [True, False] $ \huff ->
                forM_ eis $ \ei -> do
                    e <- encodeEncoderInstructions [ei] huff
                    next <- encodeEncoderInstructions [Duplicate 1] huff
                    let len = BS.length e
                    encoderInstructionLength e `shouldBe` Just len
                    encoderInstructionLength (e <> next) `shouldBe` Just len
                    -- Short of it: not yet known, or known right.
                    forM_ [0 .. len - 1] $ \k ->
                        encoderInstructionLength (BS.take k e)
                            `shouldSatisfy` maybe True (== len)
    describe "encodeDecoderInstructions and decodeDecoderInstructions" $ do
        it "encode/decodes decode instructions properly" $ do
            let eis0 =
                    [ SectionAcknowledgement 10
                    , StreamCancellation 100
                    , InsertCountIncrement 200
                    ]
            bs1 <- encodeDecoderInstructions eis0
            (eis1, "") <- decodeDecoderInstructions bs1
            eis1 `shouldBe` eis0
