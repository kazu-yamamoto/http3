{-# LANGUAGE OverloadedStrings #-}

module QPACK.InstructionSpec where

import Data.ByteString ()
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
                    , InsertWithoutNameReference tokenContentType "text/plain"
                    , Duplicate 40
                    ]
            bs1 <- encodeEncoderInstructions eis0 True
            (eis1, "") <- decodeEncoderInstructions' bs1
            eis1 `shouldBe` eis0
            bs2 <- encodeEncoderInstructions eis0 False
            (eis2, "") <- decodeEncoderInstructions' bs2
            eis2 `shouldBe` eis0
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
