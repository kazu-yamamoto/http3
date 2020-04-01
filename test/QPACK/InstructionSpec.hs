{-# LANGUAGE OverloadedStrings #-}

module QPACK.InstructionSpec where

import Data.ByteString ()
import Network.HPACK.Token
import Test.Hspec

import Network.QPACK.Instruction

spec :: Spec
spec = do
    describe "encodeEncoderInstructions and decodeEncoderInstructions" $ do
        it "encode/decodes encode instructions properly" $ do
            let eis0 = [SetDynamicTableCapacity 4096
                       ,InsertWithNameReference (SIndex 92) "Warp/4.3.2.1"
                       ,InsertWithoutNameReference tokenContentType "text/plain"
                       ,Duplicate 40
                       ]
            bs1  <- encodeEncoderInstructions eis0 True
            eis1 <- decodeEncoderInstructions bs1
            eis1 `shouldBe` eis0
            bs2  <- encodeEncoderInstructions eis0 False
            eis2 <- decodeEncoderInstructions bs2
            eis2 `shouldBe` eis0
