module QPACK.TableSpec where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Network.QPACK.HeaderBlock

spec :: Spec
spec = do
    describe "encodeInsertCount and decodeInsertCount" $ do
        prop "duality" $ \(Triple m ei di) -> do
            let ereq = encodeInsertCount m ei
                ei' = decodeInsertCount m di ereq
            ei' `shouldBe` ei

data Triple = Triple Int Int Int deriving (Eq, Show)

instance Arbitrary Triple where
    arbitrary = do
        m  <- arbitrary `suchThat` (>= 3)
        ei <- arbitrary `suchThat` (>= 0)
        di <- arbitrary `suchThat` (\n -> ei - m <= n  && n <= ei)
        return $ Triple m ei di
