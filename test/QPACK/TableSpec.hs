module QPACK.TableSpec where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Network.QPACK.HeaderBlock

spec :: Spec
spec = do
    describe "encodeRequiredInsertCount and decodeRequiredInsertCount" $ do
        prop "duality" $ \(Triple m ei di) -> do
            let ereq = encodeRequiredInsertCount m ei
                ei' = decodeRequiredInsertCount m di ereq
            ei' `shouldBe` ei

data Triple = Triple Int Int Int deriving (Eq, Show)

instance Arbitrary Triple where
    arbitrary = do
        m  <- arbitrary `suchThat` (>= 3)
        ei <- arbitrary `suchThat` (>= 0)
        di <- arbitrary `suchThat` (\n -> ei - m <= n  && n <= ei)
        return $ Triple m ei di
