module QPACK.TableSpec where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Network.QPACK.HeaderBlock.Prefix
import Network.QPACK.Types

spec :: Spec
spec = do
    describe "encodeRequiredInsertCount and decodeRequiredInsertCount" $ do
        prop "duality" $ \(Triple m ei di) -> do
            let ereq = encodeRequiredInsertCount m (InsertionPoint ei)
                InsertionPoint ei' = decodeRequiredInsertCount m (InsertionPoint di) ereq
            ei' `shouldBe` ei
    describe "encodeBase and decodeBase" $ do
        prop "duality" $ \(Doubl base reqInsCnt) -> do
            let (s,delta) = encodeBase (InsertionPoint reqInsCnt) (BasePoint base)
                BasePoint base' = decodeBase (InsertionPoint reqInsCnt) s delta
            base' `shouldBe` base

data Doubl = Doubl Int Int deriving (Eq, Show)

instance Arbitrary Doubl where
    arbitrary = do
        x <- arbitrary `suchThat` (>= 1)
        y <- arbitrary `suchThat` (>= 1)
        return $ Doubl x y

data Triple = Triple Int Int Int deriving (Eq, Show)

instance Arbitrary Triple where
    arbitrary = do
        m  <- arbitrary `suchThat` (>= 3)
        ei <- arbitrary `suchThat` (>= 0)
        di <- arbitrary `suchThat` (\n -> ei - m <= n  && n <= ei)
        return $ Triple m ei di
