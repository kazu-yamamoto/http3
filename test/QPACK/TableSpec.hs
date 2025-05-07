module QPACK.TableSpec where

import Network.QPACK.Internal
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
    describe "encodeRequiredInsertCount and decodeRequiredInsertCount" $ do
        prop "duality" $ \(Triple m ei di) -> do
            let ereq = encodeRequiredInsertCount m (RequiredInsertCount ei)
                RequiredInsertCount ei' = decodeRequiredInsertCount m (InsertionPoint di) ereq
            ei' `shouldBe` ei
    describe "encodeBase and decodeBase" $ do
        prop "duality" $ \(Doubl base reqInsCnt) -> do
            let (s, delta) = encodeBase (RequiredInsertCount reqInsCnt) (BasePoint base)
                BasePoint base' = decodeBase (RequiredInsertCount reqInsCnt) s delta
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
        m <- arbitrary `suchThat` (>= 3)
        ei <- arbitrary `suchThat` (>= 0)
        di <- arbitrary `suchThat` (\n -> ei - m <= n && n <= ei)
        return $ Triple m ei di
