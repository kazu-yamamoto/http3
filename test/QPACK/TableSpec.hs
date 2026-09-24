module QPACK.TableSpec where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Network.QPACK.Internal
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
    describe "toIndexedEntry" $ do
        it "refuses a static index outside the table" $ do
            dyntbl <- newDynamicTableForDecoding 2048 (\_ -> return ())
            -- Entry has no Eq, so keep only whether it came back at all.
            let look i = do
                    r <-
                        E.try $
                            atomically (toIndexedEntry dyntbl (SIndex (AbsoluteIndex i)))
                                >>= E.evaluate
                    return $ either Left (const (Right ())) r
            -- Past the end was already refused; below the start was not, and
            -- the read behind it is unchecked.
            look 1000 `shouldReturn` Left (IllegalStaticIndex 1000)
            look (-1) `shouldReturn` Left (IllegalStaticIndex (-1))

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
