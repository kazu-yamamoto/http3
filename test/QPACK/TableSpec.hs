{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QPACK.TableSpec where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forM_, replicateM_)
import qualified Data.ByteString as BS
import Network.HPACK.Internal (toEntryToken)
import Network.HPACK.Token (toToken)
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

    describe "the decoder's table" $ do
        -- An entry is 32 octets more than its name and value: "x" and n
        -- octets of value come to n + 33.
        let entry n = toEntryToken (toToken "x") (BS.replicate n 0x61)
            -- MaxEntries from an announced capacity of 4096, and the
            -- capacity the encoder set.
            newTable cap = do
                dyntbl <- newDynamicTableForDecoding 2048 (\_ -> return ())
                setMaxEntries dyntbl 4096
                setDecoderTableCapacity dyntbl cap
                return dyntbl
            insert dyntbl n = atomically $ insertEntryToDecoder (entry n) dyntbl
            present dyntbl i = do
                r <- E.try $ atomically (toDynamicEntry dyntbl (AbsoluteIndex i)) >>= E.evaluate
                return $ either (\(_ :: E.SomeException) -> False) (const True) r
        it "refuses an entry larger than the capacity" $ do
            dyntbl <- newTable 100
            insert dyntbl 68 `shouldThrow` (== EncoderInstructionError)
            -- One that fits exactly is taken.
            _ <- insert dyntbl 67
            getTableCapacity dyntbl `shouldReturn` 100
        it "evicts the oldest entries to make room" $ do
            dyntbl <- newTable 100
            -- 60 octets each: only one fits.
            replicateM_ 2 $ insert dyntbl 27
            present dyntbl 0 `shouldReturn` False
            present dyntbl 1 `shouldReturn` True
            getTableCapacity dyntbl `shouldReturn` 60
        it "evicts what no longer fits when the capacity is lowered" $ do
            dyntbl <- newTable 4096
            replicateM_ 3 $ insert dyntbl 27
            setDecoderTableCapacity dyntbl 100
            mapM (present dyntbl) [0, 1, 2] `shouldReturn` [False, False, True]
            getTableCapacity dyntbl `shouldReturn` 60
        it "holds no more than the capacity whatever is inserted" $ do
            -- A slot for each 32 octets of 4096, and entries of 4000 octets:
            -- the ring took 128 of them, half a megabyte.
            dyntbl <- newTable 4096
            forM_ [1 .. 200 :: Int] $ \_ -> insert dyntbl 4000
            getTableCapacity dyntbl >>= (`shouldSatisfy` (<= 4096))
            mapM (present dyntbl) [198, 199] `shouldReturn` [False, True]
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
