{-# LANGUAGE BinaryLiterals #-}

module Network.QPACK.HeaderBlock.Prefix (
    encodePrefix
  , decodePrefix
  , encodeRequiredInsertCount
  , decodeRequiredInsertCount
  , encodeBase
  , decodeBase
  ) where

import Network.ByteOrder
import Network.HPACK.Internal

import Imports
import Network.QPACK.Table
import Network.QPACK.Types

----------------------------------------------------------------

-- |
-- >>> encodeRequiredInsertCount 3 9
-- 4
-- >>> encodeRequiredInsertCount 128 1000
-- 233
encodeRequiredInsertCount :: Int -> AbsoluteIndex -> Int
encodeRequiredInsertCount _ 0                       = 0
encodeRequiredInsertCount maxEntries (AbsoluteIndex reqInsertCount) =
    (reqInsertCount `mod` (2 * maxEntries)) + 1

-- | for decoder
--
-- >>> decodeRequiredInsertCount 3 10 4
-- AbsoluteIndex 9
-- >>> decodeRequiredInsertCount 128 990 233
-- AbsoluteIndex 1000
decodeRequiredInsertCount :: Int -> AbsoluteIndex -> Int -> AbsoluteIndex
decodeRequiredInsertCount _ _ 0 = 0
decodeRequiredInsertCount maxEntries (AbsoluteIndex totalNumberOfInserts) encodedInsertCount
  | encodedInsertCount > fullRange = error "decodeRequiredInsertCount"
  | reqInsertCount > maxValue && reqInsertCount <= fullRange = error "decodeRequiredInsertCount"
  | reqInsertCount > maxValue = AbsoluteIndex (reqInsertCount - fullRange)
  | otherwise                 = AbsoluteIndex reqInsertCount
  where
    fullRange = 2 * maxEntries
    maxValue = totalNumberOfInserts + maxEntries
    maxWrapped = (maxValue `div` fullRange) * fullRange
    reqInsertCount = maxWrapped + encodedInsertCount - 1

----------------------------------------------------------------

-- |
-- >>> encodeBase 6 9
-- (False,3)
-- >>> encodeBase 9 6
-- (True,2)
encodeBase :: AbsoluteIndex -> AbsoluteIndex -> (Bool, Int)
encodeBase (AbsoluteIndex reqInsCnt) (AbsoluteIndex base)
  | diff >= 0 = (False, diff)            -- base - reqInsCnt
  | otherwise = (True,  negate diff - 1) -- reqInsCnt - base - 1
  where
    diff = base - reqInsCnt

-- |
-- >>> decodeBase 6 False 3
-- AbsoluteIndex 9
-- >>> decodeBase 9 True 2
-- AbsoluteIndex 6
decodeBase :: AbsoluteIndex -> Bool -> Int -> AbsoluteIndex
decodeBase (AbsoluteIndex reqInsCnt) False deltaBase = AbsoluteIndex (reqInsCnt + deltaBase)
decodeBase (AbsoluteIndex reqInsCnt) True  deltaBase = AbsoluteIndex (reqInsCnt - deltaBase - 1)

----------------------------------------------------------------

encodePrefix :: DynamicTable -> WriteBuffer -> AbsoluteIndex -> AbsoluteIndex -> IO ()
encodePrefix dyntbl wbuf reqInsCnt baseIndex = do
    maxEntries <- getMaxNumOfEntries dyntbl
    -- Required Insert Count
    let ric = encodeRequiredInsertCount maxEntries reqInsCnt
    encodeI wbuf set0 8 ric
    -- Sign bit + Delta Base (7+)
    let (s, base) = encodeBase reqInsCnt baseIndex
        set | s         = set1
            | otherwise = set0
    encodeI wbuf set 7 base

decodePrefix :: DynamicTable -> ReadBuffer -> IO (AbsoluteIndex, AbsoluteIndex)
decodePrefix dyntbl rbuf = do
    maxEntries <- getMaxNumOfEntries dyntbl
    totalNumberOfInserts <- getTotalNumOfEntries dyntbl
    w8 <- read8 rbuf
    ric <- decodeI 8 w8 rbuf
    let reqInsCnt = decodeRequiredInsertCount maxEntries totalNumberOfInserts ric
    w8' <- read8 rbuf
    let s = w8' `testBit` 7
        w8'' = w8' .&. 0b01111111
    delta <- decodeI 8 w8'' rbuf
    let baseIndex = decodeBase reqInsCnt s delta
    return (reqInsCnt,baseIndex)

----------------------------------------------------------------

type Setter = Word8 -> Word8

set1, set0 :: Setter
set1    x = x `setBit` 7
set0    = id
