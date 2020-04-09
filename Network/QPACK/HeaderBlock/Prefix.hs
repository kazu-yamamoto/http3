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
encodeRequiredInsertCount :: Int -> InsertionPoint -> Int
encodeRequiredInsertCount _ 0                       = 0
encodeRequiredInsertCount maxEntries (InsertionPoint reqInsertCount) =
    (reqInsertCount `mod` (2 * maxEntries)) + 1

-- | for decoder
--
-- >>> decodeRequiredInsertCount 3 10 4
-- InsertionPoint 9
-- >>> decodeRequiredInsertCount 128 990 233
-- InsertionPoint 1000
decodeRequiredInsertCount :: Int -> InsertionPoint -> Int -> InsertionPoint
decodeRequiredInsertCount _ _ 0 = 0
decodeRequiredInsertCount maxEntries (InsertionPoint totalNumberOfInserts) encodedInsertCount
  | encodedInsertCount > fullRange = error "decodeRequiredInsertCount"
  | reqInsertCount > maxValue && reqInsertCount <= fullRange = error "decodeRequiredInsertCount"
  | reqInsertCount > maxValue = InsertionPoint (reqInsertCount - fullRange)
  | otherwise                 = InsertionPoint reqInsertCount
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
encodeBase :: InsertionPoint -> BasePoint -> (Bool, Int)
encodeBase (InsertionPoint reqInsCnt) (BasePoint base)
  | diff >= 0 = (False, diff)            -- base - reqInsCnt
  | otherwise = (True,  negate diff - 1) -- reqInsCnt - base - 1
  where
    diff = base - reqInsCnt

-- |
-- >>> decodeBase 6 False 3
-- BasePoint 9
-- >>> decodeBase 9 True 2
-- BasePoint 6
decodeBase :: InsertionPoint -> Bool -> Int -> BasePoint
decodeBase (InsertionPoint reqInsCnt) False deltaBase = BasePoint (reqInsCnt + deltaBase)
decodeBase (InsertionPoint reqInsCnt) True  deltaBase = BasePoint (reqInsCnt - deltaBase - 1)

----------------------------------------------------------------

-- | Encoding the prefix part of header block.
--   This should be used after 'encodeTokenHeader'.
encodePrefix :: WriteBuffer -> DynamicTable -> IO ()
encodePrefix wbuf dyntbl = do
    maxEntries <- getMaxNumOfEntries dyntbl
    baseIndex  <- getBasePoint dyntbl
    reqInsCnt  <- getLargestReference dyntbl
    -- Required Insert Count
    let ric = encodeRequiredInsertCount maxEntries reqInsCnt
    encodeI wbuf set0 8 ric
    -- Sign bit + Delta Base (7+)
    let (s, base) = encodeBase reqInsCnt baseIndex
        set | s         = set1
            | otherwise = set0
    encodeI wbuf set 7 base

decodePrefix :: ReadBuffer -> DynamicTable -> IO (InsertionPoint, BasePoint)
decodePrefix rbuf dyntbl = do
    maxEntries <- getMaxNumOfEntries dyntbl
    totalNumberOfInserts <- getInsertionPoint dyntbl
    w8 <- read8 rbuf
    ric <- decodeI 8 w8 rbuf
    let reqInsCnt = decodeRequiredInsertCount maxEntries totalNumberOfInserts ric
    w8' <- read8 rbuf
    let s = w8' `testBit` 7
        w8'' = w8' .&. 0b01111111
    delta <- decodeI 8 w8'' rbuf
    let baseIndex = decodeBase reqInsCnt s delta
    return (reqInsCnt,baseIndex)
