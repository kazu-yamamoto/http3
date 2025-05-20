{-# LANGUAGE BinaryLiterals #-}

module Network.QPACK.HeaderBlock.Prefix (
    -- * Prefix
    encodePrefix,
    decodePrefix,
    encodeRequiredInsertCount,
    decodeRequiredInsertCount,
    encodeBase,
    decodeBase,
) where

import qualified Control.Exception as E
import Network.ByteOrder
import Network.HPACK.Internal (decodeI, encodeI)

import Imports
import Network.QPACK.Error
import Network.QPACK.Table
import Network.QPACK.Types

----------------------------------------------------------------

-- |
-- >>> encodeRequiredInsertCount 3 9
-- 4
-- >>> encodeRequiredInsertCount 128 1000
-- 233
encodeRequiredInsertCount :: Int -> RequiredInsertCount -> Int
encodeRequiredInsertCount _ 0 = 0
encodeRequiredInsertCount maxEntries (RequiredInsertCount reqInsertCount) =
    (reqInsertCount `mod` (2 * maxEntries)) + 1

-- | for decoder
--
-- >>> decodeRequiredInsertCount 3 10 4
-- RequiredInsertCount 9
-- >>> decodeRequiredInsertCount 128 990 233
-- RequiredInsertCount 1000
decodeRequiredInsertCount
    :: Int -> InsertionPoint -> Int -> RequiredInsertCount
decodeRequiredInsertCount _ _ 0 = 0
decodeRequiredInsertCount 0 _ n = RequiredInsertCount (n - 1)
decodeRequiredInsertCount maxEntries (InsertionPoint totalNumberOfInserts) encodedInsertCount
    | encodedInsertCount > fullRange = E.throw IllegalInsertCount
    | reqInsertCount > maxValue && reqInsertCount <= fullRange =
        E.throw IllegalInsertCount
    | reqInsertCount > maxValue = RequiredInsertCount (reqInsertCount - fullRange)
    | otherwise = RequiredInsertCount reqInsertCount
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
encodeBase :: RequiredInsertCount -> BasePoint -> (Bool, Int)
encodeBase (RequiredInsertCount reqInsCnt) (BasePoint base)
    | diff >= 0 = (False, diff) -- base - reqInsCnt
    | otherwise = (True, negate diff - 1) -- reqInsCnt - base - 1
  where
    diff = base - reqInsCnt

-- |
-- >>> decodeBase 6 False 3
-- BasePoint 9
-- >>> decodeBase 9 True 2
-- BasePoint 6
decodeBase :: RequiredInsertCount -> Bool -> Int -> BasePoint
decodeBase (RequiredInsertCount reqInsCnt) False deltaBase = BasePoint (reqInsCnt + deltaBase)
decodeBase (RequiredInsertCount reqInsCnt) True deltaBase = BasePoint (reqInsCnt - deltaBase - 1)

----------------------------------------------------------------

-- | Encoding the prefix part of header block.
--   This should be used after 'encodeTokenHeader'.
encodePrefix :: WriteBuffer -> DynamicTable -> IO ()
encodePrefix wbuf dyntbl = do
    clearWriteBuffer wbuf
    maxEntries <- getMaxNumOfEntries dyntbl
    baseIndex <- getBasePoint dyntbl
    reqInsCnt <- getRequiredInsertCount dyntbl
    qpackDebug dyntbl $ print reqInsCnt
    -- Required Insert Count
    let ric = encodeRequiredInsertCount maxEntries reqInsCnt
    encodeI wbuf set0 8 ric
    -- Sign bit + Delta Base (7+)
    let (s, base) = encodeBase reqInsCnt baseIndex
        set
            | s = set1
            | otherwise = set0
    encodeI wbuf set 7 base

-- | Decoding the prefix part of header block.
decodePrefix
    :: ReadBuffer -> DynamicTable -> IO (RequiredInsertCount, BasePoint, Bool)
decodePrefix rbuf dyntbl = do
    maxEntries <- getMaxNumOfEntries dyntbl
    totalNumberOfInserts <- getInsertionPoint dyntbl
    w8 <- read8 rbuf
    ric <- decodeI 8 w8 rbuf
    let reqInsCnt = decodeRequiredInsertCount maxEntries totalNumberOfInserts ric
    w8' <- read8 rbuf
    let s = w8' `testBit` 7
        w8'' = w8' .&. 0b01111111
    delta <- decodeI 7 w8'' rbuf
    let baseIndex = decodeBase reqInsCnt s delta
    qpackDebug dyntbl $
        putStrLn $
            show reqInsCnt
                ++ ", "
                ++ show baseIndex
                ++ ", "
                ++ show totalNumberOfInserts
                ++ ", maxN "
                ++ show maxEntries
    let needAck = reqInsCnt /= 0
    return (reqInsCnt, baseIndex, needAck)
