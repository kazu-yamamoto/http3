{-# LANGUAGE BinaryLiterals #-}

module Network.QPACK.HeaderBlock (
    encodeHeader
  , encodePrefix
  , encodeRequiredInsertCount
  , decodeRequiredInsertCount
  , decodeBase
  ) where

import Data.IORef
import Network.ByteOrder
import Network.HPACK.Internal

import Imports

data DynamicTable
data StaticTable
data ControlBuffer

-- |
-- >>> encodeRequiredInsertCount 3 9
-- 4
-- >>> encodeRequiredInsertCount 128 1000
-- 233
encodeRequiredInsertCount :: Int -> Int -> Int
encodeRequiredInsertCount _ 0                       = 0
encodeRequiredInsertCount maxEntries reqInsertCount = (reqInsertCount `mod` (2 * maxEntries)) + 1

-- | for decoder
--
-- >>> decodeRequiredInsertCount 3 10 4
-- 9
-- >>> decodeRequiredInsertCount 128 990 233
-- 1000
decodeRequiredInsertCount :: Int -> Int -> Int -> Int
decodeRequiredInsertCount _ _ 0 = 0
decodeRequiredInsertCount maxEntries totalNumberOfInserts encodedInsertCount
  | encodedInsertCount > fullRange = error "decodeRequiredInsertCount"
  | reqInsertCount > maxValue && reqInsertCount <= fullRange = error "decodeRequiredInsertCount"
  | reqInsertCount > maxValue = reqInsertCount - fullRange
  | otherwise                 = reqInsertCount
  where
    fullRange = 2 * maxEntries
    maxValue = totalNumberOfInserts + maxEntries
    maxWrapped = (maxValue `div` fullRange) * fullRange
    reqInsertCount = maxWrapped + encodedInsertCount - 1

-- |
-- >>> decodeBase 1 9 2
-- 6
decodeBase :: Int -> Int -> Int -> Int
decodeBase 0 reqInsertCount deltaBase = reqInsertCount + deltaBase
decodeBase _ reqInsertCount deltaBase = reqInsertCount - deltaBase - 1

-- dynamicTable
getBaseIndex :: DynamicTable -> IO Index
getBaseIndex = undefined
add :: DynamicTable -> Header -> IO ()
add = undefined
toAbsolute :: DynamicTable -> Index -> Index
toAbsolute = undefined

staticTable :: StaticTable
staticTable = undefined
size :: StaticTable -> Int
size = undefined

getIndex :: a -> Header -> IO (Maybe Index)
getIndex = undefined
getNameIndex :: Header -> IO Int
getNameIndex = undefined

shouldIndex :: Header -> Bool
shouldIndex = undefined
canIndex :: DynamicTable -> Header -> Bool
canIndex = undefined

streamBuffer :: Buffer
streamBuffer = undefined
controlBuffer :: ControlBuffer
controlBuffer = undefined
prefixBuffer :: WriteBuffer
prefixBuffer = undefined

-- 4.5.2.  Indexed Header Field
encodeIndexReference :: Buffer -> Index -> IO ()
encodeIndexReference = undefined

-- 4.5.3.  Indexed Header Field With Post-Base Index
encodeDynamicIndexReference :: Buffer -> Index -> Index -> IO ()
encodeDynamicIndexReference = undefined

-- 4.5.4.  Literal Header Field With Name Reference
encodeLiteralWithIncrementalIndex :: ControlBuffer -> Index -> Header -> IO ()
encodeLiteralWithIncrementalIndex = undefined

-- 4.5.5.  Literal Header Field With Post-Base Name Reference
encodeDynamicLiteral :: Buffer -> Index -> Index -> Header -> IO ()
encodeDynamicLiteral = undefined

-- 4.5.6.  Literal Header Field Without Name Reference
encodeLiteral :: Buffer -> Index -> Header -> IO ()
encodeLiteral = undefined

encodeHeader :: DynamicTable -> [Header] -> IO ()
encodeHeader dynamicTable headers = do
    baseIndex <- getBaseIndex dynamicTable
    ref <- newIORef 0 :: IO (IORef Int)
    mapM_ (encode baseIndex ref dynamicTable) headers
    largestReference <- readIORef ref
    encodePrefix largestReference baseIndex

encode :: Index -> IORef Int -> DynamicTable -> Header -> IO ()
encode baseIndex ref dynamicTable header = do
    mStaticIdx <- getIndex staticTable header
    case mStaticIdx of
      Just staticIdx -> encodeIndexReference streamBuffer staticIdx
      Nothing        -> dynamicOrLiteral baseIndex ref dynamicTable header

dynamicOrLiteral :: Index -> IORef Index -> DynamicTable -> Header -> IO ()
dynamicOrLiteral baseIndex ref dynamicTable header = do
    nameIdx <- getNameIndex header
    mDynamicIdx0 <- getIndex dynamicTable header
    mDynamicIdx <- case mDynamicIdx0 of
      Nothing
        | shouldIndex header && canIndex dynamicTable header -> do
          -- No matching entry.  Either insert+index or encode literal
              encodeLiteralWithIncrementalIndex controlBuffer nameIdx header
              add dynamicTable header
              getIndex dynamicTable header
        | otherwise -> return Nothing
      md            -> return md
    dynamicOrLiteral' nameIdx mDynamicIdx
  where
    dynamicOrLiteral' nameIdx Nothing
      | nameIdx <= size staticTable = encodeLiteral streamBuffer nameIdx header
      | otherwise                   = do
            -- encode literal, possibly with nameIdx above baseIndex
            encodeDynamicLiteral streamBuffer nameIdx baseIndex header
            largestReference <- readIORef ref
            writeIORef ref $ max largestReference $ toAbsolute dynamicTable nameIdx
    dynamicOrLiteral' _ (Just dynamicIdx) = do
        largestReference <- readIORef ref
        writeIORef ref $ max largestReference dynamicIdx
        -- Encode dynamicIdx, possibly with dynamicIdx above baseIndex
        encodeDynamicIndexReference streamBuffer dynamicIdx baseIndex

encodePrefix :: Index -> Index -> IO ()
encodePrefix largestReference baseIndex = do
    -- Required Insert Count
    encodeI prefixBuffer set0 8 largestReference
    -- Sign bit + Delta Base (7+)
    if baseIndex >= largestReference then
        encodeI prefixBuffer set0 7 (baseIndex - largestReference)
      else
        encodeI prefixBuffer set1 7 (largestReference - baseIndex)

type Setter = Word8 -> Word8

set1, set0 :: Setter
set1    x = x `setBit` 7
set0    = id

-- nameIndex?
