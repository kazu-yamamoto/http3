{-# LANGUAGE BinaryLiterals #-}

module Network.QPACK.HeaderBlock where

import Data.Bits
import Data.CaseInsensitive
import Data.IORef
import Network.ByteOrder
import Network.HPACK.Internal
import Network.HPACK.Token

data DynamicTable
data StaticTable
data ControlBuffer

{-
    Dropping    Draining Index               Insertion Point
      |          |                                 |
      v          v                                 v
      +----------+---------------------------------+--------+
      | Draining |          Referenceable          | Unused |
      | Entries  |             Entries             | Space  |
      +----------+---------------------------------+--------+
|  d  |                            |n-4|n-3|n-2|n-1|          Absolute
|n-d-1|                            | 3 | 2 | 1 | 0 |          Relative ins
|n-d-3|                            | 1 | 0 |                  Relative HB
                                           | 0 | 1 |          Post-Base
                                           ^
                                           |
                                          Base = n - 2
-}

maxTableCapacity :: Int
maxTableCapacity = 100

maxEntries :: Int
maxEntries = maxTableCapacity `div` 32

-- |
-- >>> encInsertCount 9
-- 4
encInsertCount :: Int -> Int
encInsertCount 0              = 0
encInsertCount reqInsertCount = (reqInsertCount `mod` (2 * maxEntries)) + 1

-- | for decoder
--
-- >>> requiredInsertCount 4 10
-- 9
requiredInsertCount :: Int -> Int -> Int
requiredInsertCount 0 _ = 0
requiredInsertCount encodedInsertCount totalNumberOfInserts
  | encodedInsertCount > fullRange = error "requiredInsertCount"
  | reqInsertCount > maxValue && reqInsertCount <= fullRange = error "requiredInsertCount"
  | reqInsertCount > maxValue = reqInsertCount - fullRange
  | otherwise                 = reqInsertCount
  where
    fullRange = 2 * maxEntries
    maxValue = totalNumberOfInserts + maxEntries
    maxWrapped = (maxValue `div` fullRange) * fullRange
    reqInsertCount = maxWrapped + encodedInsertCount - 1


-- |
-- >>> base 1 9 2
-- 6
base :: Int -> Int -> Int -> Int
base 0 reqInsertCount deltaBase = reqInsertCount + deltaBase
base _ reqInsertCount deltaBase = reqInsertCount - deltaBase - 1

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

-- Assuming MSBs are 0.
set1, set0, set01, set001, set0000, setH :: Setter
set1    x = x `setBit` 7
set01   x = x `setBit` 6
set001  x = x `setBit` 5
set11   x = x .|. 0b11000000
-- set0001 x = x `setBit` 4 -- Never indexing
set0    = id
set000  = id
set0000 = id
setH = set1

-- nameIndex?

data HIndex = SIndex Int | DIndex Int deriving (Eq, Ord, Show)

data EncoderInstruction = SetDynamicTableCapacity Int
                        | InsertWithNameReference HIndex HeaderValue
                        | InsertWithoutNameReference Token HeaderValue
                        | Duplicate Int -- fixme
                        deriving (Eq, Show)

encodeEI :: WriteBuffer -> Bool -> EncoderInstruction -> IO ()
encodeEI wbuf _    (SetDynamicTableCapacity cap) = encodeI wbuf set001 5 cap
encodeEI wbuf huff (InsertWithNameReference hidx v) = do
    let (set, idx) = case hidx of
          SIndex i -> (set11, i)
          DIndex i -> (set1,  i)
    encodeI wbuf set 6 idx
    encodeS wbuf huff id set1 7 v
encodeEI wbuf huff (InsertWithoutNameReference k v) = do
    encodeS wbuf huff set01 set001 5 $ foldedCase $ tokenKey k
    encodeS wbuf huff id    set1   7 v
encodeEI wbuf _    (Duplicate idx) = encodeI wbuf set000 5 idx

decodeEncoderInstruction :: ByteString -> IO [EncoderInstruction]
decodeEncoderInstruction bs = fmap snd .  withWriteBuffer' 4096 $ \wbuf ->
    withReadBuffer bs $ loop (decodeH wbuf) []
  where
    loop hdec rs rbuf = do
        n <- remainingSize rbuf
        if n == 0 then
            return $ reverse rs
          else do
            r <- decodeEI hdec rbuf
            loop hdec (r:rs) rbuf

decodeEI :: HuffmanDecoder -> ReadBuffer -> IO EncoderInstruction
decodeEI hufdec rbuf = do
    w8 <- read8 rbuf
    if w8 `testBit` 7 then
        decodeInsertWithNameReference rbuf w8 hufdec
      else if w8 `testBit` 6 then
        decodeInsertWithoutNameReference rbuf hufdec
      else if w8 `testBit` 5 then
        decodeSetDynamicTableCapacity rbuf w8
      else
        decodeDuplicate rbuf w8

decodeInsertWithNameReference :: ReadBuffer -> Word8 -> HuffmanDecoder -> IO EncoderInstruction
decodeInsertWithNameReference rbuf w8 hufdec = do
    idx <- decodeI 6 (w8 .&. 0b00111111) rbuf
    let hidx | w8 `testBit` 6 = SIndex idx
             | otherwise      = DIndex idx
    v <- decodeS (.&. 0b01111111) (`testBit` 7) hufdec rbuf
    return $ InsertWithNameReference hidx v

decodeInsertWithoutNameReference :: ReadBuffer -> HuffmanDecoder -> IO EncoderInstruction
decodeInsertWithoutNameReference rbuf hufdec = do
    ff rbuf (-1)
    k <- decodeS (.&. 0b00011111) (`testBit` 5) hufdec rbuf
    v <- decodeS (.&. 0b01111111) (`testBit` 7) hufdec rbuf
    return $ InsertWithoutNameReference (toToken k)  v

decodeSetDynamicTableCapacity :: ReadBuffer -> Word8 -> IO EncoderInstruction
decodeSetDynamicTableCapacity rbuf w8 =
    SetDynamicTableCapacity <$> decodeI 5 (w8 .&. 0b00011111) rbuf

decodeDuplicate :: ReadBuffer -> Word8 -> IO EncoderInstruction
decodeDuplicate rbuf w8 =
    Duplicate <$> decodeI 5 (w8 .&. 0b00011111) rbuf

data DecoderInstruction = HeaderAcknowledgement Int
                        | StreamCancellation Int
                        | InsertCountIncrement Int
                        deriving (Eq, Show)

encodeDI :: WriteBuffer -> DecoderInstruction -> IO ()
encodeDI wbuf (HeaderAcknowledgement n) = encodeI wbuf set1  7 n
encodeDI wbuf (StreamCancellation n)    = encodeI wbuf set01 6 n
encodeDI wbuf (InsertCountIncrement n)  = encodeI wbuf id    6 n

decodeDI :: ReadBuffer -> IO DecoderInstruction
decodeDI rbuf = do
    w8 <- read8 rbuf
    if w8 `testBit` 7 then
        HeaderAcknowledgement <$> decodeI 7 (w8 .&. 0b01111111) rbuf
      else do
        i <- decodeI 6 (w8 .&. 0b00111111) rbuf
        return $ if w8 `testBit` 6 then StreamCancellation i else InsertCountIncrement i
