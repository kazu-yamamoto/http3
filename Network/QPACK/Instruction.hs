{-# LANGUAGE BinaryLiterals #-}

module Network.QPACK.Instruction (
  -- * Encoder instructions
    HIndex(..)
  , EncoderInstruction(..)
  , encodeEncoderInstructions
  , decodeEncoderInstructions
  , encodeEI
  , decodeEI
  -- * Decoder instructions
  , DecoderInstruction(..)
  , encodeDI
  , decodeDI
  ) where

import Data.Bits
import Data.CaseInsensitive
import Network.ByteOrder
import Network.HPACK.Internal
import Network.HPACK.Token

----------------------------------------------------------------

data HIndex = SIndex Int | DIndex Int deriving (Eq, Ord, Show)

data EncoderInstruction = SetDynamicTableCapacity Int
                        | InsertWithNameReference HIndex HeaderValue
                        | InsertWithoutNameReference Token HeaderValue
                        | Duplicate Int -- fixme
                        deriving (Eq, Show)

----------------------------------------------------------------

encodeEncoderInstructions :: [EncoderInstruction] -> Bool -> IO ByteString
encodeEncoderInstructions eis huff = withWriteBuffer 4096 $ \wbuf ->
    mapM_ (encodeEI wbuf huff) eis

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

----------------------------------------------------------------

decodeEncoderInstructions :: ByteString -> IO [EncoderInstruction]
decodeEncoderInstructions bs = fmap snd .  withWriteBuffer' 4096 $ \wbuf ->
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


----------------------------------------------------------------

data DecoderInstruction = HeaderAcknowledgement Int
                        | StreamCancellation Int
                        | InsertCountIncrement Int
                        deriving (Eq, Show)

----------------------------------------------------------------

encodeDI :: WriteBuffer -> DecoderInstruction -> IO ()
encodeDI wbuf (HeaderAcknowledgement n) = encodeI wbuf set1  7 n
encodeDI wbuf (StreamCancellation n)    = encodeI wbuf set01 6 n
encodeDI wbuf (InsertCountIncrement n)  = encodeI wbuf id    6 n

----------------------------------------------------------------

decodeDI :: ReadBuffer -> IO DecoderInstruction
decodeDI rbuf = do
    w8 <- read8 rbuf
    if w8 `testBit` 7 then
        HeaderAcknowledgement <$> decodeI 7 (w8 .&. 0b01111111) rbuf
      else do
        i <- decodeI 6 (w8 .&. 0b00111111) rbuf
        return $ if w8 `testBit` 6 then StreamCancellation i else InsertCountIncrement i

----------------------------------------------------------------

type Setter = Word8 -> Word8

set1, set01, set11, set001, set000 :: Setter
set1    x = x `setBit` 7
set01   x = x `setBit` 6
set11   x = x .|. 0b11000000
set001  x = x `setBit` 5
set000  = id
