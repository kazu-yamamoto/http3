{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.QPACK.Instruction (
  -- * Encoder instructions
    HIndex(..)
  , EncoderInstruction(..)
  , InsIndex
  , encodeEncoderInstructions
  , decodeEncoderInstructions
  , decodeEncoderInstructions'
  , encodeEI
  , decodeEI
  -- * Decoder instructions
  , DecoderInstruction(..)
  , encodeDecoderInstructions
  , decodeDecoderInstructions
  , encodeDI
  , decodeDI
  ) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import Data.CaseInsensitive
import Network.ByteOrder
import Network.HPACK.Internal
import Network.HPACK.Token

import Imports
import Network.QPACK.Types
import Network.QPACK.Table.Static

----------------------------------------------------------------

type InsIndex = Either AbsoluteIndex InsRelativeIndex

data EncoderInstruction = SetDynamicTableCapacity Int
                        | InsertWithNameReference InsIndex HeaderValue
                        | InsertWithoutNameReference Token HeaderValue
                        | Duplicate InsRelativeIndex
                        deriving (Eq)

instance Show EncoderInstruction where
    show (SetDynamicTableCapacity n) = "SetDynamicTableCapacity " ++ show n
    show (InsertWithNameReference (Left aidx) v) = "InsertWithNameReference \"" ++ BS8.unpack (entryHeaderName (toStaticEntry aidx)) ++ "\" \"" ++ BS8.unpack v ++ "\""
    show (InsertWithNameReference (Right (InsRelativeIndex idx)) v) = "InsertWithNameReference (DynRel " ++ show idx ++ ") \"" ++ BS8.unpack v ++ "\""
    show (InsertWithoutNameReference t v) = "InsertWithoutNameReference \"" ++ BS8.unpack (foldedCase (tokenKey t)) ++ "\" \"" ++ BS8.unpack v ++ "\""
    show (Duplicate (InsRelativeIndex idx)) = "Duplicate (DynRel " ++ show idx ++ ")"

----------------------------------------------------------------

encodeEncoderInstructions :: [EncoderInstruction] -> Bool -> IO ByteString
encodeEncoderInstructions eis huff = withWriteBuffer 4096 $ \wbuf ->
    mapM_ (encodeEI wbuf huff) eis

encodeEI :: WriteBuffer -> Bool -> EncoderInstruction -> IO ()
encodeEI wbuf _    (SetDynamicTableCapacity cap) = encodeI wbuf set001 5 cap
encodeEI wbuf huff (InsertWithNameReference hidx v) = do
    let (set, idx) = case hidx of
          Left  (AbsoluteIndex i)    -> (set11, i)
          Right (InsRelativeIndex i) -> (set1,  i)
    encodeI wbuf set 6 idx
    encodeS wbuf huff id set1 7 v
encodeEI wbuf huff (InsertWithoutNameReference k v) = do
    encodeS wbuf huff set01 set001 5 $ foldedCase $ tokenKey k
    encodeS wbuf huff id    set1   7 v
encodeEI wbuf _    (Duplicate (InsRelativeIndex idx)) = encodeI wbuf set000 5 idx

----------------------------------------------------------------

decodeEncoderInstructions' :: ByteString -> IO ([EncoderInstruction], ByteString)
decodeEncoderInstructions' bs = fmap snd . withWriteBuffer' 4096 $ \wbuf->
  decodeEncoderInstructions (decodeH wbuf) bs

decodeEncoderInstructions :: HuffmanDecoder -> ByteString -> IO ([EncoderInstruction],ByteString)
decodeEncoderInstructions hufdec bs = withReadBuffer bs $ loop id
  where
    loop build rbuf = do
        n <- remainingSize rbuf
        if n == 0 then do
            let eis = build []
            return (eis, "")
          else do
            save rbuf
            er <- E.try $ decodeEI hufdec rbuf
            case er of
              Right r -> loop (build . (r :)) rbuf
              Left BufferOverrun -> do
                  goBack rbuf
                  rn <- remainingSize rbuf
                  left <- extractByteString rbuf rn
                  let eis = build []
                  return (eis, left)

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
    let hidx | w8 `testBit` 6 = Left (AbsoluteIndex idx)
             | otherwise      = Right (InsRelativeIndex idx)
    v <- decodeS (.&. 0b01111111) (`testBit` 7) 7 hufdec rbuf
    return $ InsertWithNameReference hidx v

decodeInsertWithoutNameReference :: ReadBuffer -> HuffmanDecoder -> IO EncoderInstruction
decodeInsertWithoutNameReference rbuf hufdec = do
    ff rbuf (-1)
    k <- decodeS (.&. 0b00011111) (`testBit` 5) 5 hufdec rbuf
    v <- decodeS (.&. 0b01111111) (`testBit` 7) 7 hufdec rbuf
    return $ InsertWithoutNameReference (toToken k)  v

decodeSetDynamicTableCapacity :: ReadBuffer -> Word8 -> IO EncoderInstruction
decodeSetDynamicTableCapacity rbuf w8 =
    SetDynamicTableCapacity <$> decodeI 5 (w8 .&. 0b00011111) rbuf

decodeDuplicate :: ReadBuffer -> Word8 -> IO EncoderInstruction
decodeDuplicate rbuf w8 =
    Duplicate . InsRelativeIndex <$> decodeI 5 (w8 .&. 0b00011111) rbuf

----------------------------------------------------------------

data DecoderInstruction = SectionAcknowledgement Int
                        | StreamCancellation Int
                        | InsertCountIncrement Int
                        deriving (Eq, Show)

----------------------------------------------------------------

encodeDecoderInstructions :: [DecoderInstruction] -> IO ByteString
encodeDecoderInstructions dis = withWriteBuffer 4096 $ \wbuf ->
    mapM_ (encodeDI wbuf) dis

encodeDI :: WriteBuffer -> DecoderInstruction -> IO ()
encodeDI wbuf (SectionAcknowledgement n) = encodeI wbuf set1  7 n
encodeDI wbuf (StreamCancellation n)    = encodeI wbuf set01 6 n
encodeDI wbuf (InsertCountIncrement n)  = encodeI wbuf id    6 n

----------------------------------------------------------------

decodeDecoderInstructions :: ByteString -> IO ([DecoderInstruction],ByteString)
decodeDecoderInstructions bs = withReadBuffer bs $ loop id
  where
    loop build rbuf = do
        n <- remainingSize rbuf
        if n == 0 then do
            let dis = build []
            return (dis, "")
          else do
            save rbuf
            er <- E.try $ decodeDI rbuf
            case er of
              Right r -> loop (build . (r :)) rbuf
              Left BufferOverrun -> do
                  goBack rbuf
                  rn <- remainingSize rbuf
                  left <- extractByteString rbuf rn
                  let dis = build []
                  return (dis, left)

decodeDI :: ReadBuffer -> IO DecoderInstruction
decodeDI rbuf = do
    w8 <- read8 rbuf
    if w8 `testBit` 7 then
        SectionAcknowledgement <$> decodeI 7 (w8 .&. 0b01111111) rbuf
      else do
        i <- decodeI 6 (w8 .&. 0b00111111) rbuf
        return $ if w8 `testBit` 6 then StreamCancellation i else InsertCountIncrement i
