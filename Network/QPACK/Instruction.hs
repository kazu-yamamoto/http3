{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.QPACK.Instruction (
    -- * Encoder instructions
    HIndex (..),
    EncoderInstruction (..),
    InsIndex,
    encodeEncoderInstructions,
    decodeEncoderInstructions,
    decodeEncoderInstructions',
    encoderInstructionLength,
    encodeEI,
    decodeEI,

    -- * Decoder instructions
    DecoderInstruction (..),
    encodeDecoderInstructions,
    decodeDecoderInstructions,
    encodeDI,
    decodeDI,
) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.CaseInsensitive
import Network.ByteOrder
import Network.HPACK.Internal (
    HuffmanDecoder,
    decodeH,
    decodeI,
    decodeS,
    encodeI,
    encodeS,
    entryHeaderName,
 )
import Network.QUIC (StreamId)

import Imports
import Network.QPACK.Table.Static
import Network.QPACK.Types

----------------------------------------------------------------

type InsIndex = Either AbsoluteIndex InsRelativeIndex

data EncoderInstruction
    = SetDynamicTableCapacity Int
    | InsertWithNameReference InsIndex FieldValue
    | InsertWithLiteralName Token FieldValue
    | Duplicate InsRelativeIndex
    deriving (Eq)

instance Show EncoderInstruction where
    show (SetDynamicTableCapacity n) = "SetDynamicTableCapacity " ++ show n
    show (InsertWithNameReference (Left aidx) v) =
        "InsertWithNameReference \""
            ++ BS8.unpack (original (entryHeaderName (toStaticEntry aidx)))
            ++ "\" \""
            ++ BS8.unpack v
            ++ "\""
    show (InsertWithNameReference (Right (InsRelativeIndex idx)) v) =
        "InsertWithNameReference (InsRelative "
            ++ show idx
            ++ ") \""
            ++ BS8.unpack v
            ++ "\""
    show (InsertWithLiteralName t v) =
        "InsertWithLiteralName \""
            ++ BS8.unpack (foldedCase (tokenKey t))
            ++ "\" \""
            ++ BS8.unpack v
            ++ "\""
    show (Duplicate (InsRelativeIndex idx)) = "Duplicate (InsRelative " ++ show idx ++ ")"

----------------------------------------------------------------

encodeEncoderInstructions :: [EncoderInstruction] -> Bool -> IO ByteString
encodeEncoderInstructions eis huff = withWriteBuffer 4096 $ \wbuf ->
    mapM_ (encodeEI wbuf huff) eis

encodeEI :: WriteBuffer -> Bool -> EncoderInstruction -> IO ()
encodeEI wbuf _ (SetDynamicTableCapacity cap) = encodeI wbuf set001 5 cap
encodeEI wbuf huff (InsertWithNameReference hidx v) = do
    let (set, idx) = case hidx of
            Left (AbsoluteIndex i) -> (set11, i)
            Right (InsRelativeIndex i) -> (set1, i)
    encodeI wbuf set 6 idx
    encodeS wbuf huff id set1 7 v
encodeEI wbuf huff (InsertWithLiteralName k v) = do
    encodeS wbuf huff set01 set001 5 $ foldedCase $ tokenKey k
    encodeS wbuf huff id set1 7 v
encodeEI wbuf _ (Duplicate (InsRelativeIndex idx)) = encodeI wbuf set000 5 idx

----------------------------------------------------------------

decodeEncoderInstructions'
    :: ByteString -> IO ([EncoderInstruction], ByteString)
decodeEncoderInstructions' bs = do
    let bufsiz = 2048
    gcbuf <- mallocPlainForeignPtrBytes 2048
    decodeEncoderInstructions (decodeH gcbuf bufsiz) bs

-- HuffmanDecoder is occupied.
-- No other threads MUST NOT use it.
decodeEncoderInstructions
    :: HuffmanDecoder -> ByteString -> IO ([EncoderInstruction], ByteString)
decodeEncoderInstructions hufdec bs = withReadBuffer bs $ loop id
  where
    loop build rbuf = do
        n <- remainingSize rbuf
        if n == 0
            then do
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

-- | The length in octets of the first encoder instruction, once enough of it
-- has arrived to tell: its first octet and the integers that give the lengths
-- of its strings (RFC 9204, section 4.3).  'Nothing' until then.
--
-- So that an instruction arriving a piece at a time is decoded once it is
-- all there, rather than tried again from its beginning with each piece.
encoderInstructionLength :: ByteString -> Maybe Int
encoderInstructionLength bs = case BS.uncons bs of
    Nothing -> Nothing
    Just (w0, _)
        -- Insert with name reference: an index, then the value
        | w0 `testBit` 7 -> int 6 0 >>= str . snd
        -- Insert with literal name: the name, then the value
        | w0 `testBit` 6 -> do
            (len, off) <- int 5 0
            str (off + len)
        -- Set dynamic table capacity, or duplicate: an integer
        | otherwise -> snd <$> int 5 0
  where
    -- A string at the offset: its length, with a 7-bit prefix after the
    -- Huffman bit, then the string itself.
    str off = do
        (len, off') <- int 7 off
        return $ off' + len
    -- An integer with an n-bit prefix at the offset: its value, and the
    -- offset after it.  One too long to be a length here is taken as one
    -- longer than any instruction can be.
    int :: Int -> Int -> Maybe (Int, Int)
    int n off = do
        w <- at off
        let mask = (1 `shiftL` n) - 1
            v = fromIntegral w .&. mask
        if v < mask then Just (v, off + 1) else go (off + 1) v 0
      where
        go i acc m
            | m > 28 = Just (maxBound `div` 2, i)
            | otherwise = do
                b <- at i
                let acc' = acc + ((fromIntegral b .&. 0x7f) `shiftL` m)
                if b < 0x80 then Just (acc', i + 1) else go (i + 1) acc' (m + 7)
    at i
        | i < BS.length bs = Just $ BS.index bs i
        | otherwise = Nothing

decodeEI :: HuffmanDecoder -> ReadBuffer -> IO EncoderInstruction
decodeEI hufdec rbuf = do
    w8 <- read8 rbuf
    if w8 `testBit` 7
        then decodeInsertWithNameReference rbuf w8 hufdec
        else
            if w8 `testBit` 6
                then decodeInsertWithLiteralName rbuf hufdec
                else
                    if w8 `testBit` 5
                        then decodeSetDynamicTableCapacity rbuf w8
                        else decodeDuplicate rbuf w8

decodeInsertWithNameReference
    :: ReadBuffer -> Word8 -> HuffmanDecoder -> IO EncoderInstruction
decodeInsertWithNameReference rbuf w8 hufdec = do
    idx <- decodeI 6 (w8 .&. 0b00111111) rbuf
    let hidx
            | w8 `testBit` 6 = Left (AbsoluteIndex idx)
            | otherwise = Right (InsRelativeIndex idx)
    v <- decodeS (.&. 0b01111111) (`testBit` 7) 7 hufdec rbuf
    return $ InsertWithNameReference hidx v

decodeInsertWithLiteralName
    :: ReadBuffer -> HuffmanDecoder -> IO EncoderInstruction
decodeInsertWithLiteralName rbuf hufdec = do
    ff rbuf (-1)
    k <- decodeS (.&. 0b00011111) (`testBit` 5) 5 hufdec rbuf
    v <- decodeS (.&. 0b01111111) (`testBit` 7) 7 hufdec rbuf
    return $ InsertWithLiteralName (toToken k) v

decodeSetDynamicTableCapacity :: ReadBuffer -> Word8 -> IO EncoderInstruction
decodeSetDynamicTableCapacity rbuf w8 =
    SetDynamicTableCapacity <$> decodeI 5 (w8 .&. 0b00011111) rbuf

decodeDuplicate :: ReadBuffer -> Word8 -> IO EncoderInstruction
decodeDuplicate rbuf w8 =
    Duplicate . InsRelativeIndex <$> decodeI 5 (w8 .&. 0b00011111) rbuf

----------------------------------------------------------------

data DecoderInstruction
    = SectionAcknowledgement StreamId
    | StreamCancellation StreamId
    | InsertCountIncrement Int
    deriving (Eq, Show)

----------------------------------------------------------------

encodeDecoderInstructions :: [DecoderInstruction] -> IO ByteString
encodeDecoderInstructions dis = withWriteBuffer 4096 $ \wbuf ->
    mapM_ (encodeDI wbuf) dis

encodeDI :: WriteBuffer -> DecoderInstruction -> IO ()
encodeDI wbuf (SectionAcknowledgement n) = encodeI wbuf set1 7 n
encodeDI wbuf (StreamCancellation n) = encodeI wbuf set01 6 n
encodeDI wbuf (InsertCountIncrement n) = encodeI wbuf id 6 n

----------------------------------------------------------------

decodeDecoderInstructions :: ByteString -> IO ([DecoderInstruction], ByteString)
decodeDecoderInstructions bs = withReadBuffer bs $ loop id
  where
    loop build rbuf = do
        n <- remainingSize rbuf
        if n == 0
            then do
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
    if w8 `testBit` 7
        then SectionAcknowledgement <$> decodeI 7 (w8 .&. 0b01111111) rbuf
        else do
            i <- decodeI 6 (w8 .&. 0b00111111) rbuf
            return $ if w8 `testBit` 6 then StreamCancellation i else InsertCountIncrement i
