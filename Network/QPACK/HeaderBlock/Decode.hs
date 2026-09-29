{-# LANGUAGE BinaryLiterals #-}

module Network.QPACK.HeaderBlock.Decode where

import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import Data.CaseInsensitive
import Data.IORef
import Network.ByteOrder
import qualified Network.HPACK as HPACK
import Network.HPACK.Internal (
    HuffmanDecoder,
    decodeH,
    decodeI,
    decodeS,
    decodeSimple,
    decodeSophisticated,
    entryToken,
    entryTokenHeader,
 )
import Network.HTTP.Types

import Imports
import Network.QPACK.Error
import Network.QPACK.HeaderBlock.Prefix
import Network.QPACK.Table
import Network.QPACK.Types

decodeTokenHeader
    :: DynamicTable
    -> ReadBuffer
    -> IO (TokenHeaderTable, Bool)
decodeTokenHeader dyntbl rbuf = do
    (reqInsertCount, bp, needAck) <- decodePrefix rbuf dyntbl
    ready <- checkRequiredInsertCountNB dyntbl reqInsertCount
    -- The count of blocked streams has to come down however the wait ends.
    -- A stream reset while waiting kills the thread, and the count left up
    -- counted a stream that was not there any more: once as many had gone as
    -- SETTINGS_QPACK_BLOCKED_STREAMS allows, every section that had to wait
    -- was refused.
    unless ready $ E.mask $ \restore -> do
        ok <- tryIncreaseStreams dyntbl
        unless ok $ E.throwIO BlockedStreamsOverflow
        restore (checkRequiredInsertCount dyntbl reqInsertCount)
            `E.finally` decreaseStreams dyntbl
    checkRequiredInsertCount dyntbl reqInsertCount
    hufdec <- newHuffmanDecoder rbuf
    dec <- limitFieldSection dyntbl $ toTokenHeader dyntbl bp hufdec
    -- HPACK's decoder refuses a section of more than 200 fields, which is
    -- the same thing as far as we are concerned: more than we will take.
    tbl <-
        decodeSophisticated dec rbuf `E.catch` \e -> case e of
            HPACK.TooLargeHeader -> E.throwIO FieldSectionTooLarge
            _ -> E.throwIO e
    return (tbl, needAck)

decodeTokenHeaderS
    :: DynamicTable
    -> ReadBuffer
    -> IO (Maybe ([Header], Bool))
decodeTokenHeaderS dyntbl rbuf = do
    (reqInsertCount, bp, needAck) <- decodePrefix rbuf dyntbl
    ok <- checkRequiredInsertCountNB dyntbl reqInsertCount
    if ok
        then do
            hufdec <- newHuffmanDecoder rbuf
            dec <- limitFieldSection dyntbl $ toTokenHeader dyntbl bp hufdec
            hs <- decodeSimple dec rbuf
            return $ Just (hs, needAck)
        else return Nothing

-- | A field line decoder that stops once the section comes to more than we
--   said we would take.
--
-- The length of a HEADERS frame is capped, but that bounds the section only
-- as it is encoded.  A field line of a couple of octets can refer to a
-- dynamic table entry as large as the table, so a frame within the cap could
-- decode to many megabytes.  Counting field by field stops that before it is
-- built, and holds one section to the limit plus one field.
limitFieldSection
    :: DynamicTable
    -> (Word8 -> ReadBuffer -> IO TokenHeader)
    -> IO (Word8 -> ReadBuffer -> IO TokenHeader)
limitFieldSection dyntbl dec = do
    lim <- getMaxHeaderSize dyntbl
    ref <- newIORef 0
    return $ \w8 rbuf -> do
        th@(t, v) <- dec w8 rbuf
        let siz = BS8.length (original (tokenKey t)) + BS8.length v + 32
        total <- atomicModifyIORef' ref $ \n -> (n + siz, n + siz)
        when (total > lim) $ E.throwIO FieldSectionTooLarge
        return th

-- | A Huffman decoder with room for anything the rest of this field section
-- can decode to.
--
-- The scratch buffer has to hold one decoded string, and the shortest Huffman
-- code is five bits, so an encoded string of n octets cannot come to more than
-- 8n\/5 symbols -- and the section that contains it is itself at most what is
-- left in the buffer.  Sizing from that means a header field is refused only
-- when the section it is in is, rather than at a fixed 2048 that nothing
-- announced: a 2100-octet value used to fail to decode while the section
-- carrying it was under 1.4K, well inside the SETTINGS_MAX_FIELD_SECTION_SIZE
-- we advertise.
--
-- Allocated per section rather than held on the table, because sections from
-- different streams decode concurrently and this buffer is not shared.
newHuffmanDecoder :: ReadBuffer -> IO HuffmanDecoder
newHuffmanDecoder rbuf = do
    siz <- remainingSize rbuf
    let bufsiz = max 1 ((siz * 8) `div` 5)
    gcbuf <- mallocPlainForeignPtrBytes bufsiz
    return $ decodeH gcbuf bufsiz

{- FOURMOLU_DISABLE -}
toTokenHeader
    :: DynamicTable
    -> BasePoint
    -> HuffmanDecoder
    -> Word8
    -> ReadBuffer
    -> IO TokenHeader
toTokenHeader dyntbl bp hufdec w8 rbuf
    | w8 `testBit` 7 =
        decodeIndexedFieldLine                  rbuf dyntbl        bp w8
    | w8 `testBit` 6 =
        decodeLiteralFieldLineWithNameReference rbuf dyntbl hufdec bp w8
    | w8 `testBit` 5 =
        decodeLiteralFieldLineWithLiteralName   rbuf dyntbl hufdec bp w8
    | w8 `testBit` 4 =
        decodeIndexedFieldLineWithPostBaseIndex rbuf dyntbl        bp w8
    | otherwise =
        decodeLiteralFieldLineWithPostBaseNameReference rbuf dyntbl hufdec bp w8
{- FOURMOLU_ENABLE -}

-- 4.5.2.  Indexed Field Line
decodeIndexedFieldLine
    :: ReadBuffer -> DynamicTable -> BasePoint -> Word8 -> IO TokenHeader
decodeIndexedFieldLine rbuf dyntbl bp w8 = do
    i <- decodeI 6 (w8 .&. 0b00111111) rbuf
    let static = w8 `testBit` 6
        hidx
            | static = SIndex $ AbsoluteIndex i
            | otherwise = DIndex $ fromPreBaseIndex (PreBaseIndex i) bp
    ret <- atomically (entryTokenHeader <$> toIndexedEntry dyntbl hidx)
    qpackDebug dyntbl $
        putStrLn $
            "IndexedFieldLine (" ++ show hidx ++ ") " ++ showTokenHeader ret
    return ret

-- 4.5.3.  Indexed Field Line With Post-Base Index
decodeIndexedFieldLineWithPostBaseIndex
    :: ReadBuffer -> DynamicTable -> BasePoint -> Word8 -> IO TokenHeader
decodeIndexedFieldLineWithPostBaseIndex rbuf dyntbl bp w8 = do
    i <- decodeI 4 (w8 .&. 0b00001111) rbuf
    let hidx = DIndex $ fromPostBaseIndex (PostBaseIndex i) bp
    ret <- atomically (entryTokenHeader <$> toIndexedEntry dyntbl hidx)
    qpackDebug dyntbl $
        putStrLn $
            "IndexedFieldLineWithPostBaseIndex ("
                ++ show hidx
                ++ " "
                ++ show bp
                ++ " after "
                ++ show i
                ++ ") "
                ++ showTokenHeader ret
    return ret

-- 4.5.4.  Literal Field Line With Name Reference
decodeLiteralFieldLineWithNameReference
    :: ReadBuffer
    -> DynamicTable
    -> HuffmanDecoder
    -> BasePoint
    -> Word8
    -> IO TokenHeader
decodeLiteralFieldLineWithNameReference rbuf dyntbl hufdec bp w8 = do
    i <- decodeI 4 (w8 .&. 0b00001111) rbuf
    let static = w8 `testBit` 4
        hidx
            | static = SIndex $ AbsoluteIndex i
            | otherwise = DIndex $ fromPreBaseIndex (PreBaseIndex i) bp
    key <- atomically (entryToken <$> toIndexedEntry dyntbl hidx)
    val <- decodeS (`clearBit` 7) (`testBit` 7) 7 hufdec rbuf
    let ret = (key, val)
    qpackDebug dyntbl $
        putStrLn $
            "LiteralFieldLineWithNameReference ("
                ++ show hidx
                ++ ") "
                ++ showTokenHeader ret
    return ret

-- 4.5.5.  Literal Field Line With Post-Base Name Reference
decodeLiteralFieldLineWithPostBaseNameReference
    :: ReadBuffer
    -> DynamicTable
    -> HuffmanDecoder
    -> BasePoint
    -> Word8
    -> IO TokenHeader
decodeLiteralFieldLineWithPostBaseNameReference rbuf dyntbl hufdec bp w8 = do
    i <- decodeI 3 (w8 .&. 0b00000111) rbuf
    let hidx = DIndex $ fromPostBaseIndex (PostBaseIndex i) bp
    key <- atomically (entryToken <$> toIndexedEntry dyntbl hidx)
    val <- decodeS (`clearBit` 7) (`testBit` 7) 7 hufdec rbuf
    let ret = (key, val)
    qpackDebug dyntbl $
        putStrLn $
            "LiteralFieldLineWithPostBaseNameReference ("
                ++ show hidx
                ++ ") "
                ++ showTokenHeader ret
    return ret

-- 4.5.6.  Literal Field Line With Literal Name
decodeLiteralFieldLineWithLiteralName
    :: ReadBuffer
    -> DynamicTable
    -> HuffmanDecoder
    -> BasePoint
    -> Word8
    -> IO TokenHeader
decodeLiteralFieldLineWithLiteralName rbuf dyntbl hufdec _bp _w8 = do
    ff rbuf (-1)
    key <- toToken <$> decodeS (.&. 0b00000111) (`testBit` 3) 3 hufdec rbuf
    val <- decodeS (`clearBit` 7) (`testBit` 7) 7 hufdec rbuf
    let ret = (key, val)
    qpackDebug dyntbl $
        putStrLn $
            "LiteralFieldLineWithLiteralName " ++ showTokenHeader ret
    return ret

showTokenHeader :: TokenHeader -> String
showTokenHeader (t, val) = "\"" ++ key ++ "\" \"" ++ BS8.unpack val ++ "\""
  where
    key = BS8.unpack $ foldedCase $ tokenKey t
