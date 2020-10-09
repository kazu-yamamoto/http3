 {-# LANGUAGE BinaryLiterals #-}

module Network.QPACK.HeaderBlock.Decode where

import Network.ByteOrder
import Network.HPACK (TokenHeader, HeaderTable, HeaderList)
import Network.HPACK.Internal
import Network.HPACK.Token (toToken)

import Imports
import Network.QPACK.HeaderBlock.Prefix
import Network.QPACK.Table
import Network.QPACK.Types

decodeTokenHeader :: DynamicTable
                  -> ReadBuffer
                  -> IO HeaderTable
decodeTokenHeader dyntbl rbuf = do
    (reqip, bp) <- decodePrefix rbuf dyntbl
    checkInsertionPoint dyntbl reqip
    decodeSophisticated (toTokenHeader dyntbl bp) rbuf

decodeTokenHeaderS :: DynamicTable
                   -> ReadBuffer
                   -> IO HeaderList
decodeTokenHeaderS dyntbl rbuf = do
    (reqip, bp) <- decodePrefix rbuf dyntbl
    checkInsertionPoint dyntbl reqip
    decodeSimple (toTokenHeader dyntbl bp) rbuf

toTokenHeader :: DynamicTable -> BasePoint -> Word8 -> ReadBuffer -> IO TokenHeader
toTokenHeader dyntbl bp w8 rbuf
  | w8 `testBit` 7 = decodeIndexedHeaderField rbuf dyntbl bp w8
  | w8 `testBit` 6 = decodeLiteralHeaderFieldWithNameReference rbuf dyntbl bp w8
  | w8 `testBit` 5 = decodeLiteralHeaderFieldWithoutNameReference rbuf dyntbl bp w8
  | w8 `testBit` 4 = decodeIndexedHeaderFieldWithPostBaseIndex rbuf dyntbl bp w8
  | otherwise      = decodeLiteralHeaderFieldWithPostBaseNameReference rbuf dyntbl bp w8

-- 4.5.2.  Indexed Header Field
decodeIndexedHeaderField :: ReadBuffer -> DynamicTable -> BasePoint -> Word8 -> IO TokenHeader
decodeIndexedHeaderField rbuf dyntbl bp w8 = do
    i <- decodeI 6 (w8 .&. 0b00111111) rbuf
    let static = w8 `testBit` 6
        hidx | static    = SIndex $ AbsoluteIndex i
             | otherwise = DIndex $ fromHBRelativeIndex (HBRelativeIndex i) bp
    entryTokenHeader <$> toIndexedEntry dyntbl hidx

-- 4.5.4.  Literal Header Field With Name Reference
decodeLiteralHeaderFieldWithNameReference :: ReadBuffer -> DynamicTable -> BasePoint -> Word8 -> IO TokenHeader
decodeLiteralHeaderFieldWithNameReference rbuf dyntbl bp w8 = do
    i <- decodeI 4 (w8 .&. 0b00001111) rbuf
    let static = w8 `testBit` 4
        hidx | static    = SIndex $ AbsoluteIndex i
             | otherwise = DIndex $ fromHBRelativeIndex (HBRelativeIndex i) bp
    key <- entryToken <$> toIndexedEntry dyntbl hidx
    let hufdec = getHuffmanDecoder dyntbl
    val <- decodeS (`clearBit` 7) (`testBit` 7) 7 hufdec rbuf
    return (key,val)

-- 4.5.6.  Literal Header Field Without Name Reference
decodeLiteralHeaderFieldWithoutNameReference :: ReadBuffer -> DynamicTable -> BasePoint -> Word8 -> IO TokenHeader
decodeLiteralHeaderFieldWithoutNameReference rbuf dyntbl _bp _w8 = do
    ff rbuf (-1)
    let hufdec = getHuffmanDecoder dyntbl
    key <- toToken <$> decodeS (.&. 0b00000111) (`testBit` 3) 3 hufdec rbuf
    val <- decodeS (`clearBit` 7) (`testBit` 7) 7 hufdec rbuf
    return (key,val)

-- 4.5.3.  Indexed Header Field With Post-Base Index
decodeIndexedHeaderFieldWithPostBaseIndex :: ReadBuffer -> DynamicTable -> BasePoint -> Word8 -> IO TokenHeader
decodeIndexedHeaderFieldWithPostBaseIndex rbuf dyntbl bp w8 = do
    i <- decodeI 4 (w8 .&. 0b00001111) rbuf
    let hidx = DIndex $ fromPostBaseIndex (PostBaseIndex i) bp
    entryTokenHeader <$> toIndexedEntry dyntbl hidx

-- 4.5.5.  Literal Header Field With Post-Base Name Reference
decodeLiteralHeaderFieldWithPostBaseNameReference :: ReadBuffer -> DynamicTable -> BasePoint -> Word8 -> IO TokenHeader
decodeLiteralHeaderFieldWithPostBaseNameReference rbuf dyntbl bp w8 = do
    i <- decodeI 3 (w8 .&. 0b00000111) rbuf
    let hidx = DIndex $ fromPostBaseIndex (PostBaseIndex i) bp
    key <- entryToken <$> toIndexedEntry dyntbl hidx
    let hufdec = getHuffmanDecoder dyntbl
    val <- decodeS (`clearBit` 7) (`testBit` 7) 7 hufdec rbuf
    return (key,val)
