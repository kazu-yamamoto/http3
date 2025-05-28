{-# LANGUAGE BinaryLiterals #-}

module Network.QPACK.HeaderBlock.Decode where

import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import Data.CaseInsensitive
import Network.ByteOrder
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
    unless ready $ do
        ok <- tryIncreaseStreams dyntbl
        unless ok $ E.throwIO BlockedStreamsOverflow
        checkRequiredInsertCount dyntbl reqInsertCount
        decreaseStreams dyntbl
    checkRequiredInsertCount dyntbl reqInsertCount
    let bufsiz = 2048
    gcbuf <- mallocPlainForeignPtrBytes 2048
    let hufdec = decodeH gcbuf bufsiz
    tbl <- decodeSophisticated (toTokenHeader dyntbl bp hufdec) rbuf
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
            let bufsiz = 2048
            gcbuf <- mallocPlainForeignPtrBytes 2048
            let hufdec = decodeH gcbuf bufsiz
            hs <- decodeSimple (toTokenHeader dyntbl bp hufdec) rbuf
            return $ Just (hs, needAck)
        else return Nothing

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
            | otherwise = DIndex $ fromHBRelativeIndex (HBRelativeIndex i) bp
    ret <- atomically (entryTokenHeader <$> toIndexedEntry dyntbl hidx)
    qpackDebug dyntbl $ do
        checkHIndex dyntbl hidx
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
    qpackDebug dyntbl $ do
        checkHIndex dyntbl hidx
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
            | otherwise = DIndex $ fromHBRelativeIndex (HBRelativeIndex i) bp
    key <- atomically (entryToken <$> toIndexedEntry dyntbl hidx)
    val <- decodeS (`clearBit` 7) (`testBit` 7) 7 hufdec rbuf
    let ret = (key, val)
    qpackDebug dyntbl $ do
        checkHIndex dyntbl hidx
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
    qpackDebug dyntbl $ do
        checkHIndex dyntbl hidx
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
