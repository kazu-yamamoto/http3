{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QPACK (
  -- * Encoder
    QEncoderConfig(..)
  , defaultQEncoderConfig
  , QEncoder
  , newQEncoder
  -- * Decoder
  , QDecoderConfig(..)
  , defaultQDecoderConfig
  , QDecoder
  , newQDecoder
  -- ** Decoder for debugging
  , QDecoderS
  , newQDecoderS
  -- * Types
  , InstructionHandler
  , InstructionHandlerS
  , Cleanup
  , Size
  , EncodeStrategy(..)
  , CompressionAlgo(..)
  -- * Re-exports
  , HeaderTable
  , TokenHeaderList
  , ValueTable
  , Header
  , HeaderList
  , getHeaderValue
  , toHeaderTable
  , original
  , foldedCase
  , mk
  ) where

import qualified Data.ByteString as B
import Data.CaseInsensitive
import Foreign.Marshal.Alloc (mallocBytes, free)
import Network.ByteOrder
import Network.HPACK (HeaderTable, TokenHeaderList, EncodeStrategy(..), CompressionAlgo(..), HeaderList, ValueTable, getHeaderValue, toHeaderTable)
import Network.HPACK.Internal
import Network.QUIC.Internal (stdoutLogger, bhow)

import Imports
import Network.QPACK.HeaderBlock
import Network.QPACK.Instruction
import Network.QPACK.Table
import Network.QPACK.Types

----------------------------------------------------------------

type QEncoder = TokenHeaderList -> IO (ByteString, ByteString)
type QDecoder = ByteString -> IO HeaderTable
type QDecoderS = ByteString -> IO HeaderList

type InstructionHandler = (Int -> IO ByteString) -> IO ()
type InstructionHandlerS = ByteString -> IO ()

type Cleanup = IO ()

----------------------------------------------------------------

data QEncoderConfig = QEncoderConfig {
    ecDynamicTableSize      :: Size
  , ecHeaderBlockBufferSize :: Size
  , ecPrefixBufferSize      :: Size
  , ecInstructionBufferSize :: Size
  , encStrategy             :: EncodeStrategy
  }

defaultQEncoderConfig :: QEncoderConfig
defaultQEncoderConfig = QEncoderConfig {
    ecDynamicTableSize      = 4096
  , ecHeaderBlockBufferSize = 4096
  , ecPrefixBufferSize      =  128
  , ecInstructionBufferSize = 4096
  , encStrategy             = EncodeStrategy Static True
  }

newQEncoder :: QEncoderConfig -> IO (QEncoder, InstructionHandler, Cleanup)
newQEncoder QEncoderConfig{..} = do
    buf1  <- mallocBytes ecHeaderBlockBufferSize
    wbuf1 <- newWriteBuffer buf1 ecHeaderBlockBufferSize
    buf2  <- mallocBytes ecPrefixBufferSize
    wbuf2 <- newWriteBuffer buf2 ecPrefixBufferSize
    buf3  <- mallocBytes ecInstructionBufferSize
    wbuf3 <- newWriteBuffer buf3 ecInstructionBufferSize
    dyntbl <- newDynamicTableForEncoding ecDynamicTableSize
    let enc = qpackEncoder encStrategy wbuf1 wbuf2 wbuf3 dyntbl
        handler = decoderInstructionHandler dyntbl
        clean = do
            free buf1
            free buf2
            free buf3
            clearDynamicTable dyntbl
    return (enc, handler, clean)

qpackEncoder :: EncodeStrategy -> WriteBuffer -> WriteBuffer -> WriteBuffer -> DynamicTable -> TokenHeaderList -> IO (ByteString, ByteString)
qpackEncoder stgy wbuf1 wbuf2 wbuf3 dyntbl ts = do
    thl <- encodeTokenHeader wbuf1 wbuf3 stgy dyntbl ts -- fixme: leftover
    when (thl /= []) $ stdoutLogger "qpackEncoder: leftover"
    hb0 <- toByteString wbuf1
    ins <- toByteString wbuf3
    encodePrefix wbuf2 dyntbl
    prefix <- toByteString wbuf2
    let hb = prefix `B.append` hb0
    return (hb, ins)

decoderInstructionHandler :: DynamicTable -> (Int -> IO ByteString) -> IO ()
decoderInstructionHandler dyntbl recv = loop
  where
    loop = do
        _ <- getInsertionPoint dyntbl -- fixme
        bs <- recv 1024
        when (bs /= "") $ do
            (_ins,leftover) <- decodeDecoderInstructions bs -- fixme: saving leftover
            when (leftover /= "") $ stdoutLogger "decoderInstructionHandler: leftover"
            -- fixme: handle _ins
            mapM_ print _ins
            loop

----------------------------------------------------------------

data QDecoderConfig = QDecoderConfig {
    dcDynamicTableSize      :: Size
  , dcHuffmanBufferSize     :: Size
  }

defaultQDecoderConfig :: QDecoderConfig
defaultQDecoderConfig = QDecoderConfig {
    dcDynamicTableSize      = 4096
  , dcHuffmanBufferSize     = 4096
  }

newQDecoder :: QDecoderConfig -> IO (QDecoder, InstructionHandler, Cleanup)
newQDecoder QDecoderConfig{..} = do
    dyntbl <- newDynamicTableForDecoding dcDynamicTableSize dcHuffmanBufferSize
    let dec = qpackDecoder dyntbl
        handler = encoderInstructionHandler dyntbl
        clean = clearDynamicTable dyntbl
    return (dec, handler, clean)

newQDecoderS :: QDecoderConfig -> IO (QDecoderS, InstructionHandlerS, IO ())
newQDecoderS QDecoderConfig{..} = do
    dyntbl <- newDynamicTableForDecoding dcDynamicTableSize dcHuffmanBufferSize
    let dec = qpackDecoderS dyntbl
        handler = encoderInstructionHandlerS dyntbl
        clean = clearDynamicTable dyntbl
    return (dec, handler, clean)

qpackDecoder :: DynamicTable -> ByteString -> IO HeaderTable
qpackDecoder dyntbl bs = withReadBuffer bs $ \rbuf -> decodeTokenHeader dyntbl rbuf

qpackDecoderS :: DynamicTable -> ByteString -> IO HeaderList
qpackDecoderS dyntbl bs = withReadBuffer bs $ \rbuf -> decodeTokenHeaderS dyntbl rbuf

encoderInstructionHandler :: DynamicTable -> (Int -> IO ByteString) -> IO ()
encoderInstructionHandler dyntbl recv = loop
  where
    loop = do
        bs <- recv 1024
        when (bs /= "") $ do
            encoderInstructionHandlerS dyntbl bs
            loop

encoderInstructionHandlerS :: DynamicTable -> ByteString -> IO ()
encoderInstructionHandlerS dyntbl bs = when (bs /= "") $ do
    (ins,leftover) <- decodeEncoderInstructions hufdec bs -- fixme: saving leftover
    when (leftover /= "") $ stdoutLogger "encoderInstructionHandler: leftover"

    mapM_ print ins
    mapM_ handle ins
  where
    hufdec = getHuffmanDecoder dyntbl
    handle (SetDynamicTableCapacity n) = stdoutLogger ("SetDynamicTableCapacity " <> bhow n) -- fimxe
    handle (InsertWithNameReference ii val) = do
        idx <- case ii of
                 Left  ai -> return $ SIndex ai
                 Right ri -> do
                     ip <- getInsertionPoint dyntbl
                     return $ DIndex $ fromInsRelativeIndex ri ip
        ent0 <- toIndexedEntry dyntbl idx
        let ent = toEntryToken (entryToken ent0) val
        insertEntryToDecoder ent dyntbl
    handle (InsertWithoutNameReference t val) = do
        let ent = toEntryToken t val
        insertEntryToDecoder ent dyntbl
    handle (Duplicate n) = stdoutLogger ("Duplicate " <> bhow n) -- fimxe
