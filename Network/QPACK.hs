{-# LANGUAGE RecordWildCards #-}

module Network.QPACK (
  -- Encoder
    EncoderConfig(..)
  , defaultEncoderConfig
  , Encoder
  , newEncoder
  -- Decoder
  , DecoderConfig(..)
  , defaultDecoderConfig
  , Decoder
  , newDecoder
  -- * Types
  , HandleInstruction
  , Cleanup
  , Size
  , EncodeStrategy(..)
  , CompressionAlgo(..)
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString as B
import Foreign.Marshal.Alloc (mallocBytes, free)
import Network.ByteOrder
import Network.HPACK (TokenHeaderList, EncodeStrategy(..), CompressionAlgo(..))
import Network.HPACK.Internal

import Imports
import Network.QPACK.HeaderBlock
import Network.QPACK.Instruction
import Network.QPACK.Table
import Network.QPACK.Types

----------------------------------------------------------------

type Encoder = TokenHeaderList -> IO (ByteString, ByteString)
type Decoder = ByteString -> IO TokenHeaderList

type HandleInstruction = ByteString -> IO ()

type Cleanup = IO ()

----------------------------------------------------------------

data EncoderConfig = EncoderConfig {
    ecDynamicTableSize      :: Size
  , ecHeaderBlockBufferSize :: Size
  , ecPrefixBufferSize      :: Size
  , ecInstructionBufferSize :: Size
  , encStrategy             :: EncodeStrategy
  }

defaultEncoderConfig :: EncoderConfig
defaultEncoderConfig = EncoderConfig {
    ecDynamicTableSize      = 4096
  , ecHeaderBlockBufferSize = 4096
  , ecPrefixBufferSize      =  128
  , ecInstructionBufferSize = 4096
  , encStrategy             = EncodeStrategy Static True
  }

newEncoder :: EncoderConfig -> IO (Encoder, HandleInstruction, Cleanup)
newEncoder EncoderConfig{..} = do
    buf1  <- mallocBytes ecHeaderBlockBufferSize
    wbuf1 <- newWriteBuffer buf1 ecHeaderBlockBufferSize
    buf2  <- mallocBytes ecPrefixBufferSize
    wbuf2 <- newWriteBuffer buf2 ecPrefixBufferSize
    buf3  <- mallocBytes ecInstructionBufferSize
    wbuf3 <- newWriteBuffer buf3 ecInstructionBufferSize
    dyntbl <- newDynamicTableForEncoding ecDynamicTableSize
    q <- newTQueueIO
    tid <- forkIO $ handleDecodeInstruction dyntbl q
    let enc = qpackEncoder encStrategy wbuf1 wbuf2 wbuf3 dyntbl
        handle = atomically . writeTQueue q
        clean = do
            free buf1
            free buf2
            free buf3
            clearDynamicTable dyntbl
            killThread tid
    return (enc, handle, clean)

qpackEncoder :: EncodeStrategy -> WriteBuffer -> WriteBuffer -> WriteBuffer -> DynamicTable -> TokenHeaderList -> IO (ByteString, ByteString)
qpackEncoder stgy wbuf1 wbuf2 wbuf3 dyntbl ts = do
    _ <- encodeTokenHeader wbuf1 wbuf3 stgy dyntbl ts -- fixme: leftover
    hb0 <- toByteString wbuf1
    ins <- toByteString wbuf3
    encodePrefix wbuf2 dyntbl
    prefix <- toByteString wbuf2
    let hb = prefix `B.append` hb0
    return (hb, ins)

handleDecodeInstruction :: DynamicTable -> TQueue ByteString -> IO ()
handleDecodeInstruction dyntbl q = forever $ do
    _ <- getInsertionPoint dyntbl -- fixme
    bs <- atomically $ readTQueue q
    ins <- decodeDecoderInstructions bs
    print ins

----------------------------------------------------------------

data DecoderConfig = DecoderConfig {
    dcDynamicTableSize      :: Size
  , dcHuffmanBufferSize     :: Size
  }

defaultDecoderConfig :: DecoderConfig
defaultDecoderConfig = DecoderConfig {
    dcDynamicTableSize      = 4096
  , dcHuffmanBufferSize     = 4096
  }

newDecoder :: DecoderConfig -> IO (Decoder, HandleInstruction, Cleanup)
newDecoder DecoderConfig{..} = do
    dyntbl <- newDynamicTableForDecoding dcDynamicTableSize dcHuffmanBufferSize
    q <- newTQueueIO
    tid <- forkIO $ handleEncodeInstruction dyntbl q
    let dec = qpackDecoder dyntbl
        handle = atomically . writeTQueue q
        clean = do
            clearDynamicTable dyntbl
            killThread tid
    return (dec, handle, clean)

qpackDecoder :: DynamicTable -> ByteString -> IO TokenHeaderList
qpackDecoder dyntbl bs = withReadBuffer bs $ \rbuf -> decodeTokenHeader dyntbl rbuf

handleEncodeInstruction :: DynamicTable -> TQueue ByteString -> IO ()
handleEncodeInstruction dyntbl q = forever $ do
    bs <- atomically $ readTQueue q
    ins <- decodeEncoderInstructions bs
    print ins
    mapM_ handle ins
  where
    handle (SetDynamicTableCapacity _) = return () -- fixme
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
    handle (Duplicate _) = return ()
