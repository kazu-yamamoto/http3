{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Thread-safe QPACK encoder/decoder.
module Network.QPACK (
    -- * Encoder
    QEncoderConfig (..),
    defaultQEncoderConfig,
    QEncoder,
    newQEncoder,

    -- * Decoder
    QDecoderConfig (..),
    defaultQDecoderConfig,
    QDecoder,
    newQDecoder,

    -- ** Decoder for debugging
    QDecoderS,
    newQDecoderS,

    -- * Types
    EncodedEncoderInstruction,
    EncoderInstructionHandler,
    EncoderInstructionHandlerS,
    EncodedDecoderInstruction,
    DecoderInstructionHandler,
    InstructionHandler,
    Size,

    -- * Strategy
    EncodeStrategy (..),
    CompressionAlgo (..),

    -- * Re-exports
    HeaderTable,
    TokenHeaderList,
    ValueTable,
    Header,
    HeaderList,
    getHeaderValue,
    toHeaderTable,
    original,
    foldedCase,
    mk,
) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString as B
import Data.CaseInsensitive
import Network.ByteOrder
import Network.HPACK (
    HeaderList,
    HeaderTable,
    TokenHeaderList,
    ValueTable,
    getHeaderValue,
    toHeaderTable,
 )
import Network.HPACK.Internal
import Network.QUIC.Internal (stdoutLogger)
import qualified UnliftIO.Exception as E

import Imports
import Network.QPACK.Error
import Network.QPACK.HeaderBlock
import Network.QPACK.Instruction
import Network.QPACK.Table
import Network.QPACK.Types

----------------------------------------------------------------

-- | QPACK encoder.
type QEncoder =
    TokenHeaderList -> IO (EncodedFieldSection, EncodedEncoderInstruction)

-- | QPACK decoder.
type QDecoder = EncodedFieldSection -> IO HeaderTable

-- | QPACK simple decoder.
type QDecoderS = EncodedFieldSection -> IO HeaderList

-- | Encoder instruction handler.
type EncoderInstructionHandler = (Int -> IO EncodedEncoderInstruction) -> IO ()

-- | Simple encoder instruction handler.
type EncoderInstructionHandlerS = EncodedEncoderInstruction -> IO ()

-- | Encoded decoder instruction.
type EncodedDecoderInstruction = ByteString

-- | Decoder instruction handler.
type DecoderInstructionHandler = (Int -> IO EncodedDecoderInstruction) -> IO ()

-- | A type to integrating handlers.
type InstructionHandler = (Int -> IO ByteString) -> IO ()

----------------------------------------------------------------

-- | Configuration for QPACK encoder.
data QEncoderConfig = QEncoderConfig
    { ecDynamicTableSize :: Size
    , ecHeaderBlockBufferSize :: Size
    , ecPrefixBufferSize :: Size
    , ecInstructionBufferSize :: Size
    , encStrategy :: EncodeStrategy
    }
    deriving (Show)

-- | Default configuration for QPACK encoder.
--
-- >>> defaultQEncoderConfig
-- QEncoderConfig {ecDynamicTableSize = 4096, ecHeaderBlockBufferSize = 4096, ecPrefixBufferSize = 128, ecInstructionBufferSize = 4096, encStrategy = EncodeStrategy {compressionAlgo = Static, useHuffman = True}}
defaultQEncoderConfig :: QEncoderConfig
defaultQEncoderConfig =
    QEncoderConfig
        { ecDynamicTableSize = 4096
        , ecHeaderBlockBufferSize = 4096
        , ecPrefixBufferSize = 128
        , ecInstructionBufferSize = 4096
        , encStrategy = EncodeStrategy Static True
        }

-- | Creating a new QPACK encoder.
newQEncoder :: QEncoderConfig -> IO (QEncoder, DecoderInstructionHandler)
newQEncoder QEncoderConfig{..} = do
    let bufsiz1 = ecHeaderBlockBufferSize
        bufsiz2 = ecPrefixBufferSize
        bufsiz3 = ecInstructionBufferSize
    gcbuf1 <- mallocPlainForeignPtrBytes bufsiz1
    gcbuf2 <- mallocPlainForeignPtrBytes bufsiz2
    gcbuf3 <- mallocPlainForeignPtrBytes bufsiz3
    dyntbl <- newDynamicTableForEncoding ecDynamicTableSize
    lock <- newMVar ()
    let enc =
            qpackEncoder
                encStrategy
                gcbuf1
                bufsiz1
                gcbuf2
                bufsiz2
                gcbuf3
                bufsiz3
                dyntbl
                lock
        handler = decoderInstructionHandler dyntbl
    return (enc, handler)

qpackEncoder
    :: EncodeStrategy
    -> GCBuffer
    -> Int
    -> GCBuffer
    -> Int
    -> GCBuffer
    -> Int
    -> DynamicTable
    -> MVar ()
    -> TokenHeaderList
    -> IO (EncodedFieldSection, EncodedEncoderInstruction)
qpackEncoder stgy gcbuf1 bufsiz1 gcbuf2 bufsiz2 gcbuf3 bufsiz3 dyntbl lock ts =
    withMVar lock $ \_ ->
        withForeignPtr gcbuf1 $ \buf1 ->
            withForeignPtr gcbuf2 $ \buf2 ->
                withForeignPtr gcbuf3 $ \buf3 -> do
                    wbuf1 <- newWriteBuffer buf1 bufsiz1
                    wbuf2 <- newWriteBuffer buf2 bufsiz2
                    wbuf3 <- newWriteBuffer buf3 bufsiz3
                    thl <- encodeTokenHeader wbuf1 wbuf3 stgy dyntbl ts -- fixme: leftover
                    when (thl /= []) $ stdoutLogger "qpackEncoder: leftover"
                    hb0 <- toByteString wbuf1
                    ins <- toByteString wbuf3
                    encodePrefix wbuf2 dyntbl
                    prefix <- toByteString wbuf2
                    let hb = prefix `B.append` hb0
                    return (hb, ins)

decoderInstructionHandler
    :: DynamicTable -> (Int -> IO EncodedDecoderInstruction) -> IO ()
decoderInstructionHandler dyntbl recv = loop
  where
    loop = do
        _ <- getInsertionPoint dyntbl -- fixme
        bs <- recv 1024
        when (bs /= "") $ do
            (ins, leftover) <- decodeDecoderInstructions bs -- fixme: saving leftover
            when (leftover /= "") $ stdoutLogger "decoderInstructionHandler: leftover"
            qpackDebug dyntbl $ mapM_ print ins
            mapM_ handle ins
            loop
    handle (SectionAcknowledgement _n) = return ()
    handle (StreamCancellation _n) = return ()
    handle (InsertCountIncrement n)
        | n == 0 = E.throwIO DecoderInstructionError
        | otherwise = return ()

----------------------------------------------------------------

-- | Configuration for QPACK decoder.
data QDecoderConfig = QDecoderConfig
    { dcDynamicTableSize :: Size
    , dcHuffmanBufferSize :: Size
    }
    deriving (Show)

-- | Default configuration for QPACK decoder.
--
-- >>> defaultQDecoderConfig
-- QDecoderConfig {dcDynamicTableSize = 4096, dcHuffmanBufferSize = 4096}
defaultQDecoderConfig :: QDecoderConfig
defaultQDecoderConfig =
    QDecoderConfig
        { dcDynamicTableSize = 4096
        , dcHuffmanBufferSize = 4096
        }

-- | Creating a new QPACK decoder.
newQDecoder :: QDecoderConfig -> IO (QDecoder, EncoderInstructionHandler)
newQDecoder QDecoderConfig{..} = do
    dyntbl <- newDynamicTableForDecoding dcDynamicTableSize dcHuffmanBufferSize
    let dec = qpackDecoder dyntbl
        handler = encoderInstructionHandler dyntbl
    return (dec, handler)

-- | Creating a new simple QPACK decoder.
newQDecoderS
    :: QDecoderConfig -> Bool -> IO (QDecoderS, EncoderInstructionHandlerS)
newQDecoderS QDecoderConfig{..} debug = do
    dyntbl <- newDynamicTableForDecoding dcDynamicTableSize dcHuffmanBufferSize
    when debug $ setDebugQPACK dyntbl
    let dec = qpackDecoderS dyntbl
        handler = encoderInstructionHandlerS dyntbl
    return (dec, handler)

qpackDecoder :: DynamicTable -> EncodedFieldSection -> IO HeaderTable
qpackDecoder dyntbl bs = withReadBuffer bs $ \rbuf -> decodeTokenHeader dyntbl rbuf

qpackDecoderS :: DynamicTable -> EncodedFieldSection -> IO HeaderList
qpackDecoderS dyntbl bs = withReadBuffer bs $ \rbuf -> decodeTokenHeaderS dyntbl rbuf

encoderInstructionHandler
    :: DynamicTable -> (Int -> IO EncodedEncoderInstruction) -> IO ()
encoderInstructionHandler dyntbl recv = loop
  where
    loop = do
        bs <- recv 1024
        when (bs /= "") $ do
            encoderInstructionHandlerS dyntbl bs
            loop

encoderInstructionHandlerS :: DynamicTable -> EncodedEncoderInstruction -> IO ()
encoderInstructionHandlerS dyntbl bs = when (bs /= "") $ do
    (ins, leftover) <- decodeEncoderInstructions hufdec bs -- fixme: saving leftover
    when (leftover /= "") $ stdoutLogger "encoderInstructionHandler: leftover"

    qpackDebug dyntbl $ mapM_ print ins
    mapM_ handle ins
  where
    hufdec = getHuffmanDecoder dyntbl
    handle (SetDynamicTableCapacity n)
        | n > 4096 = E.throwIO EncoderInstructionError
        | otherwise = return ()
    handle (InsertWithNameReference ii val) = atomically $ do
        idx <- case ii of
            Left ai -> return $ SIndex ai
            Right ri -> do
                ip <- getInsertionPointSTM dyntbl
                return $ DIndex $ fromInsRelativeIndex ri ip
        ent0 <- toIndexedEntry dyntbl idx
        let ent = toEntryToken (entryToken ent0) val
        insertEntryToDecoder ent dyntbl
    handle (InsertWithoutNameReference t val) = atomically $ do
        let ent = toEntryToken t val
        insertEntryToDecoder ent dyntbl
    handle (Duplicate ri) = atomically $ do
        ip <- getInsertionPointSTM dyntbl
        let idx = DIndex $ fromInsRelativeIndex ri ip
        ent <- toIndexedEntry dyntbl idx
        insertEntryToDecoder ent dyntbl
