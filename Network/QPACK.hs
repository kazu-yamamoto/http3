{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Thread-safe QPACK encoder/decoder.
module Network.QPACK (
    -- * Encoder
    QEncoderConfig (..),
    defaultQEncoderConfig,
    QEncoder,
    newQEncoder,
    TableOperation (..),

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

    -- * Re-exports
    TokenHeaderTable,
    TokenHeaderList,
    ValueTable,
    Header,
    getFieldValue,
    toTokenHeaderTable,
    original,
    foldedCase,
    mk,
) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString as B
import Data.CaseInsensitive
import Network.ByteOrder
import Network.HPACK.Internal (
    GCBuffer,
    Size,
    entryToken,
    toEntryToken,
    toTokenHeaderTable,
 )
import Network.HTTP.Types
import Network.QUIC.Internal (StreamId, stdoutLogger)

import Imports
import Network.QPACK.Error
import Network.QPACK.HeaderBlock
import Network.QPACK.Instruction
import Network.QPACK.Table
import Network.QPACK.Types

----------------------------------------------------------------

-- | QPACK encoder.
type QEncoder = TokenHeaderList -> IO EncodedFieldSection

-- | QPACK decoder.
type QDecoder = StreamId -> EncodedFieldSection -> IO TokenHeaderTable

-- | QPACK simple decoder.
type QDecoderS = StreamId -> EncodedFieldSection -> IO [Header]

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

data TableOperation = TableOperation
    { setCapacity :: Int -> IO ()
    , setBlockedStreams :: Int -> IO ()
    }

----------------------------------------------------------------

-- | Configuration for QPACK encoder.
data QEncoderConfig = QEncoderConfig
    { ecDynamicTableSize :: Size
    , ecHeaderBlockBufferSize :: Size
    , ecPrefixBufferSize :: Size
    , ecInstructionBufferSize :: Size
    }
    deriving (Show)

-- | Default configuration for QPACK encoder.
--
-- >>> defaultQEncoderConfig
-- QEncoderConfig {ecDynamicTableSize = 4096, ecHeaderBlockBufferSize = 4096, ecPrefixBufferSize = 128, ecInstructionBufferSize = 4096}
defaultQEncoderConfig :: QEncoderConfig
defaultQEncoderConfig =
    QEncoderConfig
        { ecDynamicTableSize = 4096
        , ecHeaderBlockBufferSize = 4096
        , ecPrefixBufferSize = 128
        , ecInstructionBufferSize = 4096
        }

-- | Creating a new QPACK encoder.
newQEncoder
    :: QEncoderConfig
    -> (EncodedEncoderInstruction -> IO ())
    -> IO (QEncoder, DecoderInstructionHandler, TableOperation)
newQEncoder QEncoderConfig{..} sendEI = do
    let bufsiz1 = ecHeaderBlockBufferSize
        bufsiz2 = ecPrefixBufferSize
        bufsiz3 = ecInstructionBufferSize
    gcbuf1 <- mallocPlainForeignPtrBytes bufsiz1
    gcbuf2 <- mallocPlainForeignPtrBytes bufsiz2
    gcbuf3 <- mallocPlainForeignPtrBytes bufsiz3
    dyntbl <- newDynamicTableForEncoding sendEI
    lock <- newMVar ()
    let enc =
            qpackEncoder
                gcbuf1
                bufsiz1
                gcbuf2
                bufsiz2
                gcbuf3
                bufsiz3
                dyntbl
                lock
        handler = decoderInstructionHandler dyntbl
        ctl =
            TableOperation
                { setCapacity = \n -> do
                    let tableSize = min ecDynamicTableSize n
                    setTableCapacity dyntbl tableSize
                    ins <- encodeEncoderInstructions [SetDynamicTableCapacity tableSize] False
                    sendIns dyntbl ins
                , setBlockedStreams = setTableStreamsBlocked dyntbl
                }
    return (enc, handler, ctl)

qpackEncoder
    :: GCBuffer
    -> Int
    -> GCBuffer
    -> Int
    -> GCBuffer
    -> Int
    -> DynamicTable
    -> MVar ()
    -> TokenHeaderList
    -> IO EncodedFieldSection
qpackEncoder gcbuf1 bufsiz1 gcbuf2 bufsiz2 gcbuf3 bufsiz3 dyntbl lock ts =
    withMVar lock $ \_ ->
        withForeignPtr gcbuf1 $ \buf1 ->
            withForeignPtr gcbuf2 $ \buf2 ->
                withForeignPtr gcbuf3 $ \buf3 -> do
                    wbuf1 <- newWriteBuffer buf1 bufsiz1
                    wbuf2 <- newWriteBuffer buf2 bufsiz2
                    wbuf3 <- newWriteBuffer buf3 bufsiz3
                    thl <- encodeTokenHeader wbuf1 wbuf3 dyntbl ts -- fixme: leftover
                    when (thl /= []) $ stdoutLogger "qpackEncoder: leftover"
                    hb0 <- toByteString wbuf1
                    ins <- toByteString wbuf3
                    when (ins /= "") $ sendIns dyntbl ins
                    encodePrefix wbuf2 dyntbl
                    prefix <- toByteString wbuf2
                    let hb = prefix `B.append` hb0
                    return hb

-- Note: dyntbl for encoder
decoderInstructionHandler :: DynamicTable -> DecoderInstructionHandler
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
    -- FIXME: updating dynamic table for encoder
    handle (SectionAcknowledgement _n) = return ()
    handle (StreamCancellation _n) = return ()
    handle (InsertCountIncrement n)
        | n == 0 = E.throwIO DecoderInstructionError
        | otherwise = setKnownReceivedCount dyntbl n

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
newQDecoder
    :: QDecoderConfig
    -> (EncodedDecoderInstruction -> IO ())
    -> IO (QDecoder, EncoderInstructionHandler)
newQDecoder QDecoderConfig{..} sendDI = do
    dyntbl <-
        newDynamicTableForDecoding dcHuffmanBufferSize sendDI
    let dec = qpackDecoder dyntbl
        handler = encoderInstructionHandler dcDynamicTableSize dyntbl
    return (dec, handler)

-- | Creating a new simple QPACK decoder.
newQDecoderS
    :: QDecoderConfig
    -> (EncodedDecoderInstruction -> IO ())
    -> Bool
    -> IO (QDecoderS, EncoderInstructionHandlerS)
newQDecoderS QDecoderConfig{..} sendDI debug = do
    dyntbl <-
        newDynamicTableForDecoding dcHuffmanBufferSize sendDI
    when debug $ setDebugQPACK dyntbl
    let dec = qpackDecoderS dyntbl
        handler = encoderInstructionHandlerS dcDynamicTableSize dyntbl
    return (dec, handler)

qpackDecoder
    :: DynamicTable -> StreamId -> EncodedFieldSection -> IO TokenHeaderTable
qpackDecoder dyntbl sid bs = do
    (tbl, needAck) <- withReadBuffer bs $ \rbuf -> decodeTokenHeader dyntbl rbuf
    when needAck $
        encodeDecoderInstructions [SectionAcknowledgement sid] >>= sendIns dyntbl
    return tbl

qpackDecoderS :: DynamicTable -> StreamId -> EncodedFieldSection -> IO [Header]
qpackDecoderS dyntbl sid bs = do
    (hs, needAck) <- withReadBuffer bs $ \rbuf -> decodeTokenHeaderS dyntbl rbuf
    when needAck $
        encodeDecoderInstructions [SectionAcknowledgement sid] >>= sendIns dyntbl
    return hs

-- Note: dyntbl for decoder
encoderInstructionHandler :: Int -> DynamicTable -> EncoderInstructionHandler
encoderInstructionHandler decCapLim dyntbl recv = loop
  where
    loop = do
        bs <- recv 1024
        when (bs /= "") $ do
            encoderInstructionHandlerS decCapLim dyntbl bs
            loop
{- FOURMOLU_DISABLE -}
-- Note: dyntbl for decoder
encoderInstructionHandlerS :: Int -> DynamicTable -> EncoderInstructionHandlerS
encoderInstructionHandlerS _ _dyntbl "" = return ()
encoderInstructionHandlerS decCapLim dyntbl bs = do
    (ins, leftover) <- decodeEncoderInstructions hufdec bs -- fixme: saving leftover
    when (leftover /= "") $ stdoutLogger "encoderInstructionHandler: leftover"

    qpackDebug dyntbl $ mapM_ print ins
    mapM_ handle ins
  where
    hufdec = getHuffmanDecoder dyntbl
    handle (SetDynamicTableCapacity n)
        | n > decCapLim = E.throwIO EncoderInstructionError
        | otherwise = setTableCapacity dyntbl n
    handle (InsertWithNameReference ii val) = do
        -- XXX Checking ready
        atomically $ do
            idx <- case ii of
                Left ai -> return $ SIndex ai
                Right ri -> do
                    ip <- getInsertionPointSTM dyntbl
                    return $ DIndex $ fromInsRelativeIndex ri ip
            ent0 <- toIndexedEntry dyntbl idx
            let ent = toEntryToken (entryToken ent0) val
            insertEntryToDecoder ent dyntbl
        -- encodeDecoderInstructions [InsertCountIncrement 1] >>= getSendDI dyntbl
    handle (InsertWithLiteralName t val) = do
        -- XXX Checking ready
        atomically $ do
            let ent = toEntryToken t val
            insertEntryToDecoder ent dyntbl
       -- encodeDecoderInstructions [InsertCountIncrement 1] >>= getSendDI dyntbl
    handle (Duplicate ri) = do
        atomically $ do
            ip <- getInsertionPointSTM dyntbl
            let idx = DIndex $ fromInsRelativeIndex ri ip
            ent <- toIndexedEntry dyntbl idx
            insertEntryToDecoder ent dyntbl
        -- encodeDecoderInstructions [InsertCountIncrement 1] >>= getSendDI dyntbl
{- FOURMOLU_ENABLE -}
