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
import qualified Data.ByteString as BS
import Data.CaseInsensitive
import qualified Data.CaseInsensitive as CI
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
type QEncoder = StreamId -> TokenHeaderList -> IO EncodedFieldSection

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
    gcbuf1 <- mallocPlainForeignPtrBytes bufsiz1
    gcbuf2 <- mallocPlainForeignPtrBytes bufsiz2
    dyntbl <- newDynamicTableForEncoding sendEI
    lock <- newMVar ()
    let enc =
            qpackEncoder
                gcbuf1
                bufsiz1
                gcbuf2
                bufsiz2
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

tokenHeaderSize :: TokenHeader -> Int
tokenHeaderSize (t, v) = BS.length (CI.original (tokenKey t)) + BS.length v + 8 -- adhoc overhead

split :: Int -> TokenHeaderList -> (TokenHeaderList, TokenHeaderList)
split lim ts = split' 0 ts
  where
    split' _ [] = ([], [])
    split' s (x : xs)
        | siz > lim = E.throw BufferOverrun
        | s' < lim = ([x], xs)
        | otherwise =
            let (ys, zs) = split' s' xs
             in (x : ys, zs)
      where
        siz = tokenHeaderSize x
        s' = s + siz

splitThrough :: Int -> TokenHeaderList -> [TokenHeaderList]
splitThrough lim ts0 = loop ts0 id
  where
    loop [] builder = builder []
    loop ts builder = loop ts2 (builder . (ts1 :))
      where
        (ts1, ts2) = split lim ts

qpackEncoder
    :: GCBuffer
    -> Int
    -> GCBuffer
    -> Int
    -> DynamicTable
    -> MVar ()
    -> StreamId
    -> TokenHeaderList
    -> IO EncodedFieldSection
qpackEncoder gcbuf1 bufsiz1 gcbuf2 bufsiz2 dyntbl lock sid ts =
    withMVar lock $ \_ ->
        withForeignPtr gcbuf1 $ \buf1 ->
            withForeignPtr gcbuf2 $ \buf2 -> do
                let tss = splitThrough bufsiz1 ts
                his <- mapM (qpackEncodeHeader buf1 bufsiz1 buf2 bufsiz2 dyntbl) tss
                let (hbs, daiss) = unzip his
                prefix <- qpackEncodePrefix buf1 bufsiz1 dyntbl
                let section = BS.concat (prefix : hbs)
                reqInsCnt <- getRequiredInsertCount dyntbl
                insertSection dyntbl sid $ Section reqInsCnt $ concat daiss
                return section

qpackEncodeHeader
    :: Buffer
    -> BufferSize
    -> Buffer
    -> BufferSize
    -> DynamicTable
    -> TokenHeaderList
    -> IO (ByteString, [AbsoluteIndex])
qpackEncodeHeader buf1 bufsiz1 buf2 bufsiz2 dyntbl ts = do
    wbuf1 <- newWriteBuffer buf1 bufsiz1
    wbuf2 <- newWriteBuffer buf2 bufsiz2
    dais <- encodeTokenHeader wbuf1 wbuf2 dyntbl ts
    hb <- toByteString wbuf1
    ins <- toByteString wbuf2
    when (ins /= "") $ sendIns dyntbl ins
    return (hb, dais)

qpackEncodePrefix :: Buffer -> BufferSize -> DynamicTable -> IO ByteString
qpackEncodePrefix buf1 bufsiz1 dyntbl = do
    wbuf1 <- newWriteBuffer buf1 bufsiz1
    encodePrefix wbuf1 dyntbl
    toByteString wbuf1

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
    handle (SectionAcknowledgement sid) = do
        msec <- getAndDelSection dyntbl sid
        case msec of
            Nothing -> undefined -- XXX error
            Just (Section reqInsCnt ais) -> do
                updateKnownReceivedCount dyntbl reqInsCnt
                mapM_ (decreaseReference dyntbl) ais
    handle (StreamCancellation _n) = return ()
    handle (InsertCountIncrement n)
        | n == 0 = E.throwIO DecoderInstructionError
        | otherwise = incrementKnownReceivedCount dyntbl n

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
