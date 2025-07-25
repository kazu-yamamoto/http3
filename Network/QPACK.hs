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

    -- ** Encoder for debugging
    QEncoderS,
    newQEncoderS,

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
import Data.CaseInsensitive hiding (map)
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
import Network.QUIC.Internal (StreamId)

import Imports
import Network.QPACK.Error
import Network.QPACK.HeaderBlock
import Network.QPACK.Instruction
import Network.QPACK.Table
import Network.QPACK.Types

----------------------------------------------------------------

-- | QPACK encoder.
type QEncoder = StreamId -> TokenHeaderList -> IO EncodedFieldSection

-- | QPACK simple encoder.
type QEncoderS = StreamId -> [Header] -> IO EncodedFieldSection

-- | QPACK decoder.
type QDecoder = StreamId -> EncodedFieldSection -> IO TokenHeaderTable

-- | QPACK simple decoder.
type QDecoderS = StreamId -> EncodedFieldSection -> IO (Maybe [Header])

-- | Encoder instruction handler.
type EncoderInstructionHandler = (Int -> IO EncodedEncoderInstruction) -> IO ()

-- | Simple encoder instruction handler.
--   Leftover is returned.
type EncoderInstructionHandlerS =
    EncodedEncoderInstruction -> IO EncodedEncoderInstruction

-- | Encoded decoder instruction.
type EncodedDecoderInstruction = ByteString

-- | Decoder instruction handler.
type DecoderInstructionHandler = (Int -> IO EncodedDecoderInstruction) -> IO ()

-- | A type to integrating handlers.
type InstructionHandler = (Int -> IO ByteString) -> IO ()

data TableOperation = TableOperation
    { setCapacity :: Int -> IO ()
    , setBlockedStreams :: Int -> IO ()
    , setHeaderSize :: Int -> IO ()
    }

----------------------------------------------------------------

-- | Configuration for QPACK encoder.
data QEncoderConfig = QEncoderConfig
    { ecMaxTableCapacity :: Size
    , ecHeaderBlockBufferSize :: Size
    , ecInstructionBufferSize :: Size
    }
    deriving (Show)

-- | Default configuration for QPACK encoder.
--
-- >>> defaultQEncoderConfig
-- QEncoderConfig {ecMaxTableCapacity = 4096, ecHeaderBlockBufferSize = 4096, ecInstructionBufferSize = 4096}
defaultQEncoderConfig :: QEncoderConfig
defaultQEncoderConfig =
    QEncoderConfig
        { ecMaxTableCapacity = 4096
        , ecHeaderBlockBufferSize = 4096
        , ecInstructionBufferSize = 4096
        }

-- | Creating a new QPACK encoder.
newQEncoder
    :: QEncoderConfig
    -> (EncodedEncoderInstruction -> IO ())
    -> IO (QEncoder, DecoderInstructionHandler, TableOperation)
newQEncoder QEncoderConfig{..} sendEI = do
    let bufsiz1 = ecHeaderBlockBufferSize
        bufsiz2 = ecInstructionBufferSize
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
                    -- "n" is decoder-proposed size via settings.
                    let tableSize = min ecMaxTableCapacity n
                    setTableCapacity dyntbl tableSize
                    ins <- encodeEncoderInstructions [SetDynamicTableCapacity tableSize] False
                    sendIns dyntbl ins
                , setBlockedStreams = setMaxBlockedStreams dyntbl
                , setHeaderSize = setMaxHeaderSize dyntbl
                }
    return (enc, handler, ctl)

tokenHeaderSize :: TokenHeader -> Int
tokenHeaderSize (t, v) = BS.length (CI.original (tokenKey t)) + BS.length v + 8 -- adhoc overhead

split :: Int -> TokenHeaderList -> (TokenHeaderList, TokenHeaderList)
split lim ts = split' 0 ts
  where
    split' _ [] = ([], [])
    split' s xxs@(x : xs)
        | siz > lim = E.throw BufferOverrun
        | s' < lim =
            let (ys, zs) = split' s' xs
             in (x : ys, zs)
        | otherwise = ([], xxs)
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
    -> QEncoder
qpackEncoder gcbuf1 bufsiz1 gcbuf2 bufsiz2 dyntbl lock sid ts =
    withMVar lock $ \_ ->
        withForeignPtr gcbuf1 $ \buf1 ->
            withForeignPtr gcbuf2 $ \buf2 -> do
                siz <- getTableCapacity dyntbl
                qpackDebug dyntbl $
                    putStrLn $
                        "---- Stream " ++ show sid ++ " " ++ "tblsiz: " ++ show siz
                setBasePointToInsersionPoint dyntbl
                clearRequiredInsertCount dyntbl
                let tss = splitThrough bufsiz1 ts
                his <- mapM (qpackEncodeHeader buf1 bufsiz1 buf2 bufsiz2 dyntbl) tss
                let (hbs, daiss) = unzip his
                prefix <- qpackEncodePrefix buf1 bufsiz1 dyntbl
                let section = BS.concat (prefix : hbs)
                reqInsCnt <- getRequiredInsertCount dyntbl
                -- To count only blocked sections,
                -- dont' register this section if reqInsCnt == 0.
                when (reqInsCnt /= 0) $
                    insertSection dyntbl sid $
                        Section reqInsCnt $
                            concat daiss
                return section

qpackEncoderS
    :: GCBuffer
    -> Int
    -> GCBuffer
    -> Int
    -> DynamicTable
    -> MVar ()
    -> QEncoderS
qpackEncoderS gcbuf1 bufsiz1 gcbuf2 bufsiz2 dyntbl lock sid hs =
    withMVar lock $ \_ ->
        withForeignPtr gcbuf1 $ \buf1 ->
            withForeignPtr gcbuf2 $ \buf2 -> do
                siz <- getTableCapacity dyntbl
                qpackDebug dyntbl $
                    putStrLn $
                        "---- Stream " ++ show sid ++ " " ++ "tblsiz: " ++ show siz
                setBasePointToInsersionPoint dyntbl
                clearRequiredInsertCount dyntbl
                let tss = splitThrough bufsiz1 ts
                his <- mapM (qpackEncodeHeader buf1 bufsiz1 buf2 bufsiz2 dyntbl) tss
                let (hbs, daiss) = unzip his
                prefix <- qpackEncodePrefix buf1 bufsiz1 dyntbl
                let section = BS.concat (prefix : hbs)
                reqInsCnt <- getRequiredInsertCount dyntbl
                -- To count only blocked sections,
                -- dont' register this section if reqInsCnt == 0.
                immAck <- getImmediateAck dyntbl
                when (reqInsCnt /= 0) $ do
                    blocked <- wouldSectionBeBlocked dyntbl reqInsCnt
                    when blocked $ insertBlockedStreamE dyntbl sid
                    let dais = concat daiss
                    insertSection dyntbl sid $ Section reqInsCnt dais
                    when immAck $ do
                        -- The same logic of SectionAcknowledgement.
                        updateKnownReceivedCount dyntbl reqInsCnt
                        mapM_ (decreaseReference dyntbl) dais
                        deleteBlockedStreamE dyntbl sid
                -- Need to emulate InsertCountIncrement since
                -- SectionAcknowledgement is not returned if
                -- RequiredInsertCount is 0.
                when immAck $ setInsersionPointToKnownReceivedCount dyntbl
                return section
  where
    mk' (k, v) = (t, v)
      where
        t = toToken $ foldedCase k
    ts = map mk' hs

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
decoderInstructionHandler dyntbl recv = loop ""
  where
    loop bs0 = do
        bs1 <- recv 1024
        let bs
                | bs0 == "" = bs1
                | otherwise = bs0 <> bs1
        when (bs /= "") $ do
            (ins, leftover) <- decodeDecoderInstructions bs
            qpackDebug dyntbl $ mapM_ print ins
            mapM_ handle ins
            loop leftover
    handle (SectionAcknowledgement sid) = do
        msec <- getAndDelSection dyntbl sid
        case msec of
            Nothing -> E.throwIO DecoderInstructionError
            Just (Section reqInsCnt ais) -> do
                updateKnownReceivedCount dyntbl reqInsCnt
                mapM_ (decreaseReference dyntbl) ais
                deleteBlockedStreamE dyntbl sid
    handle (StreamCancellation _n) = return () -- fixme
    handle (InsertCountIncrement n)
        | n == 0 = E.throwIO DecoderInstructionError
        | otherwise = incrementKnownReceivedCount dyntbl n

----------------------------------------------------------------

newQEncoderS
    :: QEncoderConfig -- capacity
    -> (EncodedEncoderInstruction -> IO ())
    -> Int -- blocked stream
    -> Bool -- immediate Acks
    -> Bool -- debug
    -> IO QEncoderS
newQEncoderS QEncoderConfig{..} saveEI blocked immediateAck debug = do
    let bufsiz1 = ecHeaderBlockBufferSize
        bufsiz2 = ecInstructionBufferSize
    gcbuf1 <- mallocPlainForeignPtrBytes bufsiz1
    gcbuf2 <- mallocPlainForeignPtrBytes bufsiz2
    dyntbl <- newDynamicTableForEncoding saveEI
    setTableCapacity dyntbl ecMaxTableCapacity
    setMaxBlockedStreams dyntbl blocked
    setImmediateAck dyntbl immediateAck
    setDebugQPACK dyntbl debug
    lock <- newMVar ()
    let enc =
            qpackEncoderS
                gcbuf1
                bufsiz1
                gcbuf2
                bufsiz2
                dyntbl
                lock
    return enc

----------------------------------------------------------------

-- | Configuration for QPACK decoder.
data QDecoderConfig = QDecoderConfig
    { dcMaxTableCapacity :: Size
    , dcHuffmanBufferSize :: Size -- for encoder insteruction handler
    , dcBlockedSterams :: Int
    , dcMaxFieldSectionSize :: Int
    }
    deriving (Show)

-- | Default configuration for QPACK decoder.
--
-- >>> defaultQDecoderConfig
-- QDecoderConfig {dcMaxTableCapacity = 4096, dcHuffmanBufferSize = 2048, dcBlockedSterams = 100, dcMaxFieldSectionSize = 32768}
defaultQDecoderConfig :: QDecoderConfig
defaultQDecoderConfig =
    QDecoderConfig
        { dcMaxTableCapacity = 4096
        , dcHuffmanBufferSize = 2048 -- no global locking
        , dcBlockedSterams = 100
        , dcMaxFieldSectionSize = 32768
        }

-- | Creating a new QPACK decoder.
newQDecoder
    :: QDecoderConfig
    -> (EncodedDecoderInstruction -> IO ())
    -> IO (QDecoder, EncoderInstructionHandler)
newQDecoder QDecoderConfig{..} sendDI = do
    dyntbl <-
        newDynamicTableForDecoding dcHuffmanBufferSize sendDI
    setMaxBlockedStreams dyntbl dcBlockedSterams
    let dec = qpackDecoder dyntbl
        handler = encoderInstructionHandler dcMaxTableCapacity dyntbl
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
    setMaxBlockedStreams dyntbl dcBlockedSterams
    setDebugQPACK dyntbl debug
    let dec = qpackDecoderS dyntbl
        handler = encoderInstructionHandlerS dcMaxTableCapacity dyntbl
    return (dec, handler)

qpackDecoder
    :: DynamicTable -> StreamId -> EncodedFieldSection -> IO TokenHeaderTable
qpackDecoder dyntbl sid bs = do
    (tbl, needAck) <- withReadBuffer bs $ \rbuf -> decodeTokenHeader dyntbl rbuf
    when needAck $
        encodeDecoderInstructions [SectionAcknowledgement sid] >>= sendIns dyntbl
    return tbl

qpackDecoderS
    :: DynamicTable -> StreamId -> EncodedFieldSection -> IO (Maybe [Header])
qpackDecoderS dyntbl sid bs = do
    qpackDebug dyntbl $ putStrLn $ "---- Stream " ++ show sid
    mhs <- withReadBuffer bs $ \rbuf -> decodeTokenHeaderS dyntbl rbuf
    case mhs of
        Nothing -> return Nothing
        Just (hs, needAck) -> do
            when needAck $
                encodeDecoderInstructions [SectionAcknowledgement sid] >>= sendIns dyntbl
            return $ Just hs

-- Note: dyntbl for decoder
encoderInstructionHandler :: Int -> DynamicTable -> EncoderInstructionHandler
encoderInstructionHandler decCapLim dyntbl recv = loop ""
  where
    loop bs0 = do
        bs1 <- recv 1024
        let bs
                | bs0 == "" = bs1
                | otherwise = bs0 <> bs1
        when (bs /= "") $ do
            leftover <- encoderInstructionHandlerS decCapLim dyntbl bs
            loop leftover

-- Note: dyntbl for decoder
encoderInstructionHandlerS :: Int -> DynamicTable -> EncoderInstructionHandlerS
encoderInstructionHandlerS _ _dyntbl "" = return ""
encoderInstructionHandlerS decCapLim dyntbl bs = do
    (ins, leftover) <- decodeEncoderInstructions hufdec bs
    cnt <- sum <$> mapM handle ins
    when (cnt /= 0) $
        encodeDecoderInstructions [InsertCountIncrement cnt] >>= sendIns dyntbl
    return leftover
  where
    hufdec = getHuffmanDecoder dyntbl -- only for encoder instruction handler
    handle ins@(SetDynamicTableCapacity n)
        | n > decCapLim = E.throwIO EncoderInstructionError
        | otherwise = do
            setTableCapacity dyntbl n
            qpackDebug dyntbl $ print ins
            return 0
    handle ins@(InsertWithNameReference ii val) = do
        ready <- isTableReady dyntbl
        unless ready $ E.throwIO EncoderInstructionError
        dai <- atomically $ do
            idx <- case ii of
                Left ai -> return $ SIndex ai
                Right ri -> do
                    ip <- getInsertionPointSTM dyntbl
                    return $ DIndex $ fromInsRelativeIndex ri ip
            ent0 <- toIndexedEntry dyntbl idx
            let ent = toEntryToken (entryToken ent0) val
            _ <- insertEntryToDecoder ent dyntbl
            return idx
        qpackDebug dyntbl $ putStrLn $ show ins ++ ": " ++ show dai
        return 1
    handle ins@(InsertWithLiteralName t val) = do
        ready <- isTableReady dyntbl
        unless ready $ E.throwIO EncoderInstructionError
        dai <- atomically $ do
            let ent = toEntryToken t val
            insertEntryToDecoder ent dyntbl
        qpackDebug dyntbl $ putStrLn $ show ins ++ ": " ++ show dai
        return 1
    handle ins@(Duplicate ri) = do
        ready <- isTableReady dyntbl
        unless ready $ E.throwIO EncoderInstructionError
        (dai, dai') <- atomically $ do
            ip <- getInsertionPointSTM dyntbl
            let ai = fromInsRelativeIndex ri ip
                idx = DIndex ai
            ent <- toIndexedEntry dyntbl idx
            ai' <- insertEntryToDecoder ent dyntbl
            return (ai, ai')
        qpackDebug dyntbl $
            putStrLn $
                show ins ++ ": " ++ show dai ++ " -> " ++ show dai'
        return 1
