{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Network.QPACK.Table.Dynamic where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOArray)
import Data.Array.MArray (newArray)
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Imports
import Network.ByteOrder
import Network.HPACK.Internal (
    Entry,
    GCBuffer,
    HuffmanDecoder,
    Index,
    Size,
    decH,
    dummyEntry,
    maxNumbers,
 )
import Network.QPACK.Table.RevIndex
import Network.QPACK.Types

data Section = Section RequiredInsertCount [AbsoluteIndex]

{- FOURMOLU_DISABLE -}
data CodeInfo
    = EncodeInfo
        { revIndex            :: RevIndex -- Reverse index
        , requiredInsertCount :: IORef RequiredInsertCount
        , droppingPoint       :: IORef AbsoluteIndex
        , drainingPoint       :: IORef AbsoluteIndex
        , blockedStreams      :: IORef Int
        , knownReceivedCount  :: TVar Int
        , referenceCounters   :: IORef (IOArray Index Int)
        , sections            :: IORef (IntMap Section)
        }
    | DecodeInfo HuffmanDecoder
{- FOURMOLU_ENABLE -}

-- | Dynamic table for QPACK.
{- FOURMOLU_DISABLE -}
data DynamicTable = DynamicTable
    { codeInfo        :: CodeInfo
    , insertionPoint  :: TVar InsertionPoint
    , maxNumOfEntries :: TVar Int
    , circularTable   :: TVar Table
    , basePoint       :: IORef BasePoint
    , debugQPACK      :: IORef Bool
    , capaReady       :: IORef Bool
    , sendIns         :: ByteString -> IO ()
    }
{- FOURMOLU_ENABLE -}

type Table = TArray Index Entry

----------------------------------------------------------------

getBasePoint :: DynamicTable -> IO BasePoint
getBasePoint DynamicTable{..} = readIORef basePoint

setBasePointToInsersionPoint :: DynamicTable -> IO ()
setBasePointToInsersionPoint DynamicTable{..} = do
    InsertionPoint ip <- readTVarIO insertionPoint
    writeIORef basePoint $ BasePoint ip

getInsertionPoint :: DynamicTable -> IO InsertionPoint
getInsertionPoint DynamicTable{..} = readTVarIO insertionPoint

getInsertionPointSTM :: DynamicTable -> STM InsertionPoint
getInsertionPointSTM DynamicTable{..} = readTVar insertionPoint

checkRequiredInsertCount :: DynamicTable -> RequiredInsertCount -> IO ()
checkRequiredInsertCount DynamicTable{..} (RequiredInsertCount reqip) = atomically $ do
    InsertionPoint ip <- readTVar insertionPoint
    -- RequiredInsertCount is index + 1
    -- InsertionPoin is index + 1
    -- So, equal is necessary.
    check (reqip <= ip)

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | Creating 'DynamicTable' for encoding.
newDynamicTableForEncoding
    :: (ByteString -> IO ())
    -> IO DynamicTable
newDynamicTableForEncoding sendEI = do
    arr <- newArray (0, 0) 0
    info <- do
        revIndex            <- newRevIndex
        requiredInsertCount <- newIORef 0
        droppingPoint       <- newIORef 0
        drainingPoint       <- newIORef 0
        blockedStreams      <- newIORef 0
        knownReceivedCount  <- newTVarIO 0
        referenceCounters   <- newIORef arr
        sections            <- newIORef IntMap.empty
        return EncodeInfo{..}
    newDynamicTable info sendEI
{- FOURMOLU_ENABLE -}

-- | Creating 'DynamicTable' for decoding.
newDynamicTableForDecoding
    :: Size
    -- ^ The size of temporary buffer for Huffman decoding
    -> (ByteString -> IO ())
    -> IO DynamicTable
newDynamicTableForDecoding huftmpsiz sendDI = do
    gcbuf <- mallocPlainForeignPtrBytes huftmpsiz
    tvar <- newTVarIO $ Just (gcbuf, huftmpsiz)
    let decoder = decodeHLock tvar
        info = DecodeInfo decoder
    newDynamicTable info sendDI

decodeHLock
    :: TVar (Maybe (GCBuffer, Int)) -> ReadBuffer -> Int -> IO ByteString
decodeHLock tvar rbuf len = E.bracket lock unlock $ \(gcbuf, bufsiz) ->
    withForeignPtr gcbuf $ \buf -> do
        wbuf <- newWriteBuffer buf bufsiz
        decH wbuf rbuf len
        toByteString wbuf
  where
    lock = atomically $ do
        mx <- readTVar tvar
        case mx of
            Nothing -> retry
            Just x -> do
                writeTVar tvar Nothing
                return x
    unlock x = atomically $ writeTVar tvar $ Just x

newDynamicTable :: CodeInfo -> (ByteString -> IO ()) -> IO DynamicTable
newDynamicTable info send = do
    tbl <- atomically $ newArray (0, 0) dummyEntry
    let codeInfo = info
    insertionPoint <- newTVarIO 0
    maxNumOfEntries <- newTVarIO 0
    circularTable <- newTVarIO tbl
    basePoint <- newIORef 0
    debugQPACK <- newIORef False
    capaReady <- newIORef False
    let sendIns = send
    return DynamicTable{..}

updateDynamicTable :: DynamicTable -> Size -> IO ()
updateDynamicTable DynamicTable{..} maxsiz = do
    tbl <- atomically $ newArray (0, end) dummyEntry
    atomically $ do
        writeTVar maxNumOfEntries maxN
        writeTVar circularTable tbl
    case codeInfo of
        EncodeInfo{..} -> do
            arr <- newArray (0, end) 0
            writeIORef referenceCounters arr
        _ -> return ()
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

----------------------------------------------------------------

setDebugQPACK :: DynamicTable -> IO ()
setDebugQPACK DynamicTable{..} = writeIORef debugQPACK True

getDebugQPACK :: DynamicTable -> IO Bool
getDebugQPACK DynamicTable{..} = readIORef debugQPACK

qpackDebug :: DynamicTable -> IO () -> IO ()
qpackDebug DynamicTable{..} action = do
    debug <- readIORef debugQPACK
    when debug action

----------------------------------------------------------------

getMaxNumOfEntries :: DynamicTable -> IO Int
getMaxNumOfEntries DynamicTable{..} = readTVarIO maxNumOfEntries

----------------------------------------------------------------

{-# INLINE getRevIndex #-}
getRevIndex :: DynamicTable -> RevIndex
getRevIndex DynamicTable{..} = revIndex
  where
    EncodeInfo{..} = codeInfo

getHuffmanDecoder :: DynamicTable -> HuffmanDecoder
getHuffmanDecoder DynamicTable{..} = huf
  where
    DecodeInfo huf = codeInfo

----------------------------------------------------------------

clearRequiredInsertCount :: DynamicTable -> IO ()
clearRequiredInsertCount DynamicTable{..} = writeIORef requiredInsertCount 0
  where
    EncodeInfo{..} = codeInfo

getRequiredInsertCount :: DynamicTable -> IO RequiredInsertCount
getRequiredInsertCount DynamicTable{..} = readIORef requiredInsertCount
  where
    EncodeInfo{..} = codeInfo

absoluteIndexToRequiredInsertCount :: AbsoluteIndex -> RequiredInsertCount
absoluteIndexToRequiredInsertCount (AbsoluteIndex idx) =
    RequiredInsertCount (idx + 1)

updateRequiredInsertCount :: DynamicTable -> AbsoluteIndex -> IO ()
updateRequiredInsertCount DynamicTable{..} aidx = do
    let newric = absoluteIndexToRequiredInsertCount aidx
    oldric <- readIORef requiredInsertCount
    when (newric > oldric) $ writeIORef requiredInsertCount newric
  where
    EncodeInfo{..} = codeInfo

----------------------------------------------------------------

insertEntryToEncoder :: Entry -> DynamicTable -> IO AbsoluteIndex
insertEntryToEncoder ent dyntbl@DynamicTable{..} = do
    InsertionPoint insp <- atomically $ do
        x <- readTVar insertionPoint
        writeTVar insertionPoint (x + 1)
        return x
    maxN <- readTVarIO maxNumOfEntries
    let i = insp `mod` maxN
    table <- readTVarIO circularTable
    atomically $ unsafeWrite table i ent
    let revtbl = getRevIndex dyntbl
    let ai = AbsoluteIndex insp
    insertRevIndex ent (DIndex ai) revtbl
    return ai

insertEntryToDecoder :: Entry -> DynamicTable -> STM ()
insertEntryToDecoder ent DynamicTable{..} = do
    x@(InsertionPoint insp) <- readTVar insertionPoint
    writeTVar insertionPoint (x + 1)
    maxN <- readTVar maxNumOfEntries
    let i = insp `mod` maxN
    table <- readTVar circularTable
    unsafeWrite table i ent

toDynamicEntry :: DynamicTable -> AbsoluteIndex -> STM Entry
toDynamicEntry DynamicTable{..} (AbsoluteIndex idx) = do
    maxN <- readTVar maxNumOfEntries
    let i = idx `mod` maxN
    table <- readTVar circularTable
    unsafeRead table i

----------------------------------------------------------------

setTableCapacity :: DynamicTable -> Int -> IO ()
setTableCapacity dyntbl@DynamicTable{..} n = do
    updateDynamicTable dyntbl n
    writeIORef capaReady True

isTableReady :: DynamicTable -> IO Bool
isTableReady DynamicTable{..} = readIORef capaReady

setTableStreamsBlocked :: DynamicTable -> Int -> IO ()
setTableStreamsBlocked DynamicTable{..} n = writeIORef blockedStreams n
  where
    EncodeInfo{..} = codeInfo

setKnownReceivedCount :: DynamicTable -> Int -> IO ()
setKnownReceivedCount DynamicTable{..} n =
    atomically $ modifyTVar' knownReceivedCount (+ n)
  where
    EncodeInfo{..} = codeInfo
