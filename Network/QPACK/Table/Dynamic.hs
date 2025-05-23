{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Network.QPACK.Table.Dynamic where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOArray, newArray)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Imports
import Network.ByteOrder
import Network.Control
import Network.HPACK.Internal (
    Entry,
    GCBuffer,
    HuffmanDecoder,
    Index,
    Size,
    decH,
    dummyEntry,
    entryFieldValue,
    entryHeaderName,
    entrySize,
    maxNumbers,
 )
import System.IO.Unsafe (unsafePerformIO)

import Network.QPACK.Table.RevIndex
import Network.QPACK.Types
import Network.QUIC (StreamId)

data Section = Section RequiredInsertCount [AbsoluteIndex]

{- FOURMOLU_DISABLE -}
data CodeInfo
    = EncodeInfo
        { revIndex            :: RevIndex -- Reverse index
        , requiredInsertCount :: IORef RequiredInsertCount
        , droppingPoint       :: IORef AbsoluteIndex
        , drainingPoint       :: IORef AbsoluteIndex
        , knownReceivedCount  :: TVar Int
        , referenceCounters   :: IORef (IOArray Index Int)
        , sections            :: IORef (IntMap Section)
        , lruCache            :: LRUCacheRef FieldName FieldValue
        }
    | DecodeInfo
        { huffmanDecoder :: HuffmanDecoder
        , blockedStreams :: IORef Int
        }
{- FOURMOLU_ENABLE -}

-- | Dynamic table for QPACK.
{- FOURMOLU_DISABLE -}
data DynamicTable = DynamicTable
    { codeInfo          :: CodeInfo
    , insertionPoint    :: TVar InsertionPoint
    , maxNumOfEntries   :: TVar Int
    , circularTable     :: TVar Table
    , basePoint         :: IORef BasePoint
    , debugQPACK        :: IORef Bool
    , capaReady         :: IORef Bool
    , tableSize         :: TVar Size
    , maxTableSize      :: IORef Size
    , sendIns           :: ByteString -> IO ()
    , maxHeaderSize     :: IORef Int
    , maxBlockedStreams :: IORef Int
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

checkRequiredInsertCountNB :: DynamicTable -> RequiredInsertCount -> IO Bool
checkRequiredInsertCountNB DynamicTable{..} (RequiredInsertCount reqip) = atomically $ do
    InsertionPoint ip <- readTVar insertionPoint
    return (reqip <= ip)

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
        knownReceivedCount  <- newTVarIO 0
        referenceCounters   <- newIORef arr
        sections            <- newIORef IntMap.empty
        lruCache            <- newLRUCacheRef 0
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
    let huffmanDecoder = decodeHLock tvar
    blockedStreams <- newIORef 0
    newDynamicTable DecodeInfo{..} sendDI

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
    tableSize <- newTVarIO 0
    maxTableSize <- newIORef 0
    let sendIns = send
    maxHeaderSize <- newIORef maxBound
    maxBlockedStreams <- newIORef 0 -- fixme
    return DynamicTable{..}

----------------------------------------------------------------

setDebugQPACK :: DynamicTable -> IO ()
setDebugQPACK DynamicTable{..} = writeIORef debugQPACK True

getDebugQPACK :: DynamicTable -> IO Bool
getDebugQPACK DynamicTable{..} = readIORef debugQPACK

qpackDebug :: DynamicTable -> IO () -> IO ()
qpackDebug DynamicTable{..} action = do
    debug <- readIORef debugQPACK
    when debug $ withMVar stdoutLock $ \_ -> action

{-# NOINLINE stdoutLock #-}
stdoutLock :: MVar ()
stdoutLock = unsafePerformIO $ newMVar ()

----------------------------------------------------------------

getMaxNumOfEntries :: DynamicTable -> IO Int
getMaxNumOfEntries DynamicTable{..} = readTVarIO maxNumOfEntries

getDynamicTableSize :: DynamicTable -> IO Int
getDynamicTableSize DynamicTable{..} = readTVarIO tableSize

----------------------------------------------------------------

{-# INLINE getRevIndex #-}
getRevIndex :: DynamicTable -> RevIndex
getRevIndex DynamicTable{..} = revIndex
  where
    EncodeInfo{..} = codeInfo

getHuffmanDecoder :: DynamicTable -> HuffmanDecoder
getHuffmanDecoder DynamicTable{..} = huffmanDecoder
  where
    DecodeInfo{..} = codeInfo

getLruCache :: DynamicTable -> LRUCacheRef FieldName FieldValue
getLruCache DynamicTable{..} = lruCache
  where
    EncodeInfo{..} = codeInfo

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
    atomically $ modifyTVar' tableSize (+ entrySize ent)
    return ai

insertEntryToDecoder :: Entry -> DynamicTable -> STM ()
insertEntryToDecoder ent DynamicTable{..} = do
    x@(InsertionPoint insp) <- readTVar insertionPoint
    writeTVar insertionPoint (x + 1)
    maxN <- readTVar maxNumOfEntries
    let i = insp `mod` maxN
    table <- readTVar circularTable
    unsafeWrite table i ent
    modifyTVar' tableSize (+ entrySize ent)

toDynamicEntry :: DynamicTable -> AbsoluteIndex -> STM Entry
toDynamicEntry DynamicTable{..} (AbsoluteIndex idx) = do
    maxN <- readTVar maxNumOfEntries
    let i = idx `mod` maxN
    table <- readTVar circularTable
    unsafeRead table i

----------------------------------------------------------------

canInsertEntry :: DynamicTable -> Entry -> IO Bool
canInsertEntry DynamicTable{..} ent = do
    siz <- readTVarIO tableSize
    maxsiz <- readIORef maxTableSize
    return (siz + entrySize ent <= maxsiz)

----------------------------------------------------------------

getBlockedStreams :: DynamicTable -> IO Int
getBlockedStreams DynamicTable{..} = IntMap.size <$> readIORef sections
  where
    EncodeInfo{..} = codeInfo

insertSection :: DynamicTable -> StreamId -> Section -> IO ()
insertSection DynamicTable{..} sid section = atomicModifyIORef' sections ins
  where
    ins m =
        let m' = IntMap.insert sid section m
         in (m', ())
    EncodeInfo{..} = codeInfo

getAndDelSection :: DynamicTable -> StreamId -> IO (Maybe Section)
getAndDelSection DynamicTable{..} sid = atomicModifyIORef' sections getAndDel
  where
    getAndDel m =
        let (msec, m') = IntMap.updateLookupWithKey f sid m
         in (m', msec)
    f _ _ = Nothing -- delete the entry if found
    EncodeInfo{..} = codeInfo

increaseReference :: DynamicTable -> AbsoluteIndex -> IO ()
increaseReference = modifyReference (+ 1)

decreaseReference :: DynamicTable -> AbsoluteIndex -> IO ()
decreaseReference = modifyReference (subtract 1)

modifyReference :: (Int -> Int) -> DynamicTable -> AbsoluteIndex -> IO ()
modifyReference func DynamicTable{..} (AbsoluteIndex idx) = do
    maxN <- readTVarIO maxNumOfEntries
    let i = idx `mod` maxN
    arr <- readIORef referenceCounters
    -- modifyArray' is not provided by GHC 9.4 or earlier, sigh.
    x <- unsafeRead arr i
    let x' = func x
    unsafeWrite arr i x'
  where
    EncodeInfo{..} = codeInfo

----------------------------------------------------------------

setTableCapacity :: DynamicTable -> Int -> IO ()
setTableCapacity dyntbl@DynamicTable{..} maxsiz = do
    qpackDebug dyntbl $ putStrLn $ "setTableCapacity " ++ show maxsiz
    writeIORef maxTableSize maxsiz
    tbl <- atomically $ newArray (0, end) dummyEntry
    atomically $ do
        writeTVar maxNumOfEntries maxN
        writeTVar circularTable tbl
    case codeInfo of
        EncodeInfo{..} -> do
            arr <- newArray (0, end) 0
            writeIORef referenceCounters arr
            setLRUCapacity lruCache maxN
        _ -> return ()
    writeIORef capaReady True
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

isTableReady :: DynamicTable -> IO Bool
isTableReady DynamicTable{..} = readIORef capaReady

setMaxBlockedStreams :: DynamicTable -> Int -> IO ()
setMaxBlockedStreams DynamicTable{..} n = writeIORef maxBlockedStreams n

getMaxBlockedStreams :: DynamicTable -> IO Int
getMaxBlockedStreams DynamicTable{..} = readIORef maxBlockedStreams

getMaxHeaderSize :: DynamicTable -> IO Int
getMaxHeaderSize DynamicTable{..} = readIORef maxHeaderSize

setMaxHeaderSize :: DynamicTable -> Int -> IO ()
setMaxHeaderSize DynamicTable{..} n = writeIORef maxHeaderSize n

incrementKnownReceivedCount :: DynamicTable -> Int -> IO ()
incrementKnownReceivedCount DynamicTable{..} n =
    atomically $ modifyTVar' knownReceivedCount (+ n)
  where
    EncodeInfo{..} = codeInfo

updateKnownReceivedCount :: DynamicTable -> RequiredInsertCount -> IO ()
updateKnownReceivedCount DynamicTable{..} (RequiredInsertCount reqInsCnt) =
    atomically $ modifyTVar' knownReceivedCount $ \krc -> max reqInsCnt krc
  where
    EncodeInfo{..} = codeInfo

----------------------------------------------------------------

tryDrop :: DynamicTable -> Int -> IO ()
tryDrop dyntbl@DynamicTable{..} requiredSize = loop requiredSize
  where
    EncodeInfo{..} = codeInfo
    loop n | n <= 0 = return ()
    loop n = do
        maxN <- readTVarIO maxNumOfEntries
        AbsoluteIndex ai <- readIORef droppingPoint
        let i = ai `mod` maxN
        refs <- readIORef referenceCounters
        refN <- unsafeRead refs i
        when (refN == 0) $ do
            table <- readTVarIO circularTable
            ent <- atomically $ do
                e <- unsafeRead table i
                unsafeWrite table i dummyEntry
                return e
            qpackDebug dyntbl $
                putStrLn $
                    "DROPPED: " ++ show (entryHeaderName ent) ++ " " ++ show (entryFieldValue ent)
            let siz = entrySize ent
            atomically $ modifyTVar' tableSize $ subtract siz
            modifyIORef' droppingPoint (+ 1)
            deleteRevIndex revIndex ent
            loop (n - siz)

duplicate :: DynamicTable -> HIndex -> IO AbsoluteIndex
duplicate _ (SIndex _) = error "duplicate"
duplicate dyntbl@DynamicTable{..} (DIndex (AbsoluteIndex ai)) = do
    maxN <- readTVarIO maxNumOfEntries
    let i = ai `mod` maxN
    table <- readTVarIO circularTable
    ent <- atomically $ unsafeRead table i
    deleteRevIndex revIndex ent
    insertEntryToEncoder ent dyntbl
  where
    EncodeInfo{..} = codeInfo

isDraining :: DynamicTable -> AbsoluteIndex -> IO Bool
isDraining DynamicTable{..} ai = do
    di <- readIORef drainingPoint
    return (ai <= di)
  where
    EncodeInfo{..} = codeInfo

adjustDrainingPoint :: DynamicTable -> IO ()
adjustDrainingPoint DynamicTable{..} = do
    InsertionPoint beg <- readTVarIO insertionPoint
    AbsoluteIndex end <- readIORef droppingPoint
    let num = beg - end
        space = max 2 (num !>>. 4)
        end' = beg - num + space
    writeIORef drainingPoint $ AbsoluteIndex end'
  where
    EncodeInfo{..} = codeInfo

----------------------------------------------------------------

tryIncreaseStreams :: DynamicTable -> IO Bool
tryIncreaseStreams DynamicTable{..} = do
    lim <- readIORef maxBlockedStreams
    curr <- atomicModifyIORef' blockedStreams (\n -> (n + 1, n + 1))
    return (curr <= lim)
  where
    DecodeInfo{..} = codeInfo

decreaseStreams :: DynamicTable -> IO ()
decreaseStreams DynamicTable{..} = atomicModifyIORef' blockedStreams (\n -> (n - 1, ()))
  where
    DecodeInfo{..} = codeInfo
