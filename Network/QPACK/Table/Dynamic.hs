{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Network.QPACK.Table.Dynamic (
    -- * Dynamic table
    DynamicTable,
    newDynamicTableForEncoding,
    newDynamicTableForDecoding,

    -- * Capacity
    isTableReady,
    getTableCapacity,
    setTableCapacity,
    getMaxNumOfEntries,

    -- * Entry
    insertEntryToDecoder,
    insertEntryToEncoder,
    toDynamicEntry,

    -- * Section
    Section (..),
    insertSection,
    getAndDelSection,
    increaseReference,
    decreaseReference,

    -- * Streams
    getMaxBlockedStreams,
    setMaxBlockedStreams,
    tryIncreaseStreams,
    decreaseStreams,

    -- * Blocked streams
    insertBlockedStreamE,
    deleteBlockedStreamE,
    checkBlockedStreams,

    -- * Required insert count
    getRequiredInsertCount,
    clearRequiredInsertCount,
    checkRequiredInsertCount,
    checkRequiredInsertCountNB,
    updateRequiredInsertCount,

    -- * Known received count
    incrementKnownReceivedCount,
    updateKnownReceivedCount,
    wouldSectionBeBlocked,
    wouldInstructionBeBlocked,
    setInsersionPointToKnownReceivedCount,

    -- * Points
    getBasePoint,
    setBasePointToInsersionPoint,
    getInsertionPoint,
    getInsertionPointSTM,

    -- * Draining
    isDraining,
    adjustDrainingPoint,
    duplicate,
    tryDrop,

    -- * Dropping
    canInsertEntry,

    -- * Accessing
    getLruCache,
    getRevIndex,
    getHuffmanDecoder,
    sendIns,

    -- * Max header size
    getMaxHeaderSize,
    setMaxHeaderSize,

    -- * Debug
    qpackDebug,
    getDebugQPACK,
    setDebugQPACK,
    printReferences,
    checkHIndex,
    checkAbsoluteIndex,

    -- * QIF
    getImmediateAck,
    setImmediateAck,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOArray, newArray)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Set (Set)
import qualified Data.Set as Set -- Set.size is O(1), IntSet.size is O(n)
import Imports
import Network.Control
import Network.HPACK.Internal (
    Entry,
    HuffmanDecoder,
    Index,
    Size,
    decodeH,
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

----------------------------------------------------------------

type Table = TArray Index Entry
data Section = Section RequiredInsertCount [AbsoluteIndex]

{- FOURMOLU_DISABLE -}
data CodeInfo
    = EncodeInfo
        { revIndex            :: RevIndex -- Reverse index
        , requiredInsertCount :: IORef RequiredInsertCount
        , droppingPoint       :: IORef AbsoluteIndex
        , drainingPoint       :: IORef AbsoluteIndex
        , knownReceivedCount  :: TVar Int
        , referenceCounters   :: IORef (IOArray Index (Maybe Int))
        , sections            :: IORef (IntMap Section)
        , lruCache            :: LRUCacheRef (FieldName, FieldValue) ()
        , immediateAck        :: IORef Bool -- for QIF
        , blockedStreamsE     :: IORef (Set Int)
        }
    | DecodeInfo
        { huffmanDecoder  :: HuffmanDecoder  -- only for encoder instruction handler
        , blockedStreamsD :: IORef Int
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

----------------------------------------------------------------

{- FOURMOLU_DISABLE -}
-- | Creating 'DynamicTable' for encoding.
newDynamicTableForEncoding
    :: (ByteString -> IO ())
    -> IO DynamicTable
newDynamicTableForEncoding sendEI = do
    arr <- newArray (0, 0) Nothing
    info <- do
        revIndex            <- newRevIndex
        requiredInsertCount <- newIORef 0
        droppingPoint       <- newIORef 0
        drainingPoint       <- newIORef 0
        knownReceivedCount  <- newTVarIO 0
        referenceCounters   <- newIORef arr
        sections            <- newIORef IntMap.empty
        lruCache            <- newLRUCacheRef 0
        immediateAck        <- newIORef False
        blockedStreamsE     <- newIORef Set.empty
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
    let huffmanDecoder = decodeH gcbuf huftmpsiz
    blockedStreamsD <- newIORef 0
    newDynamicTable DecodeInfo{..} sendDI

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
    maxBlockedStreams <- newIORef 0
    return DynamicTable{..}

----------------------------------------------------------------

isTableReady :: DynamicTable -> IO Bool
isTableReady DynamicTable{..} = readIORef capaReady

getTableCapacity :: DynamicTable -> IO Int
getTableCapacity DynamicTable{..} = readTVarIO tableSize

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
            arr <- newArray (0, end) Nothing
            writeIORef referenceCounters arr
            setLRUCapacity lruCache (maxN * 4)
        _ -> return ()
    writeIORef capaReady True
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

getMaxNumOfEntries :: DynamicTable -> IO Int
getMaxNumOfEntries DynamicTable{..} = readTVarIO maxNumOfEntries

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
    insertRevIndex ent ai revtbl
    atomically $ modifyTVar' tableSize (+ entrySize ent)
    dropIfNecessary dyntbl
    resetReference dyntbl ai
    return ai

insertEntryToDecoder :: Entry -> DynamicTable -> STM AbsoluteIndex
insertEntryToDecoder ent DynamicTable{..} = do
    x@(InsertionPoint insp) <- readTVar insertionPoint
    writeTVar insertionPoint (x + 1)
    maxN <- readTVar maxNumOfEntries
    let i = insp `mod` maxN
    table <- readTVar circularTable
    unsafeWrite table i ent
    modifyTVar' tableSize (+ entrySize ent)
    return $ AbsoluteIndex insp

toDynamicEntry :: DynamicTable -> AbsoluteIndex -> STM Entry
toDynamicEntry DynamicTable{..} (AbsoluteIndex idx) = do
    maxN <- readTVar maxNumOfEntries
    let i = idx `mod` maxN
    table <- readTVar circularTable
    unsafeRead table i

----------------------------------------------------------------

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
    let x' = func <$> if x == Nothing then Just 0 else x
    unsafeWrite arr i x'
  where
    EncodeInfo{..} = codeInfo

resetReference :: DynamicTable -> AbsoluteIndex -> IO ()
resetReference DynamicTable{..} (AbsoluteIndex idx) = do
    maxN <- readTVarIO maxNumOfEntries
    let i = idx `mod` maxN
    arr <- readIORef referenceCounters
    unsafeWrite arr i $ Nothing
  where
    EncodeInfo{..} = codeInfo

----------------------------------------------------------------

getMaxBlockedStreams :: DynamicTable -> IO Int
getMaxBlockedStreams DynamicTable{..} = readIORef maxBlockedStreams

setMaxBlockedStreams :: DynamicTable -> Int -> IO ()
setMaxBlockedStreams DynamicTable{..} n = writeIORef maxBlockedStreams n

-- Decoder

tryIncreaseStreams :: DynamicTable -> IO Bool
tryIncreaseStreams DynamicTable{..} = do
    lim <- readIORef maxBlockedStreams
    curr <- atomicModifyIORef' blockedStreamsD (\n -> (n + 1, n + 1))
    return (curr <= lim)
  where
    DecodeInfo{..} = codeInfo

decreaseStreams :: DynamicTable -> IO ()
decreaseStreams DynamicTable{..} = atomicModifyIORef' blockedStreamsD (\n -> (n - 1, ()))
  where
    DecodeInfo{..} = codeInfo

----------------------------------------------------------------

getBlockedStreamsE :: DynamicTable -> IO Int
getBlockedStreamsE DynamicTable{..} =
    Set.size <$> readIORef blockedStreamsE
  where
    EncodeInfo{..} = codeInfo

insertBlockedStreamE :: DynamicTable -> StreamId -> IO ()
insertBlockedStreamE DynamicTable{..} sid =
    modifyIORef' blockedStreamsE (Set.insert sid)
  where
    EncodeInfo{..} = codeInfo

deleteBlockedStreamE :: DynamicTable -> StreamId -> IO ()
deleteBlockedStreamE DynamicTable{..} sid =
    modifyIORef' blockedStreamsE (Set.delete sid)
  where
    EncodeInfo{..} = codeInfo

checkBlockedStreams :: DynamicTable -> IO Bool
checkBlockedStreams dyntbl = do
    maxBlocked <- getMaxBlockedStreams dyntbl
    blocked <- getBlockedStreamsE dyntbl
    -- The next one would be blocked, so <, not <=
    return $ blocked < maxBlocked

----------------------------------------------------------------

getRequiredInsertCount :: DynamicTable -> IO RequiredInsertCount
getRequiredInsertCount DynamicTable{..} = readIORef requiredInsertCount
  where
    EncodeInfo{..} = codeInfo

clearRequiredInsertCount :: DynamicTable -> IO ()
clearRequiredInsertCount DynamicTable{..} = writeIORef requiredInsertCount 0
  where
    EncodeInfo{..} = codeInfo

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

wouldSectionBeBlocked :: DynamicTable -> RequiredInsertCount -> IO Bool
wouldSectionBeBlocked DynamicTable{..} (RequiredInsertCount reqip) = atomically $ do
    krc <- readTVar knownReceivedCount
    return (reqip > krc)
  where
    EncodeInfo{..} = codeInfo

wouldInstructionBeBlocked :: DynamicTable -> AbsoluteIndex -> IO Bool
wouldInstructionBeBlocked DynamicTable{..} (AbsoluteIndex ai) = atomically $ do
    krc <- readTVar knownReceivedCount
    return (ai > krc)
  where
    EncodeInfo{..} = codeInfo

setInsersionPointToKnownReceivedCount :: DynamicTable -> IO ()
setInsersionPointToKnownReceivedCount dyntbl@DynamicTable{..} = do
    InsertionPoint ai <- getInsertionPoint dyntbl
    atomically $ writeTVar knownReceivedCount ai
  where
    EncodeInfo{..} = codeInfo

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

----------------------------------------------------------------

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
        space = max 1 (num !>>. 4)
        end' = beg - num + space
    writeIORef drainingPoint $ AbsoluteIndex end'
  where
    EncodeInfo{..} = codeInfo

duplicate :: DynamicTable -> AbsoluteIndex -> IO AbsoluteIndex
duplicate dyntbl@DynamicTable{..} dai@(AbsoluteIndex ai) = do
    maxN <- readTVarIO maxNumOfEntries
    let i = ai `mod` maxN
    table <- readTVarIO circularTable
    ent <- atomically $ unsafeRead table i
    deleteRevIndex revIndex ent dai
    insertEntryToEncoder ent dyntbl
  where
    EncodeInfo{..} = codeInfo

----------------------------------------------------------------

canInsertEntry :: DynamicTable -> Entry -> IO Bool
canInsertEntry DynamicTable{..} ent = do
    let siz = entrySize ent
    tblsiz <- readTVarIO tableSize
    maxtblsiz <- readIORef maxTableSize
    if tblsiz + siz <= maxtblsiz
        then
            return True
        else do
            AbsoluteIndex ai <- readIORef droppingPoint
            InsertionPoint lim <- readTVarIO insertionPoint
            loop ai lim (tblsiz + siz - maxtblsiz)
  where
    EncodeInfo{..} = codeInfo
    loop ai lim requiredSize
        | requiredSize <= 0 = return True
        | otherwise = do
            if ai < lim
                then do
                    maxN <- readTVarIO maxNumOfEntries
                    let i = ai `mod` maxN
                    refs <- readIORef referenceCounters
                    refN <- unsafeRead refs i
                    if refN == Just 0
                        then do
                            table <- readTVarIO circularTable
                            dent <- atomically $ unsafeRead table i
                            let siz = entrySize dent

                            loop (ai + 1) lim (requiredSize - siz)
                        else return False
                else return False

dropIfNecessary :: DynamicTable -> IO ()
dropIfNecessary dyntbl@DynamicTable{..} = loop
  where
    loop = do
        tblsize <- readTVarIO tableSize
        maxtblsize <- readIORef maxTableSize
        unless (tblsize <= maxtblsize) $ do
            dropped <- tryDrop dyntbl
            if dropped then loop else error "dropIfNecessary"

tryDrop :: DynamicTable -> IO Bool
tryDrop dyntbl@DynamicTable{..} = do
    maxN <- readTVarIO maxNumOfEntries
    dai@(AbsoluteIndex ai) <- readIORef droppingPoint
    InsertionPoint lim <- readTVarIO insertionPoint
    if ai < lim
        then do
            let i = ai `mod` maxN
            refs <- readIORef referenceCounters
            refN <- unsafeRead refs i
            if refN == Just 0
                then do
                    table <- readTVarIO circularTable
                    ent <- atomically $ do
                        e <- unsafeRead table i
                        unsafeWrite table i dummyEntry
                        return e
                    let siz = entrySize ent
                    atomically $ modifyTVar' tableSize $ subtract siz
                    modifyIORef' droppingPoint (+ 1)
                    qpackDebug dyntbl $ do
                        putStrLn $
                            "DROPPED (AbsoluteIndex "
                                ++ show ai
                                ++ ") "
                                ++ show (entryHeaderName ent)
                                ++ " "
                                ++ show (entryFieldValue ent)
                        tblsiz <- readTVarIO tableSize
                        putStrLn $ "    tblsiz: " ++ show tblsiz
                    deleteRevIndex revIndex ent dai
                    return True
                else return False
        else return False
  where
    EncodeInfo{..} = codeInfo

----------------------------------------------------------------

getLruCache :: DynamicTable -> LRUCacheRef (FieldName, FieldValue) ()
getLruCache DynamicTable{..} = lruCache
  where
    EncodeInfo{..} = codeInfo

{-# INLINE getRevIndex #-}
getRevIndex :: DynamicTable -> RevIndex
getRevIndex DynamicTable{..} = revIndex
  where
    EncodeInfo{..} = codeInfo

-- only for encoder instruction handler
getHuffmanDecoder :: DynamicTable -> HuffmanDecoder
getHuffmanDecoder DynamicTable{..} = huffmanDecoder
  where
    DecodeInfo{..} = codeInfo

----------------------------------------------------------------

getMaxHeaderSize :: DynamicTable -> IO Int
getMaxHeaderSize DynamicTable{..} = readIORef maxHeaderSize

setMaxHeaderSize :: DynamicTable -> Int -> IO ()
setMaxHeaderSize DynamicTable{..} n = writeIORef maxHeaderSize n

----------------------------------------------------------------

qpackDebug :: DynamicTable -> IO () -> IO ()
qpackDebug DynamicTable{..} action = do
    debug <- readIORef debugQPACK
    when debug $ withMVar stdoutLock $ \_ -> action

getDebugQPACK :: DynamicTable -> IO Bool
getDebugQPACK DynamicTable{..} = readIORef debugQPACK

setDebugQPACK :: DynamicTable -> Bool -> IO ()
setDebugQPACK DynamicTable{..} b = writeIORef debugQPACK b

{-# NOINLINE stdoutLock #-}
stdoutLock :: MVar ()
stdoutLock = unsafePerformIO $ newMVar ()

printReferences :: DynamicTable -> IO ()
printReferences DynamicTable{..} = do
    AbsoluteIndex start <- readIORef droppingPoint
    InsertionPoint end <- readTVarIO insertionPoint
    maxN <- readTVarIO maxNumOfEntries
    arr <- readIORef referenceCounters
    putStr "Refs: "
    loop start end arr maxN
    putStr "\n"
  where
    loop :: Int -> Int -> IOArray Index (Maybe Int) -> Int -> IO ()
    loop start end arr maxN
        | start < end = do
            n <- unsafeRead arr (start `mod` maxN)
            putStr $ show start ++ ": " ++ showJust n ++ ", "
            loop (start + 1) end arr maxN
        | otherwise = return ()
    EncodeInfo{..} = codeInfo
    showJust Nothing = "N"
    showJust (Just n) = show n

-- For decoder
checkHIndex :: DynamicTable -> HIndex -> IO ()
checkHIndex _ (SIndex _) = return ()
checkHIndex DynamicTable{..} (DIndex (AbsoluteIndex ai)) = do
    InsertionPoint ip <- readTVarIO insertionPoint
    maxN <- readTVarIO maxNumOfEntries
    if ip - maxN <= ai && ai < ip
        then return ()
        else error "checkHIndex"

-- For encoder
checkAbsoluteIndex :: DynamicTable -> AbsoluteIndex -> String -> IO ()
checkAbsoluteIndex DynamicTable{..} (AbsoluteIndex ai) tag = do
    InsertionPoint beg <- readTVarIO insertionPoint
    AbsoluteIndex end <- readIORef droppingPoint
    maxN <- readTVarIO maxNumOfEntries
    table <- readTVarIO circularTable
    let calcSize i acc
            | i == beg = return acc
            | otherwise = do
                siz <- entrySize <$> atomically (unsafeRead table (i `mod` maxN))
                calcSize (i + 1) (acc + siz)
    if end <= ai && ai < beg
        then do
            size <- calcSize end 0
            size0 <- readTVarIO tableSize
            when (size /= size0) $ error $ "checkAbsoluteIndex: size /= size0) " ++ tag
            lim <- readIORef maxTableSize
            when (size > lim) $ error $ "checkAbsoluteIndex: size > lim " ++ tag
            putStrLn $ "    check: tblsiz: " ++ show size ++ " " ++ show ai ++ " " ++ tag
        else
            error $
                "checkAbsoluteIndex (3) "
                    ++ tag
                    ++ " "
                    ++ show end
                    ++ " "
                    ++ show ai
                    ++ " "
                    ++ show beg
  where
    EncodeInfo{..} = codeInfo

----------------------------------------------------------------

getImmediateAck :: DynamicTable -> IO Bool
getImmediateAck DynamicTable{..} = readIORef immediateAck
  where
    EncodeInfo{..} = codeInfo

setImmediateAck :: DynamicTable -> Bool -> IO ()
setImmediateAck DynamicTable{..} b = writeIORef immediateAck b
  where
    EncodeInfo{..} = codeInfo
