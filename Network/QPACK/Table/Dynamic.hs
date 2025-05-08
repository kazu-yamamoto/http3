{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Network.QPACK.Table.Dynamic where

import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.MArray (newArray)
import Data.IORef
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

import Imports
import Network.QPACK.Table.RevIndex
import Network.QPACK.Types

data CodeInfo
    = EncodeInfo
        RevIndex -- Reverse index
        (IORef RequiredInsertCount)
    | DecodeInfo HuffmanDecoder

-- | Dynamic table for QPACK.
data DynamicTable = DynamicTable
    { codeInfo :: CodeInfo
    , droppingPoint :: IORef AbsoluteIndex
    , drainingPoint :: IORef AbsoluteIndex
    , insertionPoint :: TVar InsertionPoint
    , basePoint :: IORef BasePoint
    , maxNumOfEntries :: TVar Int
    , circularTable :: TVar Table
    , debugQPACK :: IORef Bool
    , capaReady :: IORef Bool
    , blockedStreams :: IORef Int
    , knownReceivedCount :: TVar Int
    , sendIns :: ByteString -> IO ()
    }

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
    check (reqip <= ip)

----------------------------------------------------------------

-- | Creating 'DynamicTable' for encoding.
newDynamicTableForEncoding
    :: (ByteString -> IO ())
    -> IO DynamicTable
newDynamicTableForEncoding sendEI = do
    rev <- newRevIndex
    ref <- newIORef 0
    let info = EncodeInfo rev ref
    newDynamicTable info sendEI

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

{- FOURMOLU_DISABLE -}
newDynamicTable :: CodeInfo -> (ByteString -> IO ()) -> IO DynamicTable
newDynamicTable info send = do
    tbl <- atomically $ newArray (0, 0) dummyEntry
    DynamicTable info
        <$> newIORef 0     -- droppingPoint
        <*> newIORef 0     -- drainingPoint
        <*> newTVarIO 0    -- insertionPoint
        <*> newIORef 0     -- basePoint
        <*> newTVarIO 0    -- maxNumOfEntries
        <*> newTVarIO tbl  -- circularTable
        <*> newIORef False -- debugQPACK
        <*> newIORef False -- capaReady
        <*> newIORef 0     -- blockedStreams
        <*> newTVarIO 0    -- knownReceivedCount
        <*> pure send      -- sendIns
{- FOURMOLU_ENABLE -}

updateDynamicTable :: DynamicTable -> Size -> IO ()
updateDynamicTable DynamicTable{..} maxsiz = do
    tbl <- atomically $ newArray (0, end) dummyEntry
    atomically $ do
        writeTVar maxNumOfEntries maxN
        writeTVar circularTable tbl
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
getRevIndex DynamicTable{..} = rev
  where
    EncodeInfo rev _ = codeInfo

getHuffmanDecoder :: DynamicTable -> HuffmanDecoder
getHuffmanDecoder DynamicTable{..} = huf
  where
    DecodeInfo huf = codeInfo

----------------------------------------------------------------

clearRequiredInsertCount :: DynamicTable -> IO ()
clearRequiredInsertCount DynamicTable{..} = writeIORef ref 0
  where
    EncodeInfo _ ref = codeInfo

getRequiredInsertCount :: DynamicTable -> IO RequiredInsertCount
getRequiredInsertCount DynamicTable{..} = readIORef ref
  where
    EncodeInfo _ ref = codeInfo

absoluteIndexToRequiredInsertCount :: AbsoluteIndex -> RequiredInsertCount
absoluteIndexToRequiredInsertCount (AbsoluteIndex idx) =
    RequiredInsertCount (idx + 1)

updateRequiredInsertCount :: DynamicTable -> AbsoluteIndex -> IO ()
updateRequiredInsertCount DynamicTable{..} aidx = do
    let newric = absoluteIndexToRequiredInsertCount aidx
    oldric <- readIORef ref
    when (newric > oldric) $ writeIORef ref newric
  where
    EncodeInfo _ ref = codeInfo

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

setKnownReceivedCount :: DynamicTable -> Int -> IO ()
setKnownReceivedCount DynamicTable{..} n =
    atomically $ modifyTVar' knownReceivedCount (+ n)
