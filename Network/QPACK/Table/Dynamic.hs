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
        (IORef InsertionPoint)
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

checkInsertionPoint :: DynamicTable -> InsertionPoint -> IO ()
checkInsertionPoint DynamicTable{..} reqip = atomically $ do
    ip <- readTVar insertionPoint
    check (reqip <= ip)

----------------------------------------------------------------

-- | Creating 'DynamicTable' for encoding.
newDynamicTableForEncoding
    :: Size
    -- ^ The dynamic table size
    -> IO DynamicTable
newDynamicTableForEncoding maxsiz = do
    rev <- newRevIndex
    ref <- newIORef 0
    let info = EncodeInfo rev ref
    newDynamicTable maxsiz info

-- | Creating 'DynamicTable' for decoding.
newDynamicTableForDecoding
    :: Size
    -- ^ The dynamic table size
    -> Size
    -- ^ The size of temporary buffer for Huffman decoding
    -> IO DynamicTable
newDynamicTableForDecoding maxsiz huftmpsiz = do
    gcbuf <- mallocPlainForeignPtrBytes huftmpsiz
    tvar <- newTVarIO $ Just (gcbuf, huftmpsiz)
    let decoder = decodeHLock tvar
        info = DecodeInfo decoder
    newDynamicTable maxsiz info

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

newDynamicTable :: Size -> CodeInfo -> IO DynamicTable
newDynamicTable maxsiz info = do
    tbl <- atomically $ newArray (0, end) dummyEntry
    DynamicTable info
        <$> newIORef 0 -- droppingPoint
        <*> newIORef 0 -- drainingPoint
        <*> newTVarIO 0 -- insertionPoint
        <*> newIORef 0 -- basePoint
        <*> newTVarIO maxN -- maxNumOfEntries
        <*> newTVarIO tbl -- maxDynamicTableSize
        <*> newIORef False -- debugQPACK
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

clearLargestReference :: DynamicTable -> IO ()
clearLargestReference DynamicTable{..} = writeIORef ref 0
  where
    EncodeInfo _ ref = codeInfo

getLargestReference :: DynamicTable -> IO InsertionPoint
getLargestReference DynamicTable{..} = readIORef ref
  where
    EncodeInfo _ ref = codeInfo

updateLargestReference :: DynamicTable -> AbsoluteIndex -> IO ()
updateLargestReference DynamicTable{..} (AbsoluteIndex idx) = do
    let nidx = InsertionPoint idx
    oidx <- readIORef ref
    when (nidx > oidx) $ writeIORef ref nidx
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
