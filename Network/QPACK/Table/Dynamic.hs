{-# LANGUAGE RecordWildCards #-}

module Network.QPACK.Table.Dynamic where

import Control.Concurrent.STM
import Data.Array.Base (unsafeWrite, unsafeRead)
import Data.Array.IO (IOArray, newArray)
import Data.IORef
import Foreign.Marshal.Alloc (mallocBytes, free)
import Network.ByteOrder
import Network.HPACK.Internal

import Imports
import Network.QPACK.Table.RevIndex
import Network.QPACK.Types

data CodeInfo =
    EncodeInfo RevIndex -- Reverse index
               (IORef InsertionPoint)
  | DecodeInfo HuffmanDecoder
               (IO ()) -- free buffer

-- | Dynamic table for QPACK.
data DynamicTable = DynamicTable {
    codeInfo          :: CodeInfo
  , droppingPoint     :: IORef AbsoluteIndex
  , drainingPoint     :: IORef AbsoluteIndex
  , insertionPoint    :: TVar InsertionPoint
  , basePoint         :: IORef BasePoint
  , maxNumOfEntries   :: IORef Int
  , circularTable     :: IORef Table
  }

type Table = IOArray Index Entry

----------------------------------------------------------------

getBasePoint :: DynamicTable -> IO BasePoint
getBasePoint DynamicTable{..} = readIORef basePoint

setBasePointToInsersionPoint :: DynamicTable -> IO ()
setBasePointToInsersionPoint DynamicTable{..} = do
    InsertionPoint ip <- readTVarIO insertionPoint
    writeIORef basePoint $ BasePoint ip

getInsertionPoint :: DynamicTable -> IO InsertionPoint
getInsertionPoint DynamicTable{..} = readTVarIO insertionPoint

checkInsertionPoint :: DynamicTable -> InsertionPoint -> IO ()
checkInsertionPoint DynamicTable{..} reqip = atomically $ do
    ip <- readTVar insertionPoint
    check (reqip <= ip)

----------------------------------------------------------------

-- | Creating 'DynamicTable' for encoding.
newDynamicTableForEncoding :: Size -- ^ The dynamic table size
                           -> IO DynamicTable
newDynamicTableForEncoding maxsiz = do
    rev <- newRevIndex
    ref <- newIORef 0
    let info = EncodeInfo rev ref
    newDynamicTable maxsiz info

-- | Creating 'DynamicTable' for decoding.
newDynamicTableForDecoding :: Size -- ^ The dynamic table size
                           -> Size -- ^ The size of temporary buffer for Huffman decoding
                           -> IO DynamicTable
newDynamicTableForDecoding maxsiz huftmpsiz = do
    buf <- mallocBytes huftmpsiz
    wbuf <- newWriteBuffer buf huftmpsiz
    let decoder = decodeH wbuf
        clear = free buf
        info = DecodeInfo decoder clear
    newDynamicTable maxsiz info

newDynamicTable :: Size -> CodeInfo -> IO DynamicTable
newDynamicTable maxsiz info = do
    tbl <- newArray (0,end) dummyEntry
    DynamicTable info <$> newIORef 0       -- droppingPoint
                      <*> newIORef 0       -- drainingPoint
                      <*> newTVarIO 0      -- insertionPoint
                      <*> newIORef 0       -- basePoint
                      <*> newIORef maxN    -- maxNumOfEntries
                      <*> newIORef tbl     -- maxDynamicTableSize
  where
    maxN = maxNumbers maxsiz
    end = maxN - 1

-- | Clearing 'DynamicTable'.
--   Currently, this frees the temporary buffer for Huffman decoding.
clearDynamicTable :: DynamicTable -> IO ()
clearDynamicTable DynamicTable{..} = case codeInfo of
    EncodeInfo{}       -> return ()
    DecodeInfo _ clear -> clear

----------------------------------------------------------------

getMaxNumOfEntries :: DynamicTable -> IO Int
getMaxNumOfEntries DynamicTable{..} = readIORef maxNumOfEntries

----------------------------------------------------------------

{-# INLINE getRevIndex #-}
getRevIndex :: DynamicTable-> RevIndex
getRevIndex DynamicTable{..} = rev
  where
    EncodeInfo rev _ = codeInfo

getHuffmanDecoder :: DynamicTable -> HuffmanDecoder
getHuffmanDecoder DynamicTable{..} = huf
  where
    DecodeInfo huf _ = codeInfo

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
    maxN <- readIORef maxNumOfEntries
    let i = insp `mod` maxN
    table <- readIORef circularTable
    unsafeWrite table i ent
    let revtbl = getRevIndex dyntbl
    let ai = AbsoluteIndex insp
    insertRevIndex ent (DIndex ai) revtbl
    return ai

insertEntryToDecoder :: Entry -> DynamicTable -> IO ()
insertEntryToDecoder ent DynamicTable{..} = do
    InsertionPoint insp <- atomically $ do
        x <- readTVar insertionPoint
        writeTVar insertionPoint (x + 1)
        return x
    maxN <- readIORef maxNumOfEntries
    let i = insp `mod` maxN
    table <- readIORef circularTable
    unsafeWrite table i ent

toDynamicEntry :: DynamicTable -> AbsoluteIndex -> IO Entry
toDynamicEntry DynamicTable{..} (AbsoluteIndex idx) = do
    maxN <- readIORef maxNumOfEntries
    let i = idx `mod` maxN
    table <- readIORef circularTable
    unsafeRead table i
