{-# LANGUAGE RecordWildCards #-}

module Network.QPACK.Table.Dynamic where

import Control.Concurrent.STM
import Data.Array.Base (unsafeWrite)
import Data.Array.IO (IOArray, newArray, )
import Data.IORef
import Foreign.Marshal.Alloc (mallocBytes, free)
import Network.ByteOrder
import Network.HPACK.Internal

import Imports
import Network.QPACK.Table.RevIndex
import Network.QPACK.Types

data CodeInfo =
    EncodeInfo RevIndex -- Reverse index
               (IORef AbsoluteIndex)
  | DecodeInfo HuffmanDecoder
               (IO ()) -- free buffer

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

getInsertionPoint :: DynamicTable -> IO InsertionPoint
getInsertionPoint DynamicTable{..} = atomically $ readTVar insertionPoint

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

----------------------------------------------------------------

clearLargestReference :: DynamicTable -> IO ()
clearLargestReference DynamicTable{..} = writeIORef ref 0
  where
    EncodeInfo _ ref = codeInfo

getLargestReference :: DynamicTable -> IO AbsoluteIndex
getLargestReference DynamicTable{..} = readIORef ref
  where
    EncodeInfo _ ref = codeInfo

updateLargestReference :: DynamicTable -> AbsoluteIndex -> IO ()
updateLargestReference DynamicTable{..} idx = do
    oidx <- readIORef ref
    when (idx > oidx) $ writeIORef ref idx
  where
    EncodeInfo _ ref = codeInfo

----------------------------------------------------------------

insertEntry :: Entry -> DynamicTable -> IO AbsoluteIndex
insertEntry ent dyntbl@DynamicTable{..} = do
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
