module Network.QPACK.Table (
  -- * Dynamic table
    DynamicTable
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , clearDynamicTable
  -- * Getter and setter
  , getMaxNumOfEntries
  , setBasePointToInsersionPoint
  , getBasePoint
  , getInsertionPoint
  , checkInsertionPoint
  , getLargestReference
  , updateLargestReference
  -- * Entry
  , insertEntry
  , toIndexedEntry
  -- * Reverse index
  , RevIndex
  , RevResult(..)
  , getRevIndex
  , lookupRevIndex
  -- * Misc
  , getHuffmanDecoder
  ) where

import Network.HPACK.Internal (Entry)

import Network.QPACK.Table.Dynamic
import Network.QPACK.Table.RevIndex
import Network.QPACK.Table.Static
import Network.QPACK.Types

toIndexedEntry :: DynamicTable -> HIndex -> IO Entry
toIndexedEntry _      (SIndex ai) = return $ toStaticEntry ai
toIndexedEntry dyntbl (DIndex ai) = toDynamicEntry dyntbl ai
