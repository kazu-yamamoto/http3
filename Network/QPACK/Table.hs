module Network.QPACK.Table (
  -- * Dynamic table
    DynamicTable
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  -- * Getter and setter
  , getMaxNumOfEntries
  , setBasePointToInsersionPoint
  , getBasePoint
  , getInsertionPoint
  , getLargestReference
  , updateLargestReference
  -- * Entry
  , insertEntry
  -- * Reverse index
  , RevIndex
  , RevResult(..)
  , getRevIndex
  , lookupRevIndex
  ) where

import Network.QPACK.Table.Dynamic
import Network.QPACK.Table.RevIndex
