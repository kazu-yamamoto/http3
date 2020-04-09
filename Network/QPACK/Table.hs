module Network.QPACK.Table (
    DynamicTable
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , getMaxNumOfEntries
  , setBasePointToInsersionPoint
  , getBasePoint
  , getInsertionPoint
  , getRevIndex
  , updateLargestReference
  , lookupRevIndex
  , insertEntry
  , RevResult(..)
  , RevIndex
  ) where

import Network.QPACK.Table.Dynamic
import Network.QPACK.Table.RevIndex
