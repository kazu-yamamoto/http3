module Network.QPACK.Table (
    DynamicTable
  , getMaxNumOfEntries
  , getBasePoint
  , getInsertionPoint
  , getRevIndex
  , updateLargestReference
  , lookupRevIndex
  , insertEntry
  , RevResult(..)
  ) where

import Network.QPACK.Table.Dynamic
import Network.QPACK.Table.RevIndex
