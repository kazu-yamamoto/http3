module Network.QPACK.Internal (
    module Network.QPACK.HeaderBlock.Prefix
  , module Network.QPACK.Instruction
  , DynamicTable
  , AbsoluteIndex(..)
  , InsRelativeIndex(..)
  , InsertionPoint(..)
  , BasePoint(..)
  ) where

import Network.QPACK.Table (DynamicTable)
import Network.QPACK.HeaderBlock.Prefix
import Network.QPACK.Instruction
import Network.QPACK.Types
