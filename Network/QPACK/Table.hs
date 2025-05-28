module Network.QPACK.Table (
    module Network.QPACK.Table.Dynamic,
    module Network.QPACK.Table.RevIndex,
    toIndexedEntry,
) where

import Control.Concurrent.STM
import Network.HPACK.Internal (Entry)

import Network.QPACK.Table.Dynamic
import Network.QPACK.Table.RevIndex
import Network.QPACK.Table.Static
import Network.QPACK.Types

toIndexedEntry :: DynamicTable -> HIndex -> STM Entry
toIndexedEntry _ (SIndex ai) = return $ toStaticEntry ai
toIndexedEntry dyntbl (DIndex ai) = toDynamicEntry dyntbl ai
