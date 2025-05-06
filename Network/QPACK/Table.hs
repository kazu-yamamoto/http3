module Network.QPACK.Table (
    -- * Dynamic table
    DynamicTable,
    newDynamicTableForEncoding,
    newDynamicTableForDecoding,

    -- * Getter and setter
    getMaxNumOfEntries,
    setBasePointToInsersionPoint,
    getBasePoint,
    getInsertionPoint,
    getInsertionPointSTM,
    checkInsertionPoint,
    getLargestReference,
    updateLargestReference,

    -- * Entry
    insertEntryToEncoder,
    insertEntryToDecoder,
    toIndexedEntry,

    -- * Reverse index
    RevIndex,
    RevResult (..),
    getRevIndex,
    lookupRevIndex,

    -- * Misc
    getHuffmanDecoder,
    getSendEI,
    getSendDI,
    setDebugQPACK,
    getDebugQPACK,
    qpackDebug,
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
