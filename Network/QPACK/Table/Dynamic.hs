{-# LANGUAGE RecordWildCards #-}

module Network.QPACK.Table.Dynamic where

import Control.Concurrent.STM
import Data.Array.IO (IOArray)
import Data.IORef
import Network.HPACK.Internal

import Network.QPACK.Types

data DynamicTable = DynamicTable {
    droppingPoint     :: IORef AbsoluteIndex
  , drainingPoint     :: IORef AbsoluteIndex
  , insertionPoint    :: TVar InsertPoint
  , basePoint         :: IORef BasePoint
  , tatalNumOfEntries :: IORef Int
  , maxNumOfEntries   :: IORef Int
  , circularTable     :: IORef Table
  }

type Table = IOArray Index Entry

getMaxNumOfEntries :: DynamicTable -> IO Int
getMaxNumOfEntries DynamicTable{..} = readIORef maxNumOfEntries

getTotalNumOfEntries :: DynamicTable -> IO InsertPoint
getTotalNumOfEntries DynamicTable{..} = atomically $ readTVar insertionPoint

