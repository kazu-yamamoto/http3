module Network.QPACK.Table.Dynamic where

import Control.Exception (bracket, throwIO)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOArray, newArray)
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import Foreign.Marshal.Alloc (mallocBytes, free)
import Network.ByteOrder (newWriteBuffer)
import Network.HPACK.Internal
import Control.Concurrent.STM

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
