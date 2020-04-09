module Network.QPACK (
  -- * Dynamic table
    DynamicTable
  , newDynamicTableForEncoding
  , newDynamicTableForDecoding
  , clearDynamicTable
  -- * QPACK encoder
  , encodeHeader
  , encodeTokenHeader
  , encodePrefix
  -- * Types
  , Size
  , EncodeStrategy(..)
  , CompressionAlgo(..)
  , TokenHeaderList
  , TokenHeader
  , Token
  ) where

import Network.QPACK.HeaderBlock
import Network.QPACK.Table
import Network.HPACK (TokenHeaderList, Size, EncodeStrategy(..), CompressionAlgo(..), TokenHeader)
import Network.HPACK.Token (Token)

