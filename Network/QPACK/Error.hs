{-# LANGUAGE PatternSynonyms #-}

module Network.QPACK.Error (
  -- * Errors
    ApplicationProtocolError(QpackDecompressionFailed
                            ,QpackEncoderStreamError
                            ,QpackDecoderStreamError
                            )
  , DecodeError(..)
  ) where

import Control.Exception
import Data.Typeable

import Network.QUIC

pattern QpackDecompressionFailed :: ApplicationProtocolError
pattern QpackDecompressionFailed  = ApplicationProtocolError 0x200

pattern QpackEncoderStreamError  :: ApplicationProtocolError
pattern QpackEncoderStreamError   = ApplicationProtocolError 0x201

pattern QpackDecoderStreamError  :: ApplicationProtocolError
pattern QpackDecoderStreamError   = ApplicationProtocolError 0x202

data DecodeError = IllegalStaticIndex
                 | IllegalInsertCount
                 deriving (Eq,Show,Typeable)

instance Exception DecodeError
