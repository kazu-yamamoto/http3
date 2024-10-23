{-# LANGUAGE PatternSynonyms #-}

module Network.QPACK.Error (
    -- * Errors
    ApplicationProtocolError (
        QpackDecompressionFailed,
        QpackEncoderStreamError,
        QpackDecoderStreamError
    ),
    DecodeError (..),
    EncoderInstructionError (..),
    DecoderInstructionError (..),
) where

import Control.Exception
import Data.Typeable

import Network.QUIC

{- FOURMOLU_DISABLE -}
pattern QpackDecompressionFailed :: ApplicationProtocolError
pattern QpackDecompressionFailed  = ApplicationProtocolError 0x200

pattern QpackEncoderStreamError  :: ApplicationProtocolError
pattern QpackEncoderStreamError   = ApplicationProtocolError 0x201

pattern QpackDecoderStreamError  :: ApplicationProtocolError
pattern QpackDecoderStreamError   = ApplicationProtocolError 0x202
{- FOURMOLU_ENABLE -}

data DecodeError
    = IllegalStaticIndex
    | IllegalInsertCount
    deriving (Eq, Show, Typeable)

data EncoderInstructionError = EncoderInstructionError
    deriving (Eq, Show, Typeable)
data DecoderInstructionError = DecoderInstructionError
    deriving (Eq, Show, Typeable)

instance Exception DecodeError
instance Exception EncoderInstructionError
instance Exception DecoderInstructionError
