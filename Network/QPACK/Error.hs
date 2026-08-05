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

import qualified Control.Exception as E

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
    = IllegalStaticIndex Int
    | IllegalInsertCount
    | BlockedStreamsOverflow
    deriving (Eq, Show)

data EncoderInstructionError = EncoderInstructionError
    deriving (Eq, Show)
data DecoderInstructionError = DecoderInstructionError
    deriving (Eq, Show)

instance E.Exception DecodeError
instance E.Exception EncoderInstructionError
instance E.Exception DecoderInstructionError
