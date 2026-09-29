{-# LANGUAGE PatternSynonyms #-}

module Network.QPACK.Error (
    -- * Errors
    ApplicationProtocolError (
        QpackDecompressionFailed,
        QpackEncoderStreamError,
        QpackDecoderStreamError
    ),
    DecodeError (..),
    FieldSectionTooLarge (..),
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
    | -- | An absolute index outside the dynamic table's live window
      IllegalDynamicIndex Int
    | IllegalInsertCount
    | BlockedStreamsOverflow
    deriving (Eq, Show)

-- | A field section that decodes to more than the
--   SETTINGS_MAX_FIELD_SECTION_SIZE we announced (RFC 9114, section 4.2.2).
--
-- Not a 'DecodeError': nothing is wrong with the encoding, and the
-- connection can go on.  The size is counted as the RFC counts it, the
-- lengths of each name and value plus 32 for every field.
data FieldSectionTooLarge = FieldSectionTooLarge
    deriving (Eq, Show)

data EncoderInstructionError = EncoderInstructionError
    deriving (Eq, Show)
data DecoderInstructionError = DecoderInstructionError
    deriving (Eq, Show)

instance E.Exception DecodeError
instance E.Exception FieldSectionTooLarge
instance E.Exception EncoderInstructionError
instance E.Exception DecoderInstructionError
