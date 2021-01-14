{-# LANGUAGE PatternSynonyms #-}

module Network.QPACK.Error (
    ApplicationProtocolError(QpackDecompressionFailed
                            ,QpackEncoderStreamError
                            ,QpackDecoderStreamError
                            )
  ) where

import Network.QUIC

pattern QpackDecompressionFailed :: ApplicationProtocolError
pattern QpackDecompressionFailed  = ApplicationProtocolError 0x200

pattern QpackEncoderStreamError  :: ApplicationProtocolError
pattern QpackEncoderStreamError   = ApplicationProtocolError 0x201

pattern QpackDecoderStreamError  :: ApplicationProtocolError
pattern QpackDecoderStreamError   = ApplicationProtocolError 0x202
