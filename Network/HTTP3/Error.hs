{-# LANGUAGE PatternSynonyms #-}

module Network.HTTP3.Error where

newtype HTTP3Error = HTTP3Error Int deriving (Eq, Show)

pattern H3NoError               :: HTTP3Error
pattern H3NoError                = HTTP3Error 0x100

pattern H3GeneralProtocolError  :: HTTP3Error
pattern H3GeneralProtocolError   = HTTP3Error 0x101

pattern H3InternalError         :: HTTP3Error
pattern H3InternalError          = HTTP3Error 0x102

pattern H3StreamCreationError   :: HTTP3Error
pattern H3StreamCreationError    = HTTP3Error 0x103

pattern H3ClosedCriticalStream  :: HTTP3Error
pattern H3ClosedCriticalStream   = HTTP3Error 0x104

pattern H3FrameUnexpected       :: HTTP3Error
pattern H3FrameUnexpected        = HTTP3Error 0x105

pattern H3FrameError            :: HTTP3Error
pattern H3FrameError             = HTTP3Error 0x106

pattern H3ExcessiveLoad         :: HTTP3Error
pattern H3ExcessiveLoad          = HTTP3Error 0x107

pattern H3IdError               :: HTTP3Error
pattern H3IdError                = HTTP3Error 0x108

pattern H3SettingsError         :: HTTP3Error
pattern H3SettingsError          = HTTP3Error 0x109

pattern H3MissingSettings       :: HTTP3Error
pattern H3MissingSettings        = HTTP3Error 0x10A

pattern H3RequestRejected       :: HTTP3Error
pattern H3RequestRejected        = HTTP3Error 0x10B

pattern H3RequestCancelled      :: HTTP3Error
pattern H3RequestCancelled       = HTTP3Error 0x10C

pattern H3RequestIncomplete     :: HTTP3Error
pattern H3RequestIncomplete      = HTTP3Error 0x10D

pattern H3ConnectError          :: HTTP3Error
pattern H3ConnectError           = HTTP3Error 0x10F

pattern H3VersionFallback       :: HTTP3Error
pattern H3VersionFallback        = HTTP3Error 0x110

pattern QpackDecompressionFailed :: HTTP3Error
pattern QpackDecompressionFailed = HTTP3Error 0x200

pattern QpackEncoderStreamError :: HTTP3Error
pattern QpackEncoderStreamError  = HTTP3Error 0x201

pattern QpackDecoderStreamError :: HTTP3Error
pattern QpackDecoderStreamError  = HTTP3Error 0x202
