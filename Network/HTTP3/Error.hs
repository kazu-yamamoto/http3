{-# LANGUAGE PatternSynonyms #-}

module Network.HTTP3.Error (
    ApplicationProtocolError (
        H3NoError,
        H3GeneralProtocolError,
        H3InternalError,
        H3StreamCreationError,
        H3ClosedCriticalStream,
        H3FrameUnexpected,
        H3FrameError,
        H3ExcessiveLoad,
        H3IdError,
        H3SettingsError,
        H3MissingSettings,
        H3RequestRejected,
        H3RequestCancelled,
        H3RequestIncomplete,
        H3MessageError,
        H3ConnectError,
        H3VersionFallback
    ),
) where

import Network.QUIC

{- FOURMOLU_DISABLE -}
pattern H3NoError                :: ApplicationProtocolError
pattern H3NoError                 = ApplicationProtocolError 0x100

pattern H3GeneralProtocolError   :: ApplicationProtocolError
pattern H3GeneralProtocolError    = ApplicationProtocolError 0x101

pattern H3InternalError          :: ApplicationProtocolError
pattern H3InternalError           = ApplicationProtocolError 0x102

pattern H3StreamCreationError    :: ApplicationProtocolError
pattern H3StreamCreationError     = ApplicationProtocolError 0x103

pattern H3ClosedCriticalStream   :: ApplicationProtocolError
pattern H3ClosedCriticalStream    = ApplicationProtocolError 0x104

pattern H3FrameUnexpected        :: ApplicationProtocolError
pattern H3FrameUnexpected         = ApplicationProtocolError 0x105

pattern H3FrameError             :: ApplicationProtocolError
pattern H3FrameError              = ApplicationProtocolError 0x106

pattern H3ExcessiveLoad          :: ApplicationProtocolError
pattern H3ExcessiveLoad           = ApplicationProtocolError 0x107

pattern H3IdError                :: ApplicationProtocolError
pattern H3IdError                 = ApplicationProtocolError 0x108

pattern H3SettingsError          :: ApplicationProtocolError
pattern H3SettingsError           = ApplicationProtocolError 0x109

pattern H3MissingSettings        :: ApplicationProtocolError
pattern H3MissingSettings         = ApplicationProtocolError 0x10a

pattern H3RequestRejected        :: ApplicationProtocolError
pattern H3RequestRejected         = ApplicationProtocolError 0x10b

pattern H3RequestCancelled       :: ApplicationProtocolError
pattern H3RequestCancelled        = ApplicationProtocolError 0x10c

pattern H3RequestIncomplete      :: ApplicationProtocolError
pattern H3RequestIncomplete       = ApplicationProtocolError 0x10d

pattern H3MessageError           :: ApplicationProtocolError
pattern H3MessageError            = ApplicationProtocolError 0x10e

pattern H3ConnectError           :: ApplicationProtocolError
pattern H3ConnectError            = ApplicationProtocolError 0x10f

pattern H3VersionFallback        :: ApplicationProtocolError
pattern H3VersionFallback         = ApplicationProtocolError 0x110
{- FOURMOLU_ENABLE -}
