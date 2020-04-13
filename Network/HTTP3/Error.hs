module Network.HTTP3.Error where

data HTTP3Error = H3NoError
                | H3GeneralProtocolError
                | H3InternalError
                | H3StreamCreationError
                | H3ClosedCriticalStream
                | H3FrameUnexpected
                | H3FrameError
                | H3ExcessiveLoad
                | H3IdError
                | H3SettingsError
                | H3MissingSettings
                | H3RequestRejected
                | H3RequestCancelled
                | H3RequestIncomplete
                | H3ConnectError
                | H3VersionFallback
                | QpackDecompressionFailed
                | QpackEncoderStreamError
                | QpackDecoderStreamError
                | H3UnknownError Int
                deriving (Eq, Show)

fromHTTP3Error :: HTTP3Error -> Int
fromHTTP3Error H3NoError                = 0x100
fromHTTP3Error H3GeneralProtocolError   = 0x101
fromHTTP3Error H3InternalError          = 0x102
fromHTTP3Error H3StreamCreationError    = 0x103
fromHTTP3Error H3ClosedCriticalStream   = 0x104
fromHTTP3Error H3FrameUnexpected        = 0x105
fromHTTP3Error H3FrameError             = 0x106
fromHTTP3Error H3ExcessiveLoad          = 0x107
fromHTTP3Error H3IdError                = 0x108
fromHTTP3Error H3SettingsError          = 0x109
fromHTTP3Error H3MissingSettings        = 0x10A
fromHTTP3Error H3RequestRejected        = 0x10B
fromHTTP3Error H3RequestCancelled       = 0x10C
fromHTTP3Error H3RequestIncomplete      = 0x10D
fromHTTP3Error H3ConnectError           = 0x10F
fromHTTP3Error H3VersionFallback        = 0x110
fromHTTP3Error QpackDecompressionFailed = 0x200
fromHTTP3Error QpackEncoderStreamError  = 0x201
fromHTTP3Error QpackDecoderStreamError  = 0x202
fromHTTP3Error (H3UnknownError e)       = e

toHTTP3Error :: Int -> HTTP3Error
toHTTP3Error 0x100 = H3NoError
toHTTP3Error 0x101 = H3GeneralProtocolError
toHTTP3Error 0x102 = H3InternalError
toHTTP3Error 0x103 = H3StreamCreationError
toHTTP3Error 0x104 = H3ClosedCriticalStream
toHTTP3Error 0x105 = H3FrameUnexpected
toHTTP3Error 0x106 = H3FrameError
toHTTP3Error 0x107 = H3ExcessiveLoad
toHTTP3Error 0x108 = H3IdError
toHTTP3Error 0x109 = H3SettingsError
toHTTP3Error 0x10A = H3MissingSettings
toHTTP3Error 0x10B = H3RequestRejected
toHTTP3Error 0x10C = H3RequestCancelled
toHTTP3Error 0x10D = H3RequestIncomplete
toHTTP3Error 0x10F = H3ConnectError
toHTTP3Error 0x110 = H3VersionFallback
toHTTP3Error 0x200 = QpackDecompressionFailed
toHTTP3Error 0x201 = QpackEncoderStreamError
toHTTP3Error 0x202 = QpackDecoderStreamError
toHTTP3Error     e = H3UnknownError e
