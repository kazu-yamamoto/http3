module Network.HTTP3.Stream where

data H3StreamType = H3ControlStreams
                  | H3PushStreams
                  | QPACKEncoderStream
                  | QPACKDecoderStream
                  | H3StreamTypeUnknown Int
                  deriving (Eq, Show)

fromH3StreamType :: H3StreamType -> Int
fromH3StreamType H3ControlStreams        = 0x00
fromH3StreamType H3PushStreams           = 0x01
fromH3StreamType QPACKEncoderStream      = 0x02
fromH3StreamType QPACKDecoderStream      = 0x03
fromH3StreamType (H3StreamTypeUnknown i) = i

toH3StreamType :: Int -> H3StreamType
toH3StreamType 0x00 = H3ControlStreams
toH3StreamType 0x01 = H3PushStreams
toH3StreamType 0x02 = QPACKEncoderStream
toH3StreamType 0x03 = QPACKDecoderStream
toH3StreamType    i = H3StreamTypeUnknown i
