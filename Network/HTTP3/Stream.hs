module Network.HTTP3.Stream where

import Network.QUIC (StreamId)

import Imports

data H3StreamType = H3ControlStreams
                  | H3PushStreams
                  | QPACKEncoderStream
                  | QPACKDecoderStream
                  | H3StreamTypeUnknown Int64
                  deriving (Eq, Show)

fromH3StreamType :: H3StreamType -> Int64
fromH3StreamType H3ControlStreams        = 0x00
fromH3StreamType H3PushStreams           = 0x01
fromH3StreamType QPACKEncoderStream      = 0x02
fromH3StreamType QPACKDecoderStream      = 0x03
fromH3StreamType (H3StreamTypeUnknown i) = i

toH3StreamType :: Int64 -> H3StreamType
toH3StreamType 0x00 = H3ControlStreams
toH3StreamType 0x01 = H3PushStreams
toH3StreamType 0x02 = QPACKEncoderStream
toH3StreamType 0x03 = QPACKDecoderStream
toH3StreamType    i = H3StreamTypeUnknown i

clientControlStream,clientEncoderStream,clientDecoderStream :: StreamId
clientControlStream = 2
clientEncoderStream = 6
clientDecoderStream = 10

serverControlStream,serverEncoderStream,serverDecoderStream :: StreamId
serverControlStream = 3
serverEncoderStream = 7
serverDecoderStream = 11
