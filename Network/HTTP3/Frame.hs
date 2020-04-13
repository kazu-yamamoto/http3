module Network.HTTP3.Frame where

import qualified Data.ByteString as BS
import Network.ByteOrder
import Network.QUIC.Types

data H3Frame = H3Frame H3FrameType ByteString

data H3FrameType = H3FrameData
                 | H3FrameHeaders
                 | H3FrameCancelPush
                 | H3FrameSettings
                 | H3FramePushPromise
                 | H3FrameGoaway
                 | H3FrameMaxPushId
                 | H3FrameUnknown Int
                 deriving (Eq, Show)

fromH3FrameType :: H3FrameType -> Int
fromH3FrameType H3FrameData        = 0x0
fromH3FrameType H3FrameHeaders     = 0x1
fromH3FrameType H3FrameCancelPush  = 0x3
fromH3FrameType H3FrameSettings    = 0x4
fromH3FrameType H3FramePushPromise = 0x5
fromH3FrameType H3FrameGoaway      = 0x7
fromH3FrameType H3FrameMaxPushId   = 0xD
fromH3FrameType (H3FrameUnknown i) =   i

toH3FrameType :: Int -> H3FrameType
toH3FrameType 0x0 = H3FrameData
toH3FrameType 0x1 = H3FrameHeaders
toH3FrameType 0x3 = H3FrameCancelPush
toH3FrameType 0x4 = H3FrameSettings
toH3FrameType 0x5 = H3FramePushPromise
toH3FrameType 0x7 = H3FrameGoaway
toH3FrameType 0xD = H3FrameMaxPushId
toH3FrameType   i = H3FrameUnknown i

encodeH3Frame :: H3Frame -> IO ByteString
encodeH3Frame (H3Frame typ bs) = do
    tl <- withWriteBuffer 16 $ \wbuf -> do
        encodeInt' wbuf $ fromIntegral $ fromH3FrameType typ
        encodeInt' wbuf $ fromIntegral $ BS.length bs
    return $ tl `BS.append` bs

decodeH3Frame :: ByteString -> IO H3Frame
decodeH3Frame hf = withReadBuffer hf $ \rbuf -> do
    typ <- toH3FrameType . fromIntegral <$> decodeInt' rbuf
    len <- fromIntegral <$> decodeInt' rbuf
    bs <- extractByteString rbuf len
    return $ H3Frame typ bs
