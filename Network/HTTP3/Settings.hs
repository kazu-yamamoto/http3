module Network.HTTP3.Settings where

import Network.ByteOrder
import Network.QUIC.Internal

type H3Settings = [(H3SettingsKey,Int)]

data H3SettingsKey = QpackMaxTableCapacity
                   | SettingsMaxHeaderListSize
                   | QpackBlockedStreams
                   | H3SettingsKeyUnknown Int
                   deriving (Eq, Show)

fromH3SettingsKey :: H3SettingsKey -> Int
fromH3SettingsKey QpackMaxTableCapacity     = 0x1
fromH3SettingsKey SettingsMaxHeaderListSize = 0x6
fromH3SettingsKey QpackBlockedStreams       = 0x7
fromH3SettingsKey (H3SettingsKeyUnknown i)  =   i

toH3SettingsKey :: Int -> H3SettingsKey
toH3SettingsKey 0x1 = QpackMaxTableCapacity
toH3SettingsKey 0x6 = SettingsMaxHeaderListSize
toH3SettingsKey 0x7 = QpackBlockedStreams
toH3SettingsKey   i = H3SettingsKeyUnknown i

encodeH3Settings :: H3Settings -> IO ByteString
encodeH3Settings kvs = withWriteBuffer 128 $ \wbuf -> do
    mapM_ (enc wbuf) kvs
  where
    enc wbuf (k,v) = do
        encodeInt' wbuf $ fromIntegral $ fromH3SettingsKey k
        encodeInt' wbuf $ fromIntegral v

decodeH3Settings :: ByteString -> IO H3Settings
decodeH3Settings bs = withReadBuffer bs $ \rbuf -> loop rbuf id
  where
    dec rbuf = do
        k <- toH3SettingsKey . fromIntegral <$> decodeInt' rbuf
        v <- fromIntegral <$> decodeInt' rbuf
        return (k,v)
    loop rbuf build = do
        r <- remainingSize rbuf
        if r <= 0 then
            return $ build []
          else do
            kv <- dec rbuf
            loop rbuf (build . (kv :))
