{-# LANGUAGE PatternSynonyms #-}

module Network.HTTP3.Settings where

import Network.ByteOrder
import Network.QUIC.Internal

type H3Settings = [(H3SettingsKey, Int)]

newtype H3SettingsKey = H3SettingsKey Int deriving (Eq, Show)

{- FOURMOLU_DISABLE -}
pattern SettingsQpackMaxTableCapacity :: H3SettingsKey
pattern SettingsQpackMaxTableCapacity  = H3SettingsKey 0x1

pattern SettingsMaxFieldSectionSize   :: H3SettingsKey
pattern SettingsMaxFieldSectionSize    = H3SettingsKey 0x6

pattern SettingsQpackBlockedStreams   :: H3SettingsKey
pattern SettingsQpackBlockedStreams    = H3SettingsKey 0x7
{- FOURMOLU_ENABLE -}

encodeH3Settings :: H3Settings -> IO ByteString
encodeH3Settings kvs = withWriteBuffer 128 $ \wbuf -> do
    mapM_ (enc wbuf) kvs
  where
    enc wbuf (H3SettingsKey k, v) = do
        encodeInt' wbuf $ fromIntegral k
        encodeInt' wbuf $ fromIntegral v

decodeH3Settings :: ByteString -> IO H3Settings
decodeH3Settings bs = withReadBuffer bs $ \rbuf -> loop rbuf id
  where
    dec rbuf = do
        k <- H3SettingsKey . fromIntegral <$> decodeInt' rbuf
        v <- fromIntegral <$> decodeInt' rbuf
        return (k, v)
    loop rbuf build = do
        r <- remainingSize rbuf
        if r <= 0
            then return $ build []
            else do
                kv <- dec rbuf
                loop rbuf (build . (kv :))
