{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}

module Network.HTTP3.Frame (
    H3Frame(..)
  , H3FrameType(..)
  , fromH3FrameType
  , toH3FrameType
  , encodeH3Frame
  , decodeH3Frame
  , IFrame(..)
  , parseH3Frame
  ) where

import qualified Data.ByteString as BS
import Network.ByteOrder
import Network.QUIC.Types

import Imports

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

data IFrame =
            -- | Parsing is about to start
              IInit
            -- | Parsed type only
            | IType H3FrameType
            -- | Parsing length
            | ILen H3FrameType
                   Word8 -- ^ Masked first byte
                   Int   -- ^ Bytes required
                   Int   -- ^ Bytes received so far. (sum . map length)
                   [ByteString] -- ^ Reverse order
            -- | Parsing payload
            | IPay H3FrameType
                   Int -- ^ Bytes required
                   Int -- ^ Bytes received so far.  (sum . map length)
                   [ByteString] -- ^ Reverse order
            -- | Parsing done
            | IDone H3FrameType
                    ByteString -- ^ Payload (entire or sentinel)
                    ByteString -- ^ Leftover
            deriving (Eq, Show)

parseH3Frame :: IFrame -> ByteString -> IFrame
parseH3Frame st "" = st
parseH3Frame IInit bs = parseH3Frame (IType typ) $ BS.tail bs
  where
    typ = toH3FrameType $ fromIntegral $ BS.head bs
parseH3Frame (IType typ) bs0
  | len1 < reqLen = ILen typ ft reqLen len1 [bs1]
  | otherwise    = let (bs2,bs3) = BS.splitAt reqLen bs1
                   in parseH3Frame (IPay typ (toLen ft bs2) 0 []) bs3
  where
    hd = BS.head bs0
    reqLen = requiredLen (hd .&. 0b11000000)
    ft = hd .&. 0b00111111
    bs1  = BS.tail bs0
    len1 = BS.length bs1
parseH3Frame (ILen typ ft reqLen len0 bss0) bs0
   | len1 < reqLen = ILen typ ft reqLen len1 (bs0:bss0)
   | otherwise     = let bs = compose bs0 bss0
                         (bslen,bstl) = BS.splitAt reqLen bs
                     in parseH3Frame (IPay typ (toLen ft bslen) 0 []) bstl
  where
    len1 = len0 + BS.length bs0
parseH3Frame (IPay typ reqLen len0 bss0) bs0 = case len1 `compare` reqLen of
    LT -> IPay typ reqLen len1 (bs0:bss0)
    EQ -> IDone typ (compose bs0 bss0) ""
    GT -> let (bs2,leftover) = BS.splitAt (reqLen - len0) bs0
          in IDone typ (compose bs2 bss0) leftover
  where
    len1 = len0 + BS.length bs0
parseH3Frame (IDone _ _ _) bs = parseH3Frame IInit bs

compose :: ByteString -> [ByteString] -> ByteString
compose bs bss = BS.concat $ reverse (bs:bss)

requiredLen :: Word8 -> Int
requiredLen 0b00000000 = 0
requiredLen 0b01000000 = 1
requiredLen 0b10000000 = 3
requiredLen _          = 7

toLen :: Word8 -> ByteString -> Int
toLen w0 bs = BS.foldl (\n w -> n * 256 + fromIntegral w) (fromIntegral w0) bs
