{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}

module Network.HTTP3.Frame (
    H3Frame(..)
  , H3FrameType(..)
  , fromH3FrameType
  , toH3FrameType
  , encodeH3Frame
  , encodeH3Frames
  , decodeH3Frame
  , IFrame(..)
  , parseH3Frame
  , QInt(..)
  , parseQInt
  ) where

import qualified Data.ByteString as BS
import Network.ByteOrder
import Network.QUIC.Internal

import Imports

data H3Frame = H3Frame H3FrameType ByteString

data H3FrameType = H3FrameData
                 | H3FrameHeaders
                 | H3FrameCancelPush
                 | H3FrameSettings
                 | H3FramePushPromise
                 | H3FrameGoaway
                 | H3FrameMaxPushId
                 | H3FrameUnknown Int64
                 deriving (Eq, Show)

fromH3FrameType :: H3FrameType -> Int64
fromH3FrameType H3FrameData        = 0x0
fromH3FrameType H3FrameHeaders     = 0x1
fromH3FrameType H3FrameCancelPush  = 0x3
fromH3FrameType H3FrameSettings    = 0x4
fromH3FrameType H3FramePushPromise = 0x5
fromH3FrameType H3FrameGoaway      = 0x7
fromH3FrameType H3FrameMaxPushId   = 0xD
fromH3FrameType (H3FrameUnknown i) =   i

toH3FrameType :: Int64 -> H3FrameType
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

encodeH3Frames :: [H3Frame] -> [ByteString]
encodeH3Frames fs0 = loop fs0 id
  where
    loop []                  build = build []
    loop (H3Frame ty val:fs) build = loop fs (build . (typ :) . (len :) . (val :))
      where
        typ = encodeInt $ fromIntegral $ fromH3FrameType ty
        len = encodeInt $ fromIntegral $ BS.length val

decodeH3Frame :: ByteString -> IO H3Frame
decodeH3Frame hf = withReadBuffer hf $ \rbuf -> do
    typ <- toH3FrameType . fromIntegral <$> decodeInt' rbuf
    len <- fromIntegral <$> decodeInt' rbuf
    bs <- extractByteString rbuf len
    return $ H3Frame typ bs

data QInt = QInit
          | QMore Word8        -- ^ Masked first byte
                  Int          -- ^ Bytes required
                  Int          -- ^ Bytes received so far. (sum . map length)
                  [ByteString] -- ^ Reverse order
          | QDone Int64        -- ^ Result
                  ByteString   -- ^ leftover
          deriving (Eq,Show)

parseQInt :: QInt -> ByteString -> QInt
parseQInt st "" = st
parseQInt QInit bs0
  | len1 < reqLen = QMore ft reqLen len1 [bs1]
  | otherwise     = let (bs2,bs3) = BS.splitAt reqLen bs1
                    in QDone (toLen ft bs2) bs3
  where
    hd = BS.head bs0
    reqLen = requiredLen (hd .&. 0b11000000)
    ft = hd .&. 0b00111111
    bs1  = BS.tail bs0
    len1 = BS.length bs1
parseQInt (QMore ft reqLen len0 bss0) bs0
  | len1 < reqLen = QMore ft reqLen len1 (bs0:bss0)
  | otherwise     = let (bs2,bs3) = BS.splitAt reqLen $ compose bs0 bss0
                    in QDone (toLen ft bs2) bs3
  where
    len1 = len0 + BS.length bs0
parseQInt (QDone _ _) _ = error "parseQInt"

requiredLen :: Word8 -> Int
requiredLen 0b00000000 = 0
requiredLen 0b01000000 = 1
requiredLen 0b10000000 = 3
requiredLen _          = 7

toLen :: Word8 -> ByteString -> Int64
toLen w0 bs = BS.foldl (\n w -> n * 256 + fromIntegral w) (fromIntegral w0) bs

data IFrame =
            -- | Parsing is about to start
              IInit
            -- | Parsing type
            | IType QInt
            -- | Parsing length
            | ILen H3FrameType QInt
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
parseH3Frame IInit bs = case parseQInt QInit bs of
    QDone i bs' -> let typ = toH3FrameType i
                   in parseH3Frame (ILen typ QInit) bs'
    ist         -> IType ist
parseH3Frame (IType ist) bs = case parseQInt ist bs of
    QDone i bs' -> let typ = toH3FrameType i
                   in parseH3Frame (ILen typ QInit) bs'
    ist'        -> IType ist'
parseH3Frame (ILen typ ist) bs = case parseQInt ist bs of
    QDone i bs' -> let reqLen = fromIntegral i
                   in if reqLen == 0 then
                        IDone typ "" bs'
                      else
                        parseH3Frame (IPay typ reqLen 0 []) bs'
    ist'        -> ILen typ ist'
parseH3Frame (IPay typ reqLen len0 bss0) bs0 = case len1 `compare` reqLen of
    LT -> IPay typ reqLen len1 (bs0:bss0)
    EQ -> IDone typ (compose bs0 bss0) ""
    GT -> let (bs2,leftover) = BS.splitAt (reqLen - len0) bs0
          in IDone typ (compose bs2 bss0) leftover
  where
    len1 = len0 + BS.length bs0
parseH3Frame st _ = st

compose :: ByteString -> [ByteString] -> ByteString
compose bs bss = BS.concat $ reverse (bs:bss)

{-
test :: Int64 -> QInt
tset i = loop QInit bss0
  where
    loop st [] = st
    loop st (bs:bss) = case parseQInt st bs of
        st1@(QDone _ _) -> st1
        st1             -> loop st1 bss
    bs0 = encodeInt i
    bss0 = map BS.singleton $ BS.unpack bs0
-}
