{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP3.Send (
    sendHeader
  , sendBody
  ) where

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Internal as BS
import Data.IORef
import Foreign.ForeignPtr
import Network.HPACK (toHeaderTable)
import qualified Network.HTTP.Types as HT
import Network.HTTP2.Internal
import Network.QUIC
import qualified System.TimeManager as T

import Imports
import Network.HTTP3.Context
import Network.HTTP3.Frame

sendHeader :: Context -> Stream -> T.Handle -> HT.ResponseHeaders -> IO ()
sendHeader ctx strm th hdrs = do
    -- fixme: fixHeaders
    (ths, _) <- toHeaderTable hdrs
    (hdr, "") <- qpackEncode ctx ths
    hdrblock <- encodeH3Frame $ H3Frame H3FrameHeaders hdr
    sendStream strm hdrblock
    T.tickle th

sendBody :: Context -> Stream -> T.Handle -> OutObj -> IO ()
sendBody ctx strm th outobj = case outObjBody outobj of
    OutBodyNone -> return ()
    OutBodyFile (FileSpec path fileoff bytecount) -> do
        (pread, sentinel') <- pReadMaker ctx path
        refresh <- case sentinel' of
                     Closer closer       -> timeoutClose ctx closer
                     Refresher refresher -> return refresher
        let next = fillFileBodyGetNext pread fileoff bytecount refresh
        sendNext ctx strm th next tlrmkr
    OutBodyBuilder builder -> do
        let next = fillBuilderBodyGetNext builder
        sendNext ctx strm th next tlrmkr
    OutBodyStreaming strmbdy -> sendStreaming ctx strm th strmbdy tlrmkr
  where
    tlrmkr = outObjTrailers outobj

sendNext :: Context -> Stream -> T.Handle -> DynaNext -> TrailersMaker -> IO ()
sendNext ctx strm th curr tlrmkr0 = do
    (bs, mnext, tlrmkr) <- newByteStringWith tlrmkr0 curr
    when (bs /= "") $ encodeH3Frame (H3Frame H3FrameData bs) >>= sendStream strm
    T.tickle th
    case mnext of
      Nothing -> do
          Trailers trailers <- tlrmkr Nothing
          unless (null trailers) $ sendHeader ctx strm th trailers
      Just next -> sendNext ctx strm th next tlrmkr

newByteStringWith :: TrailersMaker -> DynaNext -> IO (ByteString, Maybe DynaNext, TrailersMaker)
newByteStringWith tlrmkr0 action = do
    fp <- BS.mallocByteString 2048
    Next len mnext1 <- withForeignPtr fp $ \buf -> action buf 2048 65536 -- window size
    if len == 0 then
        return ("", Nothing, tlrmkr0)
      else do
        let bs = PS fp 0 len
        NextTrailersMaker tlrmkr1 <- tlrmkr0 $ Just bs
        return (bs, mnext1, tlrmkr1)

newByteStringAndSend :: Stream -> T.Handle -> TrailersMaker -> B.BufferWriter
                     -> IO (B.Next, TrailersMaker)
newByteStringAndSend strm th tlrmkr0 action = do
    fp <- BS.mallocByteString 2048
    (len, signal) <- withForeignPtr fp $ \buf -> action buf 2048
    if len == 0 then
        return (signal, tlrmkr0)
      else do
        let bs = PS fp 0 len
        NextTrailersMaker tlrmkr1 <- tlrmkr0 $ Just bs
        encodeH3Frame (H3Frame H3FrameData bs) >>= sendStream strm
        T.tickle th
        return (signal, tlrmkr1)

sendStreaming :: Context -> Stream -> T.Handle -> ((Builder -> IO ()) -> IO () -> IO ()) -> TrailersMaker -> IO ()
sendStreaming ctx strm th strmbdy tlrmkr0 = do
    ref <- newIORef tlrmkr0
    strmbdy (write ref) flush
    tlrmkr <- readIORef ref
    Trailers trailers <- tlrmkr Nothing
    unless (null trailers) $ sendHeader ctx strm th trailers
  where
    flush = return ()
    write ref builder = do
        tlrmkr1 <- readIORef ref
        tlrmkr2 <- newByteStringAndSend strm th tlrmkr1 (B.runBuilder builder) >>= loop
        writeIORef ref tlrmkr2
      where
        loop (B.Done,           tlrmkr1) = return tlrmkr1
        loop (B.More _ writer,  tlrmkr1) =
            newByteStringAndSend strm th tlrmkr1 writer >>= loop
        loop (B.Chunk bs writer, tlrmkr1) = do
            encodeH3Frame (H3Frame H3FrameData bs) >>= sendStream strm
            NextTrailersMaker tlrmkr2 <- tlrmkr1 $ Just bs
            T.tickle th
            newByteStringAndSend strm th tlrmkr2 writer >>= loop
