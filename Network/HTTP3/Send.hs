{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Send (
    sendHeader
  , sendBody
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Internal as BS
import Foreign.ForeignPtr
import Network.HPACK (toHeaderTable)
import qualified Network.HTTP.Types as HT
import Network.HTTP2.Internal
import Network.HTTP2.Server hiding (run)
import Network.HTTP2.Server.Internal
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

sendBody :: Context -> Stream -> T.Handle -> Response -> IO ()
sendBody ctx strm th (Response outobj) = case outObjBody outobj of
    OutBodyNone -> return ()
    OutBodyFile (FileSpec path fileoff bytecount) -> do
        (pread, sentinel') <- defaultPositionReadMaker path
        refresh <- case sentinel' of
                     Closer closer       -> timeoutClose ctx closer
                     Refresher refresher -> return refresher
        let next = fillFileBodyGetNext pread fileoff bytecount refresh
        sendNext ctx strm th next tlrrmkr
    OutBodyBuilder builder -> do
        let next = fillBuilderBodyGetNext builder
        sendNext ctx strm th next tlrrmkr
    OutBodyStreaming strmbdy -> do
        tbq <- newTBQueueIO 10
        let takeQ = atomically $ tryReadTBQueue tbq
        let next = fillStreamBodyGetNext takeQ
        void $ forkIO $ processStreaming ctx strmbdy tbq
        sendNext ctx strm th next tlrrmkr
  where
    tlrrmkr = outObjTrailers outobj

sendNext :: Context -> Stream -> T.Handle -> DynaNext -> TrailersMaker -> IO ()
sendNext ctx strm th curr tlrmkr0 = do
    (bs, mnext, tlrmkr) <- newByteStringWith tlrmkr0 curr
    encodeH3Frame (H3Frame H3FrameData bs) >>= sendStream strm
    T.tickle th
    case mnext of
      Nothing -> do
          Trailers trailers <- tlrmkr Nothing
          unless (null trailers) $ sendHeader ctx strm th trailers
      Just next -> sendNext ctx strm th next tlrmkr

newByteStringWith :: TrailersMaker -> DynaNext -> IO (ByteString, Maybe DynaNext, TrailersMaker)
newByteStringWith tlrmkr0 action= do
    fp <- BS.mallocByteString 2048
    withForeignPtr fp $ \buf -> do
        Next len mnext1 <- action buf 2048 65536 -- window size
        NextTrailersMaker tlrmkr1 <- runTrailersMaker tlrmkr0 buf len
        let bs = PS fp 0 len
        return (bs, mnext1, tlrmkr1)

processStreaming :: Context
                 -> ((BS.Builder -> IO ()) -> IO () -> IO ())
                 -> TBQueue StreamingChunk
                 -> IO ()
processStreaming ctx strmbdy tbq = do
    th <- registerThread ctx
    let push b = do
            T.pause th
            atomically $ writeTBQueue tbq (StreamingBuilder b)
            T.resume th
        flush  = atomically $ writeTBQueue tbq StreamingFlush
    strmbdy push flush
    atomically $ writeTBQueue tbq StreamingFinished
