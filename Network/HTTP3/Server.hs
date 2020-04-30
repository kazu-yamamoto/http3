{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Server (
  -- * Runner
    run
  -- * Runner arguments
  -- * HTTP\/3 server
  , Server
  -- * Request
  , Request
  -- ** Accessing request
  , requestMethod
  , requestPath
  , requestAuthority
  , requestScheme
  , requestHeaders
  , requestBodySize
  , getRequestBodyChunk
  , getRequestTrailers
  -- * Aux
  , Aux
  , auxTimeHandle
  -- * Response
  , Response
  -- ** Creating response
  , responseNoBody
  , responseFile
  , responseStreaming
  , responseBuilder
  -- ** Accessing response
  , responseBodySize
  -- ** Trailers maker
  , TrailersMaker
  , NextTrailersMaker(..)
  , defaultTrailersMaker
  , setResponseTrailersMaker
  -- * Push promise
  , PushPromise
  , pushPromise
  , promiseRequestPath
  , promiseResponse
  , promiseWeight
  -- * Types
  , Path
  , Authority
  , Scheme
  , FileSpec(..)
  , FileOffset
  , ByteCount
  -- * RecvN
  , defaultReadN
  -- * Position read for files
  , PositionReadMaker
  , PositionRead
  , Sentinel(..)
  , defaultPositionReadMaker
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Internal as BS
import Data.IORef
import Foreign.ForeignPtr
import Network.HPACK (toHeaderTable, HeaderTable)
import qualified Network.HTTP.Types as HT
import Network.HTTP2.Internal
import Network.HTTP2.Server hiding (run)
import Network.HTTP2.Server.Internal
import Network.QUIC
import qualified System.TimeManager as T

import Imports
import Network.HTTP3.Context
import Network.HTTP3.Frame
import Network.HTTP3.Run

run :: Connection -> Server -> IO ()
run conn server = E.bracket open close $ \ctx -> do
    setupUnidirectional conn
    readerServer ctx server
  where
    open = do
        ref <- newIORef IInit
        newContext conn (controlStream ref)
    close = clearContext

readerServer :: Context -> Server -> IO ()
readerServer ctx server = loop
  where
    loop = do
        estrm <- accept ctx
        case estrm of
          Right strm -> process strm >> loop
          _          -> return ()
    process strm
      | isClientInitiatedUnidirectional sid = unidirectional ctx strm
      | isClientInitiatedBidirectional  sid = void $ forkIO $ request ctx server strm
      | isServerInitiatedUnidirectional sid = return () -- error
      | otherwise                           = return ()
      where
        sid = streamId strm

data Source = Source {
    sourceRead    :: IO ByteString
  , sourcePending :: IORef (Maybe ByteString)
  }

newSource :: Stream -> IO Source
newSource strm = Source (recvStream strm 1024) <$> newIORef Nothing

readSource :: Source -> IO ByteString
readSource Source{..} = do
    mx <- readIORef sourcePending
    case mx of
      Nothing -> sourceRead
      Just x  -> do
          writeIORef sourcePending Nothing
          return x

pushbackSource :: Source -> ByteString -> IO ()
pushbackSource Source{..} "" = return ()
pushbackSource Source{..} bs = writeIORef sourcePending $ Just bs

request :: Context -> Server -> Stream -> IO ()
request ctx server strm = do
    th <- registerThread ctx
    src <- newSource strm
    mvt <- parseHeader ctx src
    case mvt of
      Nothing -> return ()
      Just vt -> do
          -- fixme Content-Length
          refI <- newIORef IInit
          refH <- newIORef Nothing
          let readB = readBody ctx src refI refH
              req = Request $ InpObj vt Nothing readB refH
          let aux = Aux th
          server req aux $ sendResponse ctx strm th

parseHeader :: Context -> Source -> IO (Maybe HeaderTable)
parseHeader ctx src = loop IInit
  where
    loop st = do
        bs <- readSource src
        if bs == "" then
            return Nothing
          else case parseH3Frame st bs of
                 IDone H3FrameHeaders payload leftover -> do
                     pushbackSource src leftover
                     Just <$> qpackDecode ctx payload
                 st' -> loop st'

readBody :: Context -> Source -> IORef IFrame -> IORef (Maybe HeaderTable) -> IO ByteString
readBody ctx src refI refH = do
    st <- readIORef refI
    loop st
  where
    loop st = do
        bs <- readSource src
        if bs == "" then
            return ""
          else case parseH3Frame st bs of
                 IPay typ siz received bss -> do
                     writeIORef refI $ IPay typ siz received []
                     return $ BS.concat $ reverse bss
                 IDone H3FrameHeaders payload leftover -> do
                     writeIORef refI IInit
                     pushbackSource src leftover
                     parseHeader ctx src >>= writeIORef refH
                     return payload
                 st' -> loop st'

sendResponse :: Context -> Stream -> T.Handle -> Response -> [PushPromise] -> IO ()
sendResponse ctx strm th rsp@(Response outobj) _pp = do
    sendHeader ctx strm th $ outObjHeaders outobj
    sendBody   ctx strm th rsp
    shutdownStream strm

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
