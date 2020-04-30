{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Client (
  -- * Runner
    run
  , Scheme
  , Authority
  -- * Runner arguments
  -- * HTTP\/3 client
  , Client
  -- * Request
  , Request
  -- * Creating request
  , requestNoBody
  , requestFile
  , requestStreaming
  , requestBuilder
  -- ** Trailers maker
  , TrailersMaker
  , NextTrailersMaker(..)
  , defaultTrailersMaker
  , setRequestTrailersMaker
  -- * Response
  , Response
  -- ** Accessing response
  , responseStatus
  , responseHeaders
  , responseBodySize
  , getResponseBodyChunk
  , getResponseTrailers
  -- * Types
  , Method
  , Path
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
import qualified Control.Exception as E
import Data.IORef
import Network.HTTP2.Client hiding (run, Config, allocSimpleConfig, freeSimpleConfig)
import Network.HTTP2.Client.Internal
import Network.HTTP2.Internal
import Network.QUIC

import Network.HTTP3.Context
import Network.HTTP3.Control
import Network.HTTP3.Frame
import Network.HTTP3.Recv
import Network.HTTP3.Send

run :: Connection -> Scheme -> Authority -> Client a -> IO a
run conn scheme auth client = E.bracket open close $ \ctx -> do
    setupUnidirectional conn
    tid <- forkIO $ readerClient ctx
    client (sendRequest ctx scheme auth) `E.finally` do
        killThread tid
  where
    open = do
        ref <- newIORef IInit
        newContext conn (controlStream ref)
    close = clearContext

readerClient :: Context -> IO ()
readerClient ctx = loop
  where
    loop = do
        estrm <- accept ctx
        case estrm of
          Right strm -> process strm >> loop
          _          -> return ()
    process strm
      | isClientInitiatedUnidirectional sid = return () -- error
      | isClientInitiatedBidirectional  sid = return ()
      | isServerInitiatedUnidirectional sid = unidirectional ctx strm
      | otherwise                           = return () -- push?
      where
        sid = streamId strm

sendRequest :: Context -> Scheme -> Authority -> Request -> (Response -> IO a) -> IO a
sendRequest ctx scheme auth (Request outobj) processResponse = do
    th <- registerThread ctx
    let hdr = outObjHeaders outobj
        hdr' = (":scheme", scheme)
             : (":authority", auth)
             : hdr
    strm <- newStream ctx
    sendHeader ctx strm th hdr'
    sendBody ctx strm th outobj
    shutdownStream strm
    src <- newSource strm
    mvt <- recvHeader ctx src
    case mvt of
      Nothing -> error ""
      Just vt -> do
          refI <- newIORef IInit
          refH <- newIORef Nothing
          let readB = recvBody ctx src refI refH
              rsp = Response $ InpObj vt Nothing readB refH
          processResponse rsp
