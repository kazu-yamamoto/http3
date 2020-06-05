{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HTTP3.Client (
  -- * Runner
    run
  -- * Runner arguments
  , ClientConfig(..)
  , Config(..)
  , defaultConfig
  , Scheme
  , Authority
  -- * HTTP\/3 client
  , Client
  -- * Request
  , Request
  -- * Creating request
  , H2.requestNoBody
  , H2.requestFile
  , H2.requestStreaming
  , H2.requestBuilder
  -- ** Trailers maker
  , H2.TrailersMaker
  , H2.NextTrailersMaker(..)
  , H2.defaultTrailersMaker
  , H2.setRequestTrailersMaker
  -- * Response
  , Response
  -- ** Accessing response
  , H2.responseStatus
  , H2.responseHeaders
  , H2.responseBodySize
  , H2.getResponseBodyChunk
  , H2.getResponseTrailers
  -- * Types
  , H2.Method
  , H2.Path
  , H2.FileSpec(..)
  , H2.FileOffset
  , H2.ByteCount
  -- * RecvN
  , H2.defaultReadN
  -- * Position read for files
  , H2.PositionReadMaker
  , H2.PositionRead
  , H2.Sentinel(..)
  , H2.defaultPositionReadMaker
  ) where

import Control.Concurrent
import qualified Control.Exception as E
import Data.IORef
import Network.HTTP2.Client (Scheme, Authority, Client)
import qualified Network.HTTP2.Client as H2
import Network.HTTP2.Client.Internal (Request(..), Response(..))
import Network.HTTP2.Internal (InpObj(..))
import qualified Network.HTTP2.Internal as H2
import Network.QUIC (Connection)
import qualified Network.QUIC as QUIC

import Network.HTTP3.Config
import Network.HTTP3.Context
import Network.HTTP3.Control
import Network.HTTP3.Frame
import Network.HTTP3.Recv
import Network.HTTP3.Send

data ClientConfig = ClientConfig {
    scheme :: Scheme
  , authority :: Authority
  }

run :: Connection -> ClientConfig -> Config -> H2.Client a -> IO a
run conn ClientConfig{..} conf client = E.bracket open close $ \ctx -> do
    setupUnidirectional conn
    tid <- forkIO $ readerClient ctx
    client (sendRequest ctx scheme authority) `E.finally` do
        killThread tid
  where
    open = do
        ref <- newIORef IInit
        newContext conn conf (controlStream ref)
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
      | QUIC.isClientInitiatedUnidirectional sid = return () -- error
      | QUIC.isClientInitiatedBidirectional  sid = return ()
      | QUIC.isServerInitiatedUnidirectional sid = unidirectional ctx strm
      | otherwise                                = return () -- push?
      where
        sid = QUIC.streamId strm

sendRequest :: Context -> Scheme -> Authority -> Request -> (Response -> IO a) -> IO a
sendRequest ctx scm auth (Request outobj) processResponse = do
    th <- registerThread ctx
    let hdr = H2.outObjHeaders outobj
        hdr' = (":scheme", scm)
             : (":authority", auth)
             : hdr
    strm <- newStream ctx
    sendHeader ctx strm th hdr'
    sendBody ctx strm th outobj
    QUIC.shutdownStream strm
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
