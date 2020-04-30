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
import qualified Control.Exception as E
import Data.IORef
import Network.HTTP2.Internal
import Network.HTTP2.Server hiding (run)
import Network.HTTP2.Server.Internal
import Network.QUIC
import qualified System.TimeManager as T

import Imports
import Network.HTTP3.Context
import Network.HTTP3.Control
import Network.HTTP3.Frame
import Network.HTTP3.Recv
import Network.HTTP3.Send

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
      | isClientInitiatedBidirectional  sid = void $ forkIO $ processRequest ctx server strm
      | isServerInitiatedUnidirectional sid = return () -- error
      | otherwise                           = return ()
      where
        sid = streamId strm

processRequest :: Context -> Server -> Stream -> IO ()
processRequest ctx server strm = do
    th <- registerThread ctx
    src <- newSource strm
    mvt <- recvHeader ctx src
    case mvt of
      Nothing -> return ()
      Just vt -> do
          -- fixme Content-Length
          refI <- newIORef IInit
          refH <- newIORef Nothing
          let readB = recvBody ctx src refI refH
              req = Request $ InpObj vt Nothing readB refH
          let aux = Aux th
          server req aux $ sendResponse ctx strm th

sendResponse :: Context -> Stream -> T.Handle -> Response -> [PushPromise] -> IO ()
sendResponse ctx strm th (Response outobj) _pp = do
    sendHeader ctx strm th $ outObjHeaders outobj
    sendBody   ctx strm th outobj
    shutdownStream strm
