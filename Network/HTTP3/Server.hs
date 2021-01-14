{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP3.Server (
  -- * Runner
    run
  -- * Runner arguments
  , Config(..)
  , allocSimpleConfig
  , freeSimpleConfig
  , Hooks(..)
  , defaultHooks
  -- * HTTP\/3 server
  , Server
  -- * Request
  , Request
  -- ** Accessing request
  , H2.requestMethod
  , H2.requestPath
  , H2.requestAuthority
  , H2.requestScheme
  , H2.requestHeaders
  , H2.requestBodySize
  , H2.getRequestBodyChunk
  , H2.getRequestTrailers
  -- * Aux
  , Aux
  , auxTimeHandle
  -- * Response
  , Response
  -- ** Creating response
  , H2.responseNoBody
  , H2.responseFile
  , H2.responseStreaming
  , H2.responseBuilder
  -- ** Accessing response
  , H2.responseBodySize
  -- ** Trailers maker
  , H2.TrailersMaker
  , H2.NextTrailersMaker(..)
  , H2.defaultTrailersMaker
  , H2.setResponseTrailersMaker
  -- * Push promise
  , PushPromise
  , H2.pushPromise
  , H2.promiseRequestPath
  , H2.promiseResponse
  , H2.promiseWeight
  -- * Types
  , H2.Path
  , H2.Authority
  , H2.Scheme
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
import Network.HTTP2.Internal (InpObj(..))
import qualified Network.HTTP2.Internal as H2
import Network.HTTP2.Server (Server, PushPromise)
import qualified Network.HTTP2.Server as H2
import Network.HTTP2.Server.Internal (Request(..), Response(..), Aux(..))
import Network.QUIC (Connection, Stream)
import qualified Network.QUIC as QUIC
import qualified System.TimeManager as T

import Imports
import Network.HTTP3.Config
import Network.HTTP3.Context
import Network.HTTP3.Control
import Network.HTTP3.Frame
import Network.HTTP3.Recv
import Network.HTTP3.Send

-- | Running an HTTP\/3 server.
run :: Connection -> Config -> Server -> IO ()
run conn conf server = E.bracket open close $ \ctx -> do
    tid <- forkIO $ setupUnidirectional conn conf
    addThreadId ctx tid
    readerServer ctx server
  where
    open = do
        ref <- newIORef IInit
        newContext conn conf (controlStream conn ref)
    close = clearContext

readerServer :: Context -> Server -> IO ()
readerServer ctx server = loop
  where
    loop = do
        accept ctx >>= process
        loop
    process strm
      | QUIC.isClientInitiatedUnidirectional sid = do
            tid <- unidirectional ctx strm
            addThreadId ctx tid
      | QUIC.isClientInitiatedBidirectional  sid = void $ forkFinally (processRequest ctx server strm) (\_ -> closeStream strm)
      | QUIC.isServerInitiatedUnidirectional sid = return () -- error
      | otherwise                                = return ()
      where
        sid = QUIC.streamId strm

processRequest :: Context -> Server -> Stream -> IO ()
processRequest ctx server strm = do
    th <- registerThread ctx
    src <- newSource strm
    mvt <- recvHeader ctx src
    case mvt of
      Nothing -> return ()
      Just vt -> do
          -- fixme: Content-Length
          refI <- newIORef IInit
          refH <- newIORef Nothing
          let readB = recvBody ctx src refI refH
              req = Request $ InpObj vt Nothing readB refH
          let aux = Aux th
          server req aux $ sendResponse ctx strm th

sendResponse :: Context -> Stream -> T.Handle -> Response -> [PushPromise] -> IO ()
sendResponse ctx strm th (Response outobj) _pp = do
    sendHeader ctx strm th $ H2.outObjHeaders outobj
    sendBody   ctx strm th outobj
    QUIC.shutdownStream strm
