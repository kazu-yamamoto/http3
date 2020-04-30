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

import qualified Control.Exception as E
import Data.IORef
import Network.HTTP2.Client hiding (run, Config, allocSimpleConfig, freeSimpleConfig)
import Network.QUIC

import Network.HTTP3.Context
import Network.HTTP3.Frame
import Network.HTTP3.Run

run :: Connection -> Client a -> IO ()
run conn _client = E.bracket open close $ \ctx -> do
    setupUnidirectional conn
    readerClient ctx
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
      | otherwise                           = return ()
      where
        sid = streamId strm
