{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HQ.Client (
  -- * Runner
    run
  -- * Runner arguments
  , H3.ClientConfig(..)
  , H3.Config(..)
  , H3.allocSimpleConfig
  , H3.freeSimpleConfig
  , H3.Scheme
  , H3.Authority
  -- * HQ client
  , H2.Client
  -- * Request
  , Request
  -- * Creating request
  , H2.requestNoBody
  -- * Response
  , Response
  -- ** Accessing response
  , H2.getResponseBodyChunk
  ) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IORef
import Network.HPACK
import qualified Network.HTTP2.Client as H2
import Network.HTTP2.Client.Internal (Request(..), Response(..))
import Network.HTTP2.Internal (InpObj(..))
import qualified Network.HTTP2.Internal as H2
import Network.QUIC (Connection)
import qualified Network.QUIC as QUIC

import qualified Network.HTTP3.Client as H3
import Network.HTTP3.Recv (newSource, readSource)

-- | Running an HQ client.
run :: Connection -> H3.ClientConfig -> H3.Config -> H2.Client a -> IO a
run conn _ _ client = E.bracket open close $ \strm -> do
    client $ sendRequest strm
  where
    open = QUIC.stream conn
    close = QUIC.closeStream

sendRequest :: QUIC.Stream -> Request -> (Response -> IO a) -> IO a
sendRequest strm (Request outobj) processResponse = do
    let hdr = H2.outObjHeaders outobj
        Just path = lookup ":path" hdr
        requestLine = BS.concat ["GET ", path, "\r\n"]
    QUIC.sendStream strm requestLine
    QUIC.shutdownStream strm
    src <- newSource strm
    refH <- newIORef Nothing
    vt <- toHeaderTable []
    let readB = readSource src
        rsp = Response $ InpObj vt Nothing readB refH
    processResponse rsp
