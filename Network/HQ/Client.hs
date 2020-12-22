{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HQ.Client (
    run
  , H3.ClientConfig(..)
  , H3.Config(..)
  , H2.Client
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
