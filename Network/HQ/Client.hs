{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | A client library for HTTP/0.9.
module Network.HQ.Client (
    -- * Runner
    run,

    -- * Runner arguments
    H3.ClientConfig (..),
    H3.Config (..),
    H3.allocSimpleConfig,
    H3.freeSimpleConfig,
    Scheme,
    Authority,

    -- * HQ client
    Client,

    -- * Request
    Request,

    -- * Creating request
    requestNoBody,

    -- * Response
    Response,

    -- ** Accessing response
    getResponseBodyChunk,
) where

import qualified Data.ByteString as BS
import Data.IORef
import Network.HPACK
import Network.HTTP.Semantics
import Network.HTTP.Semantics.Client
import Network.HTTP.Semantics.Client.Internal
import Network.QUIC (Connection)
import qualified Network.QUIC as QUIC
import Network.QUIC.Internal (possibleMyStreams)
import qualified UnliftIO.Exception as E

import Imports
import qualified Network.HTTP3.Client as H3
import Network.HTTP3.Recv (newSource, readSource')

-- | Running an HQ client.
run :: Connection -> H3.ClientConfig -> H3.Config -> Client a -> IO a
run conn _ _ client = client (sendRequest conn) aux
  where
    aux =
        Aux
            { auxPossibleClientStreams = possibleMyStreams conn
            }

sendRequest :: Connection -> Request -> (Response -> IO a) -> IO a
sendRequest conn (Request outobj) processResponse = E.bracket open close $ \strm -> do
    let hdr = outObjHeaders outobj
        path = fromJust $ lookup ":path" hdr
        requestLine = BS.concat ["GET ", path, "\r\n"]
    QUIC.sendStream strm requestLine
    QUIC.shutdownStream strm
    src <- newSource strm
    refH <- newIORef Nothing
    vt <- toTokenHeaderTable []
    let readB = readSource' src
        rsp = Response $ InpObj vt Nothing readB refH
    processResponse rsp
  where
    open = QUIC.stream conn
    close = QUIC.closeStream
