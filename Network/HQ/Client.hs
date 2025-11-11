{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | A client library for HTTP/0.9.
module Network.HQ.Client (
    -- * Runner
    run,

    -- * Client configration
    H3.ClientConfig,
    H3.defaultClientConfig,
    H3.scheme,
    Scheme,

    -- * Common configration
    H3.Config (..),
    H3.allocSimpleConfig,
    H3.freeSimpleConfig,

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

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IORef
import Network.HPACK
import Network.HTTP.Semantics
import Network.HTTP.Semantics.Client
import Network.HTTP.Semantics.Client.Internal
import Network.QUIC (Connection)
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Internal as QUIC

import Imports
import qualified Network.HTTP3.Client as H3
import Network.HTTP3.Recv (newSource, readSource')

-- | Running an HQ client.
run :: Connection -> H3.ClientConfig -> H3.Config -> Client a -> IO a
run conn _ _ client = client (sendRequest conn) aux
  where
    aux =
        defaultAux
            { auxPossibleClientStreams = QUIC.possibleMyStreams conn
            , auxSendPing = QUIC.sendFrames conn QUIC.RTT1Level [QUIC.Ping]
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
