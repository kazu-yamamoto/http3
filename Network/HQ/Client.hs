{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | A client library for HTTP/0.9.
module Network.HQ.Client (
    -- * Runner
    run,

    -- * Client configuration
    H3.ClientConfig,
    H3.defaultClientConfig,
    H3.scheme,
    Scheme,

    -- * Common configuration
    H3.Config,
    H3.confHooks,
    H3.confQEncoderConfig,
    H3.confQDecoderConfig,
    H3.confPositionReadMaker,
    H3.confTimeoutManager,
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

    -- * Exception
    UnsupportedRequest (..),
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

import qualified Network.HTTP3.Client as H3
import Network.HTTP3.Error (UnsupportedRequest (..))
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
sendRequest conn (Request outobj) processResponse = do
    -- HTTP/0.9 has GET and a path, and nothing else.  A request without a
    -- path failed in 'fromJust', and one of another method was sent as GET.
    path <- case (lookup ":method" hdr, lookup ":path" hdr) of
        (Just "GET", Just p) -> return p
        _ -> E.throwIO $ UnsupportedRequest "only GET with a path can be sent"
    E.bracket open close $ \strm -> sendRequest' strm path
  where
    hdr = outObjHeaders outobj
    open = QUIC.stream conn
    close = QUIC.closeStream
    sendRequest' strm path = do
        let requestLine = BS.concat ["GET ", path, "\r\n"]
        QUIC.sendStream strm requestLine
        QUIC.shutdownStream strm
        src <- newSource strm
        refH <- newIORef Nothing
        vt <- toTokenHeaderTable []
        let readB = readSource' src
            rsp = Response $ InpObj vt Nothing readB refH
        processResponse rsp
