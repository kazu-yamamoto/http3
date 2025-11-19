{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | A client library for HTTP/3.
module Network.HTTP3.Client (
    -- * Runner
    run,

    -- * Client configration
    ClientConfig,
    defaultClientConfig,
    scheme,
    authority,

    -- * Common configuration
    Config (..),
    defaultConfig,
    allocSimpleConfig,
    freeSimpleConfig,
    defaultQEncoderConfig,
    ecMaxTableCapacity,
    ecHeaderBlockBufferSize,
    ecInstructionBufferSize,
    defaultQDecoderConfig,
    dcMaxTableCapacity,
    dcHuffmanBufferSize,
    Hooks (..),
    defaultHooks,

    -- * HTTP semantics
    module Network.HTTP.Semantics.Client,
) where

import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.ByteString.UTF8 as UTF8
import Data.IORef
import Data.IP (IPv6)
import Network.HTTP.Semantics.Client
import Network.HTTP.Semantics.Client.Internal
import Network.QUIC (Connection)
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Internal as QUIC
import Text.Read (readMaybe)

import Imports
import Network.HTTP3.Config
import Network.HTTP3.Context
import Network.HTTP3.Error
import Network.HTTP3.Frame
import Network.HTTP3.Recv
import Network.HTTP3.Send
import Network.QPACK

-- | Configuration for HTTP\/3 or HQ client. For HQ, 'authority' is
--   not used and an server's IP address is used in 'Request'.
data ClientConfig = ClientConfig
    { scheme :: Scheme
    , authority :: Authority
    }

defaultClientConfig :: ClientConfig
defaultClientConfig =
    ClientConfig
        { scheme = "https"
        , authority = "localhost"
        }

-- | Running an HTTP\/3 client.
run :: Connection -> ClientConfig -> Config -> Client a -> IO a
run conn ClientConfig{..} conf client = withContext conn conf $ \ctx -> do
    forkManaged ctx "H3 client: readerClient" $ readerClient ctx
    client (sendRequest ctx scheme authority) aux
  where
    aux =
        defaultAux
            { auxPossibleClientStreams = QUIC.possibleMyStreams conn
            , auxSendPing = QUIC.sendFrames conn QUIC.RTT1Level [QUIC.Ping]
            }

readerClient :: Context -> IO ()
readerClient ctx = loop
  where
    loop = do
        accept ctx >>= process
        loop
    process strm
        | QUIC.isClientInitiatedUnidirectional sid = return () -- error
        | QUIC.isClientInitiatedBidirectional sid = return ()
        | QUIC.isServerInitiatedUnidirectional sid =
            forkManaged ctx "H3 client: unidirectional handler" $
                unidirectional ctx strm
        | otherwise = return () -- push?
      where
        sid = QUIC.streamId strm

sendRequest
    :: Context -> Scheme -> Authority -> Request -> (Response -> IO a) -> IO a
sendRequest ctx scm auth (Request outobj) processResponse =
    E.bracket (newStream ctx) closeStream $ \strm -> do
        forkManagedTimeout ctx "H3 client: sendRequest" $ \th -> do
            sendHeader ctx strm th hdr'
            sendBody ctx strm th outobj
            QUIC.shutdownStream strm
        src <- newSource strm
        let sid = QUIC.streamId strm
        mvt <- recvHeader ctx sid src
        case mvt of
            Nothing -> do
                QUIC.resetStream strm H3MessageError
                threadDelay 100000
                -- just for type inference
                E.throwIO $ QUIC.ApplicationProtocolErrorIsSent H3MessageError ""
            Just vt -> do
                refI <- newIORef IInit
                refH <- newIORef Nothing
                let readB = recvBody ctx sid src refI refH
                    rsp = Response $ InpObj vt Nothing readB refH
                processResponse rsp
  where
    hdr = outObjHeaders outobj
    isIPv6 = isJust (readMaybe auth :: Maybe IPv6)
    auth'
        | isIPv6 = "[" <> UTF8.fromString auth <> "]"
        | otherwise = UTF8.fromString auth
    hdr' = addIfNotExist (":scheme", scm) $ addIfNotExist (":authority", auth') hdr
    addIfNotExist keyVal@(key, _) hs = case find (\(k, _) -> k == key) hs of
        Nothing -> keyVal : hs
        Just _ -> hs
