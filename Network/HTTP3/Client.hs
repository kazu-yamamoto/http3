{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | A client library for HTTP/3.
module Network.HTTP3.Client (
    -- * Runner
    run,

    -- * Runner arguments
    ClientConfig (..),
    Config (..),
    allocSimpleConfig,
    freeSimpleConfig,
    Hooks (..),
    defaultHooks,
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
import Network.QUIC.Internal (possibleMyStreams)
import Text.Read (readMaybe)

import Imports
import Network.HTTP3.Config
import Network.HTTP3.Context
import Network.HTTP3.Control
import Network.HTTP3.Error
import Network.HTTP3.Frame
import Network.HTTP3.Recv
import Network.HTTP3.Send

-- | Configuration for HTTP\/3 or HQ client. For HQ, 'authority' is
--   not used and an server's IP address is used in 'Request'.
data ClientConfig = ClientConfig
    { scheme :: Scheme
    , authority :: Authority
    }

-- | Running an HTTP\/3 client.
run :: Connection -> ClientConfig -> Config -> Client a -> IO a
run conn ClientConfig{..} conf client = withContext conn conf $ \ctx -> do
    forkManaged ctx "H3 client: unidirectional setter" $
        setupUnidirectional conn conf
    forkManaged ctx "H3 client: readerClient" $ readerClient ctx
    client (sendRequest ctx scheme authority) aux
  where
    aux =
        Aux
            { auxPossibleClientStreams = possibleMyStreams conn
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
        mvt <- recvHeader ctx src
        case mvt of
            Nothing -> do
                QUIC.resetStream strm H3MessageError
                threadDelay 100000
                -- just for type inference
                E.throwIO $ QUIC.ApplicationProtocolErrorIsSent H3MessageError ""
            Just vt -> do
                refI <- newIORef IInit
                refH <- newIORef Nothing
                let readB = recvBody ctx src refI refH
                    rsp = Response $ InpObj vt Nothing readB refH
                processResponse rsp
  where
    hdr = outObjHeaders outobj
    isIPv6 = isJust (readMaybe auth :: Maybe IPv6)
    auth'
        | isIPv6 = "[" <> UTF8.fromString auth <> "]"
        | otherwise = UTF8.fromString auth
    hdr' = (":scheme", scm) : (":authority", auth') : hdr
