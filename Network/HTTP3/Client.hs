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
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Network.HTTP.Semantics.Client
import Network.HTTP.Semantics.Client.Internal
import Network.QUIC (Connection)
import qualified Network.QUIC as QUIC
import Network.QUIC.Internal (possibleMyStreams)

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
run conn ClientConfig{..} conf client = E.bracket open close $ \ctx -> do
    tid0 <- forkIO $ setupUnidirectional conn conf
    addThreadId ctx tid0
    tid1 <- forkIO $ readerClient ctx
    addThreadId ctx tid1
    client (sendRequest ctx scheme authority) aux
  where
    open = do
        ref <- newIORef IInit
        newContext conn conf (controlStream conn ref)
    close = clearContext
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
        | QUIC.isServerInitiatedUnidirectional sid = do
            tid <- forkIO $ unidirectional ctx strm
            addThreadId ctx tid
        | otherwise = return () -- push?
      where
        sid = QUIC.streamId strm

sendRequest
    :: Context -> Scheme -> Authority -> Request -> (Response -> IO a) -> IO a
sendRequest ctx scm auth (Request outobj) processResponse = do
    th <- registerThread ctx
    let hdr = outObjHeaders outobj
        hdr' =
            (":scheme", scm)
                : (":authority", C8.pack auth)
                : hdr
    E.bracket (newStream ctx) closeStream $ \strm -> do
        sendHeader ctx strm th hdr'
        tid <- forkIO $ do
            sendBody ctx strm th outobj
            QUIC.shutdownStream strm
        addThreadId ctx tid
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
