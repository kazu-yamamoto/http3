{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A server library for HTTP/3.
module Network.HTTP3.Server (
    -- * Runner
    run,

    -- * Runner arguments
    Config (..),
    allocSimpleConfig,
    freeSimpleConfig,
    Hooks (..),
    defaultHooks,
    module Network.HTTP.Semantics.Server,

    -- * Internal
    runIO,
    ServerIO (..),
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.IORef
import GHC.Conc.Sync
import Network.HTTP.Semantics
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal
import Network.HTTP2.Server.Internal (ServerIO (..))
import Network.QUIC (Connection, ConnectionInfo (..), Stream, getConnectionInfo)
import qualified Network.QUIC as QUIC
import qualified System.TimeManager as T

import Imports
import Network.HTTP3.Config
import Network.HTTP3.Context
import Network.HTTP3.Control
import Network.HTTP3.Error
import Network.HTTP3.Frame
import Network.HTTP3.Recv
import Network.HTTP3.Send
import Network.QPACK.Internal

-- | Running an HTTP\/3 server.
run :: Connection -> Config -> Server -> IO ()
run conn conf server = withContext conn conf $ \ctx -> do
    myThreadId >>= \t -> labelThread t "H3 server: run"
    forkManaged ctx "H3 server: unidirectional setter" $
        setupUnidirectional conn conf
    readerServer ctx $ \strm ->
        forkManagedTimeoutFinally
            ctx
            "H3 server: processRequest"
            (processRequest ctx server strm)
            (closeStream strm)

runIO :: Connection -> Config -> (ServerIO Stream -> IO (IO ())) -> IO ()
runIO conn conf action = withContext conn conf $ \ctx -> do
    forkManaged ctx "H3 server: unidirectional setter" $
        setupUnidirectional conn conf
    info <- getConnectionInfo conn
    reqq <- newTQueueIO
    let sio =
            ServerIO
                { sioMySockAddr = localSockAddr info
                , sioPeerSockAddr = remoteSockAddr info
                , sioReadRequest = atomically $ readTQueue reqq
                , sioWriteResponse = sendResponseIO ctx
                }
        put strmreq = atomically $ writeTQueue reqq strmreq
    io <- action sio
    concurrently_ io (readerServer ctx $ processRequestIO ctx put)

readerServer :: Context -> (Stream -> IO ()) -> IO ()
readerServer ctx action = loop
  where
    loop = do
        accept ctx >>= process
        loop
    process strm
        | QUIC.isClientInitiatedUnidirectional sid =
            forkManaged ctx "H3 server: unidirectional handler" $
                unidirectional ctx strm
        | QUIC.isClientInitiatedBidirectional sid = action strm
        | QUIC.isServerInitiatedUnidirectional sid = return () -- error
        | otherwise = return ()
      where
        sid = QUIC.streamId strm

processRequest
    :: Context
    -> Server
    -> Stream
    -> T.Handle
    -> IO ()
processRequest ctx server strm th = E.handle reset $ do
    src <- newSource strm
    mvt <- recvHeader ctx src
    case mvt of
        Nothing -> QUIC.resetStream strm H3MessageError
        Just ht -> do
            req <- mkRequest ctx strm src ht
            let aux = Aux th (getMySockAddr ctx) (getPeerSockAddr ctx)
            server req aux $ sendResponse ctx strm th
  where
    reset se
        | isAsyncException se = E.throwIO se
        | Just (_ :: DecodeError) <- E.fromException se =
            abort ctx QpackDecompressionFailed
        | otherwise = QUIC.resetStream strm H3MessageError

processRequestIO :: Context -> ((Stream, Request) -> IO ()) -> Stream -> IO ()
processRequestIO ctx put strm = E.handle reset $ do
    src <- newSource strm
    mvt <- recvHeader ctx src
    case mvt of
        Nothing -> QUIC.resetStream strm H3MessageError
        Just ht -> do
            req <- mkRequest ctx strm src ht
            put (strm, req)
  where
    reset se
        | isAsyncException se = E.throwIO se
        | Just (_ :: DecodeError) <- E.fromException se =
            abort ctx QpackDecompressionFailed
        | otherwise = QUIC.resetStream strm H3MessageError

mkRequest
    :: Context
    -> Stream
    -> Source
    -> (TokenHeaderList, ValueTable)
    -> IO Request
mkRequest ctx strm src ht@(_, vt) = do
    let mMethod = getFieldValue tokenMethod vt
        mScheme = getFieldValue tokenScheme vt
        mAuthority = getFieldValue tokenAuthority vt
        mPath = getFieldValue tokenPath vt
    case (mMethod, mScheme, mAuthority, mPath) of
        (Just "CONNECT", _, Just _, _) -> return ()
        (Just _, Just _, Just _, Just _) -> return ()
        _ -> QUIC.resetStream strm H3MessageError
    -- fixme: Content-Length
    refI <- newIORef IInit
    refH <- newIORef Nothing
    let readB = recvBody ctx src refI refH
        req = Request $ InpObj ht Nothing readB refH
    return req

sendResponse
    :: Context -> Stream -> T.Handle -> Response -> [PushPromise] -> IO ()
sendResponse ctx strm th (Response outobj) _pp = do
    sendHeader ctx strm th $ outObjHeaders outobj
    sendBody ctx strm th outobj
    QUIC.shutdownStream strm

sendResponseIO
    :: Context -> Stream -> Response -> IO ()
sendResponseIO ctx strm (Response outobj) =
    void $ withHandle ctx $ \th -> do
        sendHeader ctx strm th $ outObjHeaders outobj
        sendBody ctx strm th outobj
        QUIC.closeStream strm
