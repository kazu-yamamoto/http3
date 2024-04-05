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
) where

import Control.Concurrent
import Data.IORef
import Network.HTTP.Semantics
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal
import Network.QUIC (Connection, Stream)
import qualified Network.QUIC as QUIC
import qualified System.TimeManager as T
import qualified UnliftIO.Exception as E

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
run conn conf server = E.bracket open close $ \ctx -> do
    tid <- forkIO $ setupUnidirectional conn conf
    addThreadId ctx tid
    readerServer ctx server
  where
    open = do
        ref <- newIORef IInit
        newContext conn conf (controlStream conn ref)
    close = clearContext

readerServer :: Context -> Server -> IO ()
readerServer ctx server = loop
  where
    loop = do
        accept ctx >>= process
        loop
    process strm
        | QUIC.isClientInitiatedUnidirectional sid = do
            tid <- forkIO $ unidirectional ctx strm
            addThreadId ctx tid
        | QUIC.isClientInitiatedBidirectional sid =
            void $ forkFinally (processRequest ctx server strm) (\_ -> closeStream strm)
        | QUIC.isServerInitiatedUnidirectional sid = return () -- error
        | otherwise = return ()
      where
        sid = QUIC.streamId strm

processRequest :: Context -> Server -> Stream -> IO ()
processRequest ctx server strm = E.handleAny reset $ do
    th <- registerThread ctx
    src <- newSource strm
    mvt <- recvHeader ctx src
    case mvt of
        Nothing -> QUIC.resetStream strm H3MessageError
        Just ht@(_, vt) -> do
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
            let aux = Aux th (getMySockAddr ctx) (getPeerSockAddr ctx)
            server req aux $ sendResponse ctx strm th
  where
    reset se
        | Just (_ :: DecodeError) <- E.fromException se =
            abort ctx QpackDecompressionFailed
        | otherwise = QUIC.resetStream strm H3MessageError

sendResponse
    :: Context -> Stream -> T.Handle -> Response -> [PushPromise] -> IO ()
sendResponse ctx strm th (Response outobj) _pp = do
    sendHeader ctx strm th $ outObjHeaders outobj
    sendBody ctx strm th outobj
    QUIC.shutdownStream strm
