{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A server library for HTTP/0.9.
module Network.HQ.Server (
    -- * Runner
    run,

    -- * Runner arguments
    Config (..),
    allocSimpleConfig,
    freeSimpleConfig,

    -- * HQ server
    Server,

    -- * Request
    Request,

    -- ** Accessing request
    H2.requestPath,

    -- * Response
    Response,

    -- ** Creating response
    H2.responseNoBody,
    H2.responseFile,
    H2.responseStreaming,
    H2.responseBuilder,
) where

import Control.Concurrent
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Internal as BS
import Data.IORef
import Foreign.ForeignPtr
import Network.HPACK (HeaderTable, toHeaderTable)
import Network.HTTP2.Internal (InpObj (..))
import qualified Network.HTTP2.Internal as H2
import Network.HTTP2.Server (PushPromise, Server)
import qualified Network.HTTP2.Server as H2
import Network.HTTP2.Server.Internal (Aux (..), Request (..), Response (..))
import Network.QUIC (Connection, Stream)
import qualified Network.QUIC as QUIC
import Network.SockAddr (showSockAddrBS)
import Network.Socket (SockAddr)
import qualified System.TimeManager as T

import Imports
import Network.HTTP3.Config
import Network.HTTP3.Recv (newSource, readSource)

-- | Running an HQ server.
run :: Connection -> Config -> Server -> IO ()
run conn conf server = do
    info <- QUIC.getConnectionInfo conn
    let mysa = QUIC.localSockAddr info
        peersa = QUIC.remoteSockAddr info
    forever $ do
        strm <- QUIC.acceptStream conn
        forkFinally
            (processRequest conf mysa peersa server strm)
            (\_ -> QUIC.closeStream strm)

processRequest :: Config -> SockAddr -> SockAddr -> Server -> Stream -> IO ()
processRequest conf mysa peersa server strm
    | QUIC.isClientInitiatedBidirectional sid = do
        th <- T.register (confTimeoutManager conf) (return ())
        vt <- recvHeader strm mysa
        src <- newSource strm
        refH <- newIORef Nothing
        let readB = readSource src
            req = Request $ InpObj vt Nothing readB refH
            aux = Aux th mysa peersa
        server req aux $ sendResponse conf strm
    | otherwise = return () -- fixme: should consume the data?
  where
    sid = QUIC.streamId strm

recvHeader :: Stream -> SockAddr -> IO HeaderTable
recvHeader strm myaddr = do
    (method, path) <- recvRequestLine id
    let auth = showSockAddrBS myaddr
        vt =
            (":path", path)
                : (":method", method)
                : (":scheme", "https")
                : (":authority", auth)
                : []
    toHeaderTable vt
  where
    recvRequestLine builder = do
        bs <- QUIC.recvStream strm 1024
        if bs == ""
            then do
                return $ parseRequestLine $ BS.concat $ builder []
            else recvRequestLine (builder . (bs :))
    parseRequestLine bs = (method, path)
      where
        method = "GET"
        path0 = BS.drop 4 bs
        path = BS.take (BS.length path0 - 2) path0

sendResponse :: Config -> Stream -> Response -> [PushPromise] -> IO ()
sendResponse conf strm (Response outobj) _ = case H2.outObjBody outobj of
    H2.OutBodyNone -> return ()
    H2.OutBodyFile (H2.FileSpec path fileoff bytecount) -> do
        (pread, sentinel') <- confPositionReadMaker conf path
        let timmgr = confTimeoutManager conf
        refresh <- case sentinel' of
            H2.Closer closer -> do
                th <- T.register timmgr closer
                return $ T.tickle th
            H2.Refresher refresher -> return refresher
        let next = H2.fillFileBodyGetNext pread fileoff bytecount refresh
        sendNext strm next
    H2.OutBodyBuilder builder -> do
        let next = H2.fillBuilderBodyGetNext builder
        sendNext strm next
    H2.OutBodyStreaming strmbdy -> sendStreaming strm strmbdy
    H2.OutBodyStreamingUnmask _ ->
        error "sendResponse: server does not support OutBodyStreamingUnmask"

sendNext :: Stream -> H2.DynaNext -> IO ()
sendNext strm action = do
    fp <- BS.mallocByteString 2048
    H2.Next len _reqflush mnext <- withForeignPtr fp $ \buf -> action buf 2048 65536 -- window size
    if len == 0
        then return ()
        else do
            let bs = PS fp 0 len
            QUIC.sendStream strm bs
            case mnext of
                Nothing -> return ()
                Just next -> sendNext strm next

sendStreaming :: Stream -> ((Builder -> IO ()) -> IO () -> IO ()) -> IO ()
sendStreaming strm strmbdy = do
    strmbdy write flush
  where
    flush = return ()
    write builder = do
        newByteStringAndSend strm (B.runBuilder builder) >>= loop
      where
        loop B.Done = return ()
        loop (B.More _ writer) =
            newByteStringAndSend strm writer >>= loop
        loop (B.Chunk bs writer) = do
            QUIC.sendStream strm bs
            newByteStringAndSend strm writer >>= loop

newByteStringAndSend :: Stream -> B.BufferWriter -> IO B.Next
newByteStringAndSend strm action = do
    fp <- BS.mallocByteString 2048
    (len, signal) <- withForeignPtr fp $ \buf -> action buf 2048
    if len == 0
        then return signal
        else do
            let bs = PS fp 0 len
            QUIC.sendStream strm bs
            return signal
