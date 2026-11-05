{-# LANGUAGE OverloadedStrings #-}

-- | A server library for HTTP/0.9.
module Network.HQ.Server (
    -- * Runner
    run,

    -- * Common configration
    Config (..),
    allocSimpleConfig,
    freeSimpleConfig,

    -- * HQ server
    Server,

    -- * Request
    Request,

    -- ** Accessing request
    requestPath,

    -- * Response
    Response,

    -- ** Creating response
    responseNoBody,
    responseFile,
    responseStreaming,
    responseBuilder,
) where

import Control.Concurrent
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Internal as BS
import Data.IORef
import Foreign.ForeignPtr
import GHC.Conc.Sync
import Network.HPACK.Internal (toTokenHeaderTable)
import Network.HTTP.Semantics.IO
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal

import Network.QUIC (Connection, Stream)
import qualified Network.QUIC as QUIC
import Network.SockAddr (showSockAddrBS)
import Network.Socket (SockAddr)
import qualified System.TimeManager as T

import Imports
import Network.HTTP3.Config
import Network.HTTP3.Error (ApplicationProtocolError (H3InternalError))
import Network.HTTP3.Recv (newSource, readSource')

-- | Running an HQ server.
run :: Connection -> Config -> Server -> IO ()
run conn conf server = do
    info <- QUIC.getConnectionInfo conn
    let mysa = QUIC.localSockAddr info
        peersa = QUIC.remoteSockAddr info
    forever $ do
        strm <- QUIC.acceptStream conn
        forkFinally
            (labelMe "HQ processRequest" >> processRequest conf mysa peersa server strm)
            (finish strm)
  where
    -- Only a response sent in full ends with a FIN.  One whose handler
    -- failed or timed out used to end so too, and the client took what it
    -- had for the whole response.
    finish strm (Right True) = QUIC.closeStream strm
    finish strm _ = do
        QUIC.resetStream strm H3InternalError
        QUIC.closeStream strm

labelMe :: String -> IO ()
labelMe label = do
    tid <- myThreadId
    labelThread tid label

-- | Whether the response went out in full.
--
-- The handler is killed when the timeout manager says so; it used to be
-- given a timeout that did nothing, so a client that never finished its
-- request line kept the thread for as long as the connection lasted.
processRequest :: Config -> SockAddr -> SockAddr -> Server -> Stream -> IO Bool
processRequest conf mysa peersa server strm
    | QUIC.isClientInitiatedBidirectional sid = do
        done <- newIORef False
        T.withHandleKillThread (confTimeoutManager conf) (return ()) $ \th -> do
            mvt <- recvHeader strm mysa
            forM_ mvt $ \vt -> do
                src <- newSource strm
                refH <- newIORef Nothing
                let readB = readSource' src
                    req = Request $ InpObj vt Nothing readB refH
                    aux =
                        defaultAux
                            { auxTimeHandle = th
                            , auxMySockAddr = mysa
                            , auxPeerSockAddr = peersa
                            }
                server req aux $ sendResponse conf strm
                writeIORef done True
        readIORef done
    | otherwise = return True -- fixme: should consume the data?
  where
    sid = QUIC.streamId strm

-- | The request line, as a header table, or 'Nothing' when it is not one.
recvHeader :: Stream -> SockAddr -> IO (Maybe TokenHeaderTable)
recvHeader strm myaddr = do
    mpath <- recvRequestLine strm
    forM mpath $ \path -> do
        let auth = showSockAddrBS myaddr
            vt =
                (":path", path)
                    : (":method", "GET")
                    : (":scheme", "https")
                    : (":authority", auth)
                    : []
        toTokenHeaderTable vt

-- | The path of an HTTP/0.9 request line, "GET" and the path, ending in CRLF
-- or LF, or at the end of the stream.
--
-- It was taken to be the octets after the first four, less the last two,
-- of everything up to the end of the stream: no check that the method was
-- GET, a version taken as part of the path, the last octet of the path lost
-- to a bare LF, and no limit on what was held.
recvRequestLine :: Stream -> IO (Maybe ByteString)
recvRequestLine strm = loop ""
  where
    maxLength = 8192
    loop acc = do
        bs <- QUIC.recvStream strm 1024
        let acc' = acc <> bs
        case BS.elemIndex 0x0a acc' of
            Just i -> return $ parse $ BS.take i acc'
            Nothing
                | BS.null bs -> return $ parse acc'
                | BS.length acc' > maxLength -> return Nothing
                | otherwise -> loop acc'
    parse line0 = case BS.stripPrefix "GET " line of
        Just rest
            | path <- BS.takeWhile (/= 0x20) rest
            , "/" `BS.isPrefixOf` path ->
                Just path
        _ -> Nothing
      where
        line = fromMaybe line0 $ BS.stripSuffix "\r" line0

sendResponse :: Config -> Stream -> Response -> [PushPromise] -> IO ()
sendResponse conf strm (Response outobj) _ = case outObjBody outobj of
    OutBodyNone -> return ()
    OutBodyFile (FileSpec path fileoff bytecount) -> do
        (pread, sentinel) <- confPositionReadMaker conf path
        let next = fillFileBodyGetNext pread fileoff bytecount sentinel
        sendNext strm next
    OutBodyBuilder builder -> do
        let next = fillBuilderBodyGetNext builder
        sendNext strm next
    OutBodyStreaming strmbdy -> sendStreaming strm strmbdy
    OutBodyStreamingIface _ ->
        error "sendResponse: server does not support OutBodyStreamingUnmask"

sendNext :: Stream -> DynaNext -> IO ()
sendNext strm action = do
    fp <- BS.mallocByteString 2048
    Next len _reqflush mnext <- withForeignPtr fp $ \buf -> action buf 2048
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
