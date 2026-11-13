{-# LANGUAGE OverloadedStrings #-}

-- | A server library for HTTP/0.9.
module Network.HQ.Server (
    -- * Runner
    run,

    -- * Common configuration
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

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Internal as BS
import Data.IORef
import Foreign.ForeignPtr
import Network.HPACK.Internal (toTokenHeaderTable)
import Network.HTTP.Semantics.IO
import Network.HTTP.Semantics.Server
import Network.HTTP.Semantics.Server.Internal

import Network.QUIC (Connection, Stream)
import qualified Network.QUIC as QUIC
import Network.SockAddr (showSockAddrBS)
import Network.Socket (SockAddr)
import qualified System.ThreadManager as TM
import qualified System.TimeManager as T
import System.Timeout (timeout)

import Imports
import Network.HTTP3.Config
import Network.HTTP3.Error (
    ApplicationProtocolError (H3InternalError, H3RequestCancelled),
 )
import Network.HTTP3.Recv (newSource, readSource')
import Network.HTTP3.Send (closeSentinel)

-- | Running an HQ server.
run :: Connection -> Config -> Server -> IO ()
run conn conf server = do
    info <- QUIC.getConnectionInfo conn
    let mysa = QUIC.localSockAddr info
        peersa = QUIC.remoteSockAddr info
    mgr <- TM.newThreadManager
    -- The handlers are stopped once the connection is gone, as with HTTP/3.
    -- They were plain threads, left running after it.
    TM.stopAfter mgr (accept mgr mysa peersa) $ \_ ->
        void $ timeout 5000000 $ TM.waitUntilAllGone mgr
  where
    accept mgr mysa peersa = forever $ do
        strm <- QUIC.acceptStream conn
        done <- newIORef False
        TM.forkManagedTimeoutFinally
            mgr
            (confTimeoutManager conf)
            "HQ processRequest"
            (processRequest conf mysa peersa server strm >=> writeIORef done)
            (finish strm done)
    -- Only a response sent in full ends with a FIN.  One whose handler
    -- failed, timed out or was stopped is reset.
    finish strm done = do
        ok <- readIORef done
        unless ok $ QUIC.resetStream strm H3InternalError
        QUIC.closeStream strm

-- | Whether the response went out in full.
processRequest
    :: Config -> SockAddr -> SockAddr -> Server -> Stream -> T.Handle -> IO Bool
processRequest conf mysa peersa server strm th
    | QUIC.isClientInitiatedBidirectional sid = do
        mvt <- recvHeader strm mysa
        case mvt of
            Nothing -> return False
            Just vt -> do
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
                return True
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
        sendNext strm next `E.onException` closeSentinel sentinel
    OutBodyBuilder builder -> do
        let next = fillBuilderBodyGetNext builder
        sendNext strm next
    OutBodyStreaming strmbdy ->
        sendStreaming strm $ \iface -> strmbdy (outBodyPush iface) (outBodyFlush iface)
    -- It used to be refused with 'error'.
    OutBodyStreamingIface strmbdy -> sendStreaming strm strmbdy

sendNext :: Stream -> DynaNext -> IO ()
sendNext strm action = do
    -- As HTTP/3 does: 2048 octets at a time was a 'sendStream' each.
    fp <- BS.mallocByteString 16384
    Next len _reqflush mnext <- withForeignPtr fp $ \buf -> action buf 16384
    -- Nothing this time is not the end: a builder fills nothing before a
    -- large chunk.  The body used to end here, and went out empty.
    when (len /= 0) $ QUIC.sendStream strm $ PS fp 0 len
    forM_ mnext $ sendNext strm

sendStreaming :: Stream -> (OutBodyIface -> IO ()) -> IO ()
sendStreaming strm strmbdy =
    strmbdy
        OutBodyIface
            { outBodyUnmask = id
            , outBodyPush = write
            , outBodyPushFinal = write
            , outBodyFlush = return ()
            , outBodyCancel = cancel
            }
  where
    -- As HTTP/3 does: the reset stays when the stream is closed.
    cancel merr =
        QUIC.resetStream strm $ maybe H3RequestCancelled (const H3InternalError) merr
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
