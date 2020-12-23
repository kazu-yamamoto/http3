{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.HQ.Server (
    run
  , Server
  , Config(..)
  , allocSimpleConfig
  , freeSimpleConfig
  ) where

import Control.Concurrent
import qualified Data.ByteString.Builder.Extra as B
import qualified Control.Exception as E
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Internal as BS
import Data.IORef
import Foreign.ForeignPtr
import Network.HPACK (HeaderTable, toHeaderTable)
import Network.HTTP2.Internal (InpObj(..))
import qualified Network.HTTP2.Internal as H2
import Network.HTTP2.Server (Server, PushPromise)
import Network.HTTP2.Server.Internal (Request(..), Response(..), Aux(..))
import Network.QUIC (Connection, Stream)
import qualified Network.QUIC as QUIC
import qualified System.TimeManager as T

import Imports
import Network.HTTP3.Config
import Network.HTTP3.Recv (newSource, readSource)

run :: Connection -> Config -> Server -> IO ()
run conn conf server = do
    E.bracket open close $ processRequest conf server
    threadDelay 100000
  where
    open = QUIC.acceptStream conn
    close = QUIC.closeStream

processRequest :: Config -> Server -> Stream -> IO ()
processRequest conf server strm = do
    th <- T.register (confTimeoutManager conf) (return ())
    vt <- recvHeader strm
    src <- newSource strm
    refH <- newIORef Nothing
    let readB = readSource src
        req = Request $ InpObj vt Nothing readB refH
        aux = Aux th
    server req aux $ sendResponse conf strm

recvHeader :: Stream -> IO HeaderTable
recvHeader strm = do
    bs <- QUIC.recvStream strm 1024
    let (method,path) = parseRequestLine bs
        vt = (":path",path)
           : (":method", method)
           : (":scheme", "https")
--           : (":authority", undefined)
           : []
    toHeaderTable vt
  where
    parseRequestLine _bs = ("GET","/") -- undefined

sendResponse :: Config -> Stream -> Response -> [PushPromise] -> IO ()
sendResponse conf strm (Response outobj) _ = case H2.outObjBody outobj of
    H2.OutBodyNone -> return ()
    H2.OutBodyFile (H2.FileSpec path fileoff bytecount) -> do
        (pread, sentinel') <- confPositionReadMaker conf path
        let timmgr = confTimeoutManager conf
        refresh <- case sentinel' of
                     H2.Closer closer       -> do
                         th <- T.register timmgr closer
                         return $ T.tickle th
                     H2.Refresher refresher -> return refresher
        let next = H2.fillFileBodyGetNext pread fileoff bytecount refresh
        sendNext strm next
    H2.OutBodyBuilder builder -> do
        let next = H2.fillBuilderBodyGetNext builder
        sendNext strm next
    H2.OutBodyStreaming strmbdy -> sendStreaming strm strmbdy

sendNext :: Stream -> H2.DynaNext -> IO ()
sendNext strm action = do
    fp <- BS.mallocByteString 2048
    H2.Next len mnext <- withForeignPtr fp $ \buf -> action buf 2048 65536 -- window size
    if len == 0 then
        return ()
      else do
        let bs = PS fp 0 len
        QUIC.sendStream strm bs
        case mnext of
          Nothing   -> return ()
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
    if len == 0 then
        return signal
      else do
        let bs = PS fp 0 len
        QUIC.sendStream strm bs
        return signal
