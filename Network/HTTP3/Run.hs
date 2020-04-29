{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP3.Run (
    run
  ) where

import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BZ
import Data.IORef
import Network.HPACK (toHeaderTable, HeaderTable)
import Network.HTTP2.Internal (InpObj(..), OutObj(..), OutBody(..))
import Network.HTTP2.Server (Server, PushPromise)
import Network.HTTP2.Server.Internal
import Network.QUIC
import Network.QUIC.Types.Integer

import Imports
import Network.HTTP3.Context
import Network.HTTP3.Frame
import Network.HTTP3.Settings
import Network.HTTP3.Stream
import Network.QPACK

mkType :: H3StreamType -> ByteString
mkType = BS.singleton . fromIntegral . fromH3StreamType

setupUnidirectional :: Connection -> IO ()
setupUnidirectional conn = do
    let st0 = mkType H3ControlStreams
    settings <- encodeH3Settings [(QpackBlockedStreams,100),(QpackMaxTableCapacity,4096),(SettingsMaxHeaderListSize,32768)]
    bs0 <- (st0 `BS.append`) <$> encodeH3Frame (H3Frame H3FrameSettings settings)
    let bs1 = mkType QPACKEncoderStream
    let bs2 = mkType QPACKDecoderStream
    s0 <- unidirectionalStream conn
    s1 <- unidirectionalStream conn
    s2 <- unidirectionalStream conn
    -- fixme
    sendStream s0 bs0
    sendStream s1 bs1
    sendStream s2 bs2

run :: Connection -> Server -> IO ()
run conn server = E.bracket open close $ \ctx -> do
    setupUnidirectional conn
    readerServer ctx server
  where
    open = do
        ref <- newIORef IInit
        newContext conn (controlStream ref)
    close = clearContext

{-
readerClient :: Context -> IO ()
readerClient ctx = loop
  where
    loop = do
        estrm <- accept ctx
        case estrm of
          Right strm -> process strm >> loop
          _          -> return ()
    process strm
      | isClientInitiatedUnidirectional sid = return () -- error
      | isClientInitiatedBidirectional  sid = return ()
      | isServerInitiatedUnidirectional sid = unidirectional ctx strm
      | otherwise                           = return ()
      where
        sid = streamId strm
-}

readerServer :: Context -> Server -> IO ()
readerServer ctx server = loop
  where
    loop = do
        estrm <- accept ctx
        case estrm of
          Right strm -> process strm >> loop
          _          -> return ()
    process strm
      | isClientInitiatedUnidirectional sid = unidirectional ctx strm
      | isClientInitiatedBidirectional  sid = void $ forkIO $ request ctx server strm
      | isServerInitiatedUnidirectional sid = return () -- error
      | otherwise                           = return ()
      where
        sid = streamId strm

controlStream :: IORef IFrame -> InstructionHandler
controlStream ref recv = loop
  where
    loop = do
        bs <- recv 1024
        when (bs /= "") $ do
            readIORef ref >>= parse bs >>= writeIORef ref
            loop
    parse bs st0 = do
        case parseH3Frame st0 bs of
          IDone typ payload leftover -> do
              putStrLn $ "control: " ++ show typ
              case typ of
                H3FrameCancelPush -> print $ decodeInt payload
                H3FrameSettings   -> return () -- decodeH3Settings payload >>= print
                H3FrameGoaway     -> print $ decodeInt payload
                H3FrameMaxPushId  -> print $ decodeInt payload
                _                 -> putStrLn "controlStream: error"
              parse leftover IInit
          st1 -> return st1

request :: Context -> Server -> Stream -> IO ()
request ctx server strm = do
    th <- registerThread ctx
    mvt <- parseHeader ctx strm
    case mvt of
      Nothing -> return ()
      Just vt -> do
          req <- Request . InpObj vt Nothing (return "") <$> newIORef Nothing
          let aux = Aux th
          server req aux $ sendResponse ctx strm

parseHeader :: Context -> Stream -> IO (Maybe HeaderTable)
parseHeader ctx strm = loop IInit
  where
    loop st = do
        bs <- recvStream strm 1024
        if bs == "" then
            return Nothing
          else do
            let iframe = parseH3Frame st bs
            case iframe of
              -- fixme -- leftover
              IDone H3FrameHeaders payload _leftover -> Just <$> qpackDecode ctx payload
              _ -> loop iframe

{-
requestStream _ctx ost0@(HasBody _vt q ref) _sid bs fin = do
    iframe0 <- readIORef ref
    let iframe1 = parseH3Frame iframe0 bs
    case iframe1 of
      IType _ -> writeIORef ref iframe1 >> return ost0
      ILen H3FrameData _ -> writeIORef ref iframe1 >> return ost0
      IPay H3FrameData r t bss -> do
          mapM_ (\b -> atomically $ writeTQueue q b) bss
          writeIORef ref $ IPay H3FrameData r t []
          Body q ref Nothing <$> newIORef 0 <*> newIORef Nothing
      IDone H3FrameData payload leftover -> do
          when (fin && leftover /= "") $ error "requestStream(5)"
          atomically $ writeTQueue q payload
          writeIORef ref IInit
          Body q ref Nothing <$> newIORef 0 <*> newIORef Nothing
      _ -> error "requestStream(6)"
requestStream _ctx ost0@(Body q ref _ _ _) _sid bs fin = do
    iframe0 <- readIORef ref
    let iframe1 = parseH3Frame iframe0 bs
    case iframe1 of
      IType _ -> writeIORef ref iframe1 >> return ost0
      ILen H3FrameData _ -> writeIORef ref iframe1 >> return ost0
      IPay H3FrameData r t bss -> do
          mapM_ (\b -> atomically $ writeTQueue q b) bss
          writeIORef ref $ IPay H3FrameData r t []
          return ost0
      IDone H3FrameData payload leftover -> do
          when (fin && leftover /= "") $ error "requestStream(7)"
          atomically $ writeTQueue q payload
          writeIORef ref IInit
          return ost0
      _ -> error "requestStream(8)"
requestStream _ _ _ _ _ = error "requestStream (final)"
-}

sendResponse :: Context -> Stream -> Response -> [PushPromise] -> IO ()
sendResponse ctx strm (Response outobj) _pp = do
    let hdrs = outObjHeaders outobj
    -- fixme: fixHeaders
    (ths, _) <- toHeaderTable hdrs
    (hdr, "") <- qpackEncode ctx ths
    let html = case outObjBody outobj of
          OutBodyBuilder builder -> BZ.toStrict $ toLazyByteString builder
          _                      -> undefined
    hdrblock <- encodeH3Frame $ H3Frame H3FrameHeaders hdr
    bdyblock <- encodeH3Frame $ H3Frame H3FrameData html
    let hdrbdy = BS.concat [hdrblock,bdyblock]
    sendStream strm hdrbdy
    shutdownStream strm
