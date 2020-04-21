{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP3.Run (
    run
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BZ
import Data.IORef
import Network.HPACK (toHeaderTable)
import Network.HTTP2.Internal (InpObj(..), OutObj(..), OutBody(..))
import Network.HTTP2.Server (Server)
import Network.HTTP2.Server.Internal
import Network.QUIC
import Network.QUIC.Connection (isServer)
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
    if isServer conn then do
        sendStream conn serverControlStream bs0 False
        sendStream conn serverEncoderStream bs1 False
        sendStream conn serverDecoderStream bs2 False
      else do
        sendStream conn clientControlStream bs0 False
        sendStream conn clientEncoderStream bs1 False
        sendStream conn clientDecoderStream bs2 False

run :: Connection -> Server -> IO ()
run conn svr = E.bracket open close $ \(enc, handleDI, _, dec, handleEI, _) -> do
    q <- newTQueueIO
    ctx <- newContext conn (write q) enc handleDI dec handleEI
    ref <- newIORef IInit
    tid0 <- forkIO $ controlStream ref q
    tid1 <- forkIO $ reader ctx
    tid2 <- forkIO $ worker ctx svr
    setupUnidirectional conn
    sender ctx `E.finally` do
        killThread tid0
        killThread tid1
        killThread tid2
  where
    open = do
        (enc, handleDI, cleanE) <- newQEncoder defaultQEncoderConfig
        (dec, handleEI, cleanD) <- newQDecoder defaultQDecoderConfig
        return (enc, handleDI, cleanE, dec, handleEI, cleanD)
    close (_, _, cleanE, _, _, cleanD) = do
        void $ cleanE
        void $ cleanD

reader :: Context -> IO ()
reader ctx
  | isH3Server ctx = readerServer ctx
  | otherwise      = readerClient ctx

readerClient :: Context -> IO ()
readerClient ctx = loop
  where
    loop = do
        (sid, bs, fin) <- recv ctx
        process sid bs fin
        loop
    process sid bs _fin
      | isClientInitiatedUnidirectional sid = return () -- error
      | isClientInitiatedBidirectional  sid = return ()
      | isServerInitiatedUnidirectional sid = unidirectional ctx sid bs
      | otherwise                           = return ()

readerServer :: Context -> IO ()
readerServer ctx = loop
  where
    loop = do
        (sid, bs, fin) <- recv ctx
        process sid bs fin
        loop
    process sid bs fin
      | isClientInitiatedUnidirectional sid = unidirectional ctx sid bs
      | isClientInitiatedBidirectional  sid = do
            reqstrm <- getRequestStream ctx sid
            ost <- getOpenState reqstrm
            ost' <- requestStream ctx ost sid bs fin
            setOpenState reqstrm ost'
      | isServerInitiatedUnidirectional sid = return () -- error
      | otherwise                           = return ()

sender :: Context -> IO ()
sender _ctx = forever $ threadDelay 1000000

write :: TQueue ByteString -> ByteString -> IO ()
write q bs = atomically $ writeTQueue q bs

controlStream :: IORef IFrame -> TQueue ByteString -> IO ()
controlStream ref q = forever $ do
    bs <- atomically $ readTQueue q
    readIORef ref >>= loop bs >>= writeIORef ref
 where
    loop bs st0 = do
        case parseH3Frame st0 bs of
          IDone typ payload leftover -> do
              putStrLn $ "control: " ++ show typ
              case typ of
                H3FrameCancelPush -> print $ decodeInt payload
                H3FrameSettings   -> decodeH3Settings payload >>= print
                H3FrameGoaway     -> print $ decodeInt payload
                H3FrameMaxPushId  -> print $ decodeInt payload
                _                 -> putStrLn "controlStream: error"
              loop leftover IInit
          st1 -> return st1

requestStream :: Context -> OpenState -> StreamId -> ByteString -> Fin -> IO OpenState
requestStream _   ost _ "" False = return ost
requestStream _   ost _ "" True  = return ost -- fixme: removing state
requestStream ctx JustOpened sid bs fin = do
    let iframe = parseH3Frame IInit bs
    case iframe of
      IType _ -> Continued <$> newIORef iframe
      ILen H3FrameHeaders _ -> Continued <$> newIORef iframe
      IPay H3FrameHeaders _ _ _ -> Continued <$> newIORef iframe
      IDone H3FrameHeaders payload leftover -> do
          when (fin && leftover /= "") $ error "requestStream(3)"
          vt <- qpackDecode ctx payload
          ost <- if fin then do
              inpobj <- InpObj vt Nothing (return "") <$> newIORef Nothing
              putInput ctx $ Input sid inpobj Nothing
              return $ NoBody vt
            else do
              q <- newTQueueIO
              let getBody = atomically $ readTQueue q
              inpobj <- InpObj vt Nothing getBody <$> newIORef Nothing
              putInput ctx $ Input sid inpobj (Just q)
              HasBody vt q <$> newIORef IInit
          if leftover == "" then
              return ost
            else
              requestStream ctx ost sid leftover fin
      _ -> error "requestStream(4)"
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

worker :: Context -> Server -> IO ()
worker ctx server = forever $ do
    Input sid inpobj _q <- takeInput ctx
    let req = Request inpobj
    server req undefined $ sendResponse ctx sid

sendResponse :: Context -> StreamId -> Response -> p -> IO ()
sendResponse ctx sid (Response outobj) _pps = do
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
    send ctx sid hdrbdy True
