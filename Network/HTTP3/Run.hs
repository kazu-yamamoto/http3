{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP3.Run where

import qualified Control.Exception as E

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString as BS
import Data.CaseInsensitive hiding (map)
import Data.IORef
import qualified Data.IntMap as I
import Network.HPACK (TokenHeader, HeaderValue)
import Network.HPACK.Token
import Network.HTTP.Types
import Network.QUIC
import Network.QUIC.Connection

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

run :: Connection -> IO ()
run conn = E.bracket open close $ \(enc, handleDI, _, dec, handleEI, _) -> do
    q <- newTQueueIO
    ctx <- newContext conn (write q) enc handleDI dec handleEI
    tid0 <- forkIO $ handleControl q
    tid1 <- forkIO $ reader ctx
    setupUnidirectional conn
    sender ctx `E.finally` do
        killThread tid0
        killThread tid1
  where
    open = do
        (enc, handleDI, cleanE) <- newEncoder defaultEncoderConfig
        (dec, handleEI, cleanD) <- newDecoder defaultDecoderConfig
        return (enc, handleDI, cleanE, dec, handleEI, cleanD)
    close (_, _, cleanE, _, _, cleanD) = do
        void $ cleanE
        void $ cleanD

reader :: Context -> IO ()
reader ctx
  | isServer (ctxConnection ctx) = readerServer ctx
  | otherwise                    = readerClient ctx

readerClient :: Context -> IO ()
readerClient ctx = loop
  where
    loop = do
        (sid, bs, fin) <- recvStream $ ctxConnection ctx
        process sid bs fin
        loop
    process sid bs _fin
      | isClientInitiatedUnidirectional sid = return () -- error
      | isClientInitiatedBidirectional  sid = return ()
      | isServerInitiatedUnidirectional sid = handle ctx sid bs
      | otherwise                           = return ()

readerServer :: Context -> IO ()
readerServer ctx = loop
  where
    loop = do
        (sid, bs, fin) <- recvStream $ ctxConnection ctx
        process sid bs fin
        loop
    process sid bs _fin
      | isClientInitiatedUnidirectional sid = handle ctx sid bs
      | isClientInitiatedBidirectional  sid = do
            when (bs /= "") $ do
                H3Frame ftyp bdy <- decodeH3Frame bs
                when (ftyp == H3FrameHeaders) $ do
                    ctxDecoder ctx bdy >>= mapM_ print
                    print _fin
                (hdr, "") <- ctxEncoder ctx $ map toT serverHeader
                hdrblock <- encodeH3Frame $ H3Frame H3FrameHeaders hdr
                bdyblock <- encodeH3Frame $ H3Frame H3FrameData html
                let hdrbdy = BS.concat [hdrblock,bdyblock]
                sendStream (ctxConnection ctx) sid hdrbdy True
      | isServerInitiatedUnidirectional sid = return () -- error
      | otherwise                           = return ()

handle :: Context -> StreamId -> ByteString -> IO ()
handle _   _   "" = return ()
handle ctx sid bs = do
    m <- readIORef $ ctxMap ctx
    case I.lookup sid m of
      Nothing -> case BS.uncons bs of
        Nothing -> return ()
        Just (w, bs') -> do
            let typ = toH3StreamType $ fromIntegral w
                m' = I.insert sid typ m
            writeIORef (ctxMap ctx) m'
            when (bs' /= "") $ ctxSwitch ctx typ bs'
      Just typ -> ctxSwitch ctx typ bs

sender :: Context -> IO ()
sender _ctx = forever $ threadDelay 1000000

write :: TQueue ByteString -> ByteString -> IO ()
write q bs = atomically $ writeTQueue q bs

handleControl :: TQueue ByteString -> IO ()
handleControl q = forever $ do
    bs <- atomically $ readTQueue q
    H3Frame H3FrameSettings settings <- decodeH3Frame bs
    -- fixme
    void $ decodeH3Settings settings

html :: ByteString
html = "<html><head><title>Welcome to QUIC in Haskell</title></head><body><p>Welcome to QUIC in Haskell.</p></body></html>"

serverHeader :: ResponseHeaders
serverHeader = [
    (":status", "200")
  , ("Content-Type", "text/html; charset=utf-8")
  , ("Server", name)
  ]

toT :: (HeaderName, HeaderValue) -> TokenHeader
toT (k,v) = (toToken $ foldedCase k, v)

name :: ByteString
name = "HaskellQuic/0.0.0"
