{-# LANGUAGE OverloadedStrings #-}

module HTTP3.RunIOSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as B
import Data.IORef
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Client as QUICC
import qualified Network.QUIC.Internal as QUIC
import qualified Network.QUIC.Server as QUICS
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec hiding (runIO)

import HTTP3.Config
import HTTP3.Server (hideLimit, teardown)

spec :: Spec
spec =
    describe "H3 server with runIO" $ do
        it "serves a request while an earlier stream has sent no HEADERS" $
            E.bracket setupIO teardown $ \_ ->
                timeout 3000000 stalledThenRequest `shouldReturn` Just ()
        it "returns once sioDone is called and the requests are answered" $ do
            returned <- newEmptyMVar
            E.bracket (setupDone returned) teardown $ \_ -> do
                -- sioDone comes before the response: the request is still
                -- answered, and only then does runIO return.
                timeout 3000000 oneRequest `shouldReturn` Just ()
                timeout 3000000 (takeMVar returned) `shouldReturn` Just ()
        it "cancels a stream whose request it does not read to the end" $
            E.bracket setupCancel teardown $ \_ -> do
                timeout 3000000 twoRequests `shouldReturn` Just ()
                -- Time for the cancellation to go out.
                threadDelay 100000
                bs <- B.concat <$> readIORef sentOnDecoderStream
                -- Stream Cancellation is 01 and then the stream ID in six bits.
                B.elem 0x40 bs `shouldBe` False
                B.elem 0x44 bs `shouldBe` True
        it "gives back the stream of a request refused with 431" $
            E.bracket setupFewStreams teardown $ \_ ->
                timeout 5000000 tooLongRequests `shouldReturn` Just ()

setupIO :: IO ThreadId
setupIO = do
    sc <- makeTestServerConfig
    tid <- forkIO $ QUICS.run sc $ \conn ->
        E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
            runIO conn conf $ \sio -> return $ forever $ do
                (strm, _req) <- sioReadRequest sio
                sioWriteResponse sio strm $ responseBuilder ok200 [] "hello"
    threadDelay 500000 -- give enough time to the server
    return tid

-- | A server whose application takes one request, calls 'sioDone', and
-- then answers it.  The 'MVar' is filled when 'runIO' returns.
setupDone :: MVar () -> IO ThreadId
setupDone returned = do
    sc <- makeTestServerConfig
    tid <- forkIO $ QUICS.run sc $ \conn ->
        E.bracket allocSimpleConfig freeSimpleConfig $ \conf -> do
            runIO conn conf $ \sio -> return $ do
                (strm, _req) <- sioReadRequest sio
                sioDone sio
                sioWriteResponse sio strm $ responseBuilder ok200 [] "hello"
                forever $ sioReadRequest sio
            putMVar returned ()
    threadDelay 500000 -- give enough time to the server
    return tid

oneRequest :: IO ()
oneRequest = QUICC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
            sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                C.responseStatus rsp `shouldBe` Just ok200

-- | A bidirectional stream carrying only the type of a HEADERS frame, and
-- then an ordinary request on the same connection.
stalledThenRequest :: IO ()
stalledThenRequest = QUICC.run testClientConfig $ \conn -> do
    strm <- QUIC.stream conn
    QUIC.sendStream strm "\x01"
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
            sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                C.responseStatus rsp `shouldBe` Just ok200

-- | A server allowing 'fewStreams' request streams at a time, and keeping to
-- a SETTINGS_MAX_FIELD_SECTION_SIZE it does not announce.
setupFewStreams :: IO ThreadId
setupFewStreams = do
    sc0 <- makeTestServerConfig
    let sc =
            sc0
                { QUIC.scParameters =
                    (QUIC.scParameters sc0){QUIC.initialMaxStreamsBidi = fewStreams}
                }
    tid <- forkIO $ QUICS.run sc $ \conn ->
        E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
            runIO conn (hideLimit conf) $ \sio -> return $ forever $ do
                (strm, _req) <- sioReadRequest sio
                sioWriteResponse sio strm $ responseBuilder ok200 [] "hello"
    threadDelay 500000 -- give enough time to the server
    return tid

fewStreams :: Int
fewStreams = 3

-- | More requests refused with 431 than 'fewStreams', one after another: each
-- needs the stream of an earlier one given back.
tooLongRequests :: IO ()
tooLongRequests = QUICC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
            replicateM_ (fewStreams * 3) $ do
                let req = C.requestNoBody methodGet "/" [("x-a", B.replicate 40000 0x7e)]
                sendRequest req $ \rsp ->
                    C.responseStatus rsp `shouldBe` Just requestHeaderFieldsTooLarge431

-- | RFC 9204, section 4.4.2: a decoder that gives up on a stream sends a
-- Stream Cancellation.  The application reads the body of the request on
-- stream 0 to its end, and not that on stream 4.
setupCancel :: IO ThreadId
setupCancel = do
    writeIORef sentOnDecoderStream []
    sc0 <- makeTestServerConfig
    let sc = sc0{QUIC.scHooks = (QUIC.scHooks sc0){QUIC.onPlainCreated = recordDecoderStream}}
    tid <- forkIO $ QUICS.run sc $ \conn ->
        E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
            runIO conn conf $ \sio -> return $ forever $ do
                (strm, req) <- sioReadRequest sio
                when (QUIC.streamId strm == 0) $ do
                    let drain = do
                            bs <- getRequestBodyChunk req
                            unless (B.null bs) drain
                    drain
                sioWriteResponse sio strm $ responseBuilder ok200 [] "hello"
    threadDelay 500000 -- give enough time to the server
    return tid

twoRequests :: IO ()
twoRequests = QUICC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
            replicateM_ 2 $
                sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                    C.responseStatus rsp `shouldBe` Just ok200

-- | The server's QPACK decoder stream: its control, encoder and decoder
-- streams are 3, 7 and 11.
serverDecoderStream :: QUIC.StreamId
serverDecoderStream = 11

{-# NOINLINE sentOnDecoderStream #-}
sentOnDecoderStream :: IORef [B.ByteString]
sentOnDecoderStream = unsafePerformIO $ newIORef []

-- | The hook is pure, so this is the only way to see what goes out.
{-# NOINLINE recordDecoderStream #-}
recordDecoderStream :: QUIC.EncryptionLevel -> QUIC.Plain -> QUIC.Plain
recordDecoderStream _ plain = unsafePerformIO $ do
    forM_ (QUIC.plainFrames plain) $ \frame -> case frame of
        QUIC.StreamF sid _ dats _
            | sid == serverDecoderStream ->
                atomicModifyIORef' sentOnDecoderStream $ \xs -> (xs ++ dats, ())
        _ -> return ()
    return plain
