{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HTTP3.ServerSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as B
import Data.IORef
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Internal (
    ApplicationProtocolError (..),
    H3Frame (..),
    H3FrameType (..),
 )
import Network.HTTP3.Server
import Network.QPACK (
    FieldSectionTooLargeForPeer (..),
    QDecoderConfig (..),
 )
import qualified Network.QUIC as Q
import qualified Network.QUIC.Client as QUIC
import Network.QUIC.Internal (
    ClientConfig (..),
    EncryptionLevel,
    Frame (..),
    Hooks (..),
    Plain (..),
    StreamId,
 )
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

import HTTP3.Config
import HTTP3.Server

spec :: Spec
spec = beforeAll (setup server 4096) $ afterAll teardown h3spec

h3spec :: SpecWith a
h3spec = do
    describe "H3 server" $ do
        it "handles normal cases" $ \_ -> runClient
        it "tells the application the peer's address, not its own" $ \_ ->
            runSockAddrClient
        it "reads a body past a DATA frame that is empty" $ \_ ->
            runEmptyDataClient
        it "cancels a stream whose response it does not read to the end" $ \_ ->
            runCancelClient
        it "stops reading a unidirectional stream of an unknown type" $ \_ ->
            runUnknownStreamClient
        it "does not send a request header section over the server's limit" $ \_ ->
            runTooLargeForServerClient
        it "does not send a response header section over the client's limit" $ \_ ->
            runTooLargeForClientClient

runClient :: IO ()
runClient = QUIC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf client
  where
    client :: C.Client ()
    client sendRequest _aux =
        foldr1
            concurrently_
            [ client0 sendRequest _aux
            , client1 sendRequest _aux
            , client2 sendRequest _aux
            , client3 sendRequest _aux
            ]

-- | The server reports both addresses it was handed; they must differ.
--
-- Over loopback the host part is 127.0.0.1 either way, so it is the port that
-- tells them apart: the server's is fixed, the client's is ephemeral.
-- 'getPeerSockAddr' used to return the server's own address, which made these
-- two identical and left every application logging or filtering on the client
-- address looking at itself.
runSockAddrClient :: IO ()
runSockAddrClient = QUIC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
            let req = C.requestNoBody methodGet "/sockaddr" []
            sendRequest req $ \rsp -> do
                C.responseStatus rsp `shouldBe` Just ok200
                body <- consume rsp
                case B.split 0x20 body of
                    [mine, peer] -> peer `shouldNotBe` mine
                    _ -> expectationFailure $ "unexpected body: " ++ show body
  where
    consume rsp = go id
      where
        go build = do
            bs <- C.getResponseBodyChunk rsp
            if B.null bs then return (B.concat (build [])) else go (build . (bs :))

-- | A DATA frame may carry nothing (RFC 9114, section 7.2.1).  One sent right
-- after HEADERS used to be taken for the end of the body, and the server read
-- nothing of what followed.
runEmptyDataClient :: IO ()
runEmptyDataClient = QUIC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf0 -> do
        let hooks = (confHooks conf0){C.onHeadersFrameCreated = (++ [emptyData])}
            conf = conf0{confHooks = hooks}
            req = C.requestBuilder methodPost "/length" [] "hello"
        C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
            sendRequest req $ \rsp -> do
                C.responseStatus rsp `shouldBe` Just ok200
                C.getResponseBodyChunk rsp `shouldReturn` "5"
  where
    emptyData = H3Frame H3FrameData ""

-- | RFC 9204, section 4.4.2: a decoder that gives up on a stream sends a
-- Stream Cancellation, so that the encoder stops keeping the entries the
-- field sections it will never hear about refer to.  None was ever sent.
--
-- The first response is read to the end and the second is not; only the
-- second is cancelled.  What the client writes on its decoder stream is taken
-- from the QUIC packets it sends.
runCancelClient :: IO ()
runCancelClient = do
    writeIORef sentOnDecoderStream []
    let qcc =
            testClientConfig
                { ccHooks =
                    (ccHooks testClientConfig){onPlainCreated = recordDecoderStream}
                }
    QUIC.run qcc $ \conn ->
        E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
            C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
                let req = C.requestNoBody methodGet "/" []
                -- Stream 0, read to the end.
                sendRequest req $ \rsp -> do
                    let drain = do
                            bs <- C.getResponseBodyChunk rsp
                            unless (B.null bs) drain
                    drain
                -- Stream 4, not.
                sendRequest req $ \_rsp -> return ()
                -- Time for the cancellation to go out.
                threadDelay 100000
    bs <- B.concat <$> readIORef sentOnDecoderStream
    -- Stream Cancellation is 01 and then the stream ID in six bits.
    B.elem 0x40 bs `shouldBe` False
    B.elem 0x44 bs `shouldBe` True

-- | RFC 9114, section 6.2: the receiver of a stream of an unknown type must
-- stop reading it or throw away what arrives on it.  The server did neither.
--
-- STOP_SENDING is answered with a RESET_STREAM carrying the same code, so the
-- client sending one for its stream of a reserved type shows the server asked
-- it to stop.  The connection goes on.
runUnknownStreamClient :: IO ()
runUnknownStreamClient = do
    writeIORef sentResets []
    let qcc =
            testClientConfig
                { ccHooks =
                    (ccHooks testClientConfig){onPlainCreated = recordResets}
                }
    sid <- QUIC.run qcc $ \conn ->
        E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
            C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
                -- 0x21 is the first of the reserved types, 0x1f * N + 0x21.
                strm <- Q.unidirectionalStream conn
                Q.sendStream strm "\x21hello"
                let req = C.requestNoBody methodGet "/" []
                sendRequest req $ \rsp ->
                    C.responseStatus rsp `shouldBe` Just ok200
                threadDelay 200000
                return $ Q.streamId strm
    lookup sid <$> readIORef sentResets
        `shouldReturn` Just H3StreamCreationError

{-# NOINLINE sentResets #-}
sentResets :: IORef [(StreamId, ApplicationProtocolError)]
sentResets = unsafePerformIO $ newIORef []

{-# NOINLINE recordResets #-}
recordResets :: EncryptionLevel -> Plain -> Plain
recordResets _ plain = unsafePerformIO $ do
    forM_ (plainFrames plain) $ \frame -> case frame of
        ResetStream sid aerr _ ->
            atomicModifyIORef' sentResets $ \xs -> ((sid, aerr) : xs, ())
        _ -> return ()
    return plain

-- | RFC 9114, section 4.2.2: an endpoint "SHOULD NOT send an HTTP message
-- header that exceeds the indicated size".  The peer's limit used to be kept
-- and never consulted.
--
-- 40000 octets in one field is over the server's 32768; the caller is told,
-- and the connection goes on.
runTooLargeForServerClient :: IO ()
runTooLargeForServerClient = QUIC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
            let hello = C.requestNoBody methodGet "/" []
                ok rsp = C.responseStatus rsp `shouldBe` Just ok200
            -- The server's SETTINGS in first, so that its limit is known.
            sendRequest hello ok
            waitForSettings
            let big = C.requestNoBody methodGet "/" [("x-a", B.replicate 40000 0x62)]
                tooLarge FieldSectionTooLargeForPeer{} = True
            sendRequest big (\_ -> return ()) `shouldThrow` tooLarge
            sendRequest hello ok

-- | The client says it takes 1000 octets, and /bigheader answers with some
-- 2000.  The server does not send it, and resets the stream with
-- H3_INTERNAL_ERROR: the failure is its own.
runTooLargeForClientClient :: IO ()
runTooLargeForClientClient = do
    let qcc =
            testClientConfig
                { ccHooks =
                    (ccHooks testClientConfig)
                        { onResetStreamReceived = \_ aerr ->
                            E.throwIO $ Q.ApplicationProtocolErrorIsReceived aerr ""
                        }
                }
        isInternalError (Q.ApplicationProtocolErrorIsReceived aerr _) =
            aerr == H3InternalError
        isInternalError _ = False
        client = QUIC.run qcc $ \conn ->
            E.bracket allocSimpleConfig freeSimpleConfig $ \conf0 -> do
                let conf =
                        conf0
                            { confQDecoderConfig =
                                defaultQDecoderConfig{dcMaxFieldSectionSize = 1000}
                            }
                C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
                    -- Our SETTINGS in at the server first.
                    sendRequest (C.requestNoBody methodGet "/" []) $ \rsp ->
                        C.responseStatus rsp `shouldBe` Just ok200
                    waitForSettings
                    sendRequest (C.requestNoBody methodGet "/bigheader" []) (\_ -> return ())
    client `shouldThrow` isInternalError

-- | The client's QPACK decoder stream: its third unidirectional stream, after
-- the control stream and the encoder stream.
clientDecoderStream :: StreamId
clientDecoderStream = 10

{-# NOINLINE sentOnDecoderStream #-}
sentOnDecoderStream :: IORef [B.ByteString]
sentOnDecoderStream = unsafePerformIO $ newIORef []

-- | The hook is pure, so this is the only way to see what goes out.
{-# NOINLINE recordDecoderStream #-}
recordDecoderStream :: EncryptionLevel -> Plain -> Plain
recordDecoderStream _ plain = unsafePerformIO $ do
    forM_ (plainFrames plain) $ \frame -> case frame of
        StreamF sid _ dats _
            | sid == clientDecoderStream ->
                atomicModifyIORef' sentOnDecoderStream $ \xs -> (xs ++ dats, ())
        _ -> return ()
    return plain

client0 :: C.Client ()
client0 sendRequest _aux = do
    let req = C.requestNoBody methodGet "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just ok200

client1 :: C.Client ()
client1 sendRequest _aux = do
    let req = C.requestNoBody methodGet "/something" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just notFound404

client2 :: C.Client ()
client2 sendRequest _aux = do
    let req = C.requestNoBody methodPut "/" []
    sendRequest req $ \rsp -> do
        C.responseStatus rsp `shouldBe` Just methodNotAllowed405

client3 :: C.Client ()
client3 sendRequest _aux = do
    let req0 = C.requestFile methodPost "/echo" [] $ FileSpec "test/inputFile" 0 1012731
        req = C.setRequestTrailersMaker req0 maker
    sendRequest req $ \rsp -> do
        let comsumeBody = do
                bs <- C.getResponseBodyChunk rsp
                unless (B.null bs) comsumeBody
        comsumeBody
        mt <- C.getResponseTrailers rsp
        firstTrailerValue <$> mt
            `shouldBe` Just "b0870457df2b8cae06a88657a198d9b52f8e2b0a"
  where
    maker = trailersMaker hashInit
