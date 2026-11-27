{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HTTP3.ClientSpec where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Semantics.Error (
    ErrorKind (..),
    HTTPError (..),
    HTTPErrorCode (..),
    isRetryable,
 )
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Client as QUICC
import qualified Network.QUIC.Internal as QUIC
import qualified Network.QUIC.Server as QUICS
import System.Timeout (timeout)
import Test.Hspec

import HTTP3.Config
import HTTP3.Server (teardown)

spec :: Spec
spec =
    describe "H3 client" $ do
        it "tells the application of a request the server rejected" $
            E.bracket (setupWith rejecting) teardown $ \_ ->
                fmap (either (Left . kind) Right . fst) <$> timeout 3000000 request
                    `shouldReturn` Just (Left (Just Refused, True))
        it "tells the application of a rejection at once" $
            E.bracket (setupWith rejecting) teardown $ \_ -> do
                r <- timeout 3000000 request
                fmap snd r `shouldSatisfy` maybe False (< 50_000_000)
        it "resets a stream that ends with no response, as a stream error" $
            E.bracket (setupWith ending) teardown $ \_ ->
                fmap (either (Left . kind) Right . fst) <$> timeout 3000000 request
                    `shouldReturn` Just (Left (Just MalformedMessage, False))
        it "gives up a request waiting for the response" $
            E.bracket (setupWith silent) teardown $ \_ ->
                timeout
                    3000000
                    ( QUICC.run testClientConfig $ \conn ->
                        E.bracket C.allocSimpleConfig C.freeSimpleConfig $ \conf ->
                            C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
                                giveUp <- deadline 200000
                                let req = C.requestNoBody methodGet "/" []
                                C.sendRequestWith sendRequest req (\_ -> return ()) giveUp
                    )
                    `shouldReturn` Just (Left ())
        it "gives up a request whose body the server does not read" $ do
            -- The sender, held by flow control, is woken by the reset the
            -- give-up sends, before the connection ends.
            reset <- newEmptyMVar
            E.bracket (setupWith $ awaitReset reset) teardown $ \_ -> do
                r <- timeout 3000000 $ QUICC.run testClientConfig $ \conn ->
                    E.bracket C.allocSimpleConfig C.freeSimpleConfig $ \conf ->
                        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
                            senderEnded <- newEmptyMVar
                            giveUp <- deadline 200000
                            let body push _flush =
                                    forever (push $ BB.byteString $ B.replicate 16384 0x61)
                                        `E.finally` putMVar senderEnded ()
                                req = C.requestStreaming methodPost "/" [] body
                            g <- C.sendRequestWith sendRequest req (\_ -> return ()) giveUp
                            e <- timeout 1000000 $ takeMVar senderEnded
                            return (g, e)
                r `shouldBe` Just (Left (), Just ())
                timeout 1000000 (takeMVar reset) `shouldReturn` Just (Just C.H3RequestCancelled)
        it "acknowledges a section with no credit, not holding up the request" $
            -- The Section Acknowledgement waited for credit on the decoder
            -- stream, and the request with it, past its give-up.
            E.bracket setupDynamic teardown $ \_ -> do
                r <- timeout 3000000 $ QUICC.run testClientConfig $ \conn ->
                    E.bracket C.allocSimpleConfig C.freeSimpleConfig $ \conf ->
                        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
                            giveUp <- deadline 1000000
                            let body push _flush =
                                    forever (push $ BB.byteString $ B.replicate 16384 0x61)
                                req = C.requestStreaming methodPost "/" [] body
                            C.sendRequestWith sendRequest req (\_ -> return ()) giveUp
                r `shouldBe` Just (Right ())
        it "gives up a connection with runGivingUp" $
            E.bracket (setupWith silent) teardown $ \_ -> do
                giveUp <- deadline 200000
                r <- timeout 3000000 $ QUICC.run testClientConfig $ \conn ->
                    E.bracket C.allocSimpleConfig C.freeSimpleConfig $ \conf ->
                        C.runGivingUp
                            conn
                            testH3ClientConfig
                            conf
                            ( \sendRequest _aux ->
                                sendRequest (C.requestNoBody methodGet "/" []) $ \_ -> return ()
                            )
                            giveUp
                r `shouldBe` Just (Left ())
        it "gives up setting up a connection which grants no unidirectional stream" $
            -- The control and QPACK streams could not be opened, and the
            -- client waited for them with no give-up.
            E.bracket setupNoUni teardown $ \_ -> do
                giveUp <- deadline 200000
                r <- timeout 3000000 $ QUICC.run testClientConfig $ \conn ->
                    E.bracket C.allocSimpleConfig C.freeSimpleConfig $ \conf ->
                        C.runGivingUp conn testH3ClientConfig conf (\_ _ -> return ()) giveUp
                r `shouldBe` Just (Left ())

-- | A plain QUIC server doing this to every request stream.
setupWith :: (QUIC.Stream -> IO ()) -> IO ThreadId
setupWith act = do
    sc <- makeTestServerConfig
    tid <- forkIO $ QUICS.run sc $ \conn -> forever $ do
        strm <- QUIC.acceptStream conn
        when (QUIC.isClientInitiatedBidirectional $ QUIC.streamId strm) $
            act strm
    threadDelay 500000 -- give enough time to the server
    return tid

-- | A plain QUIC server granting the client little credit, and answering
-- after it is used up with a field section from the dynamic table.
setupDynamic :: IO ThreadId
setupDynamic = do
    sc0 <- makeTestServerConfig
    let params =
            (QUIC.scParameters sc0)
                { QUIC.initialMaxData = 16384
                , QUIC.initialMaxStreamDataBidiRemote = 16384
                }
        sc = sc0{QUIC.scParameters = params}
    tid <- forkIO $ QUICS.run sc $ \conn -> forever $ do
        strm <- QUIC.acceptStream conn
        when (QUIC.isClientInitiatedBidirectional $ QUIC.streamId strm) $
            void $
                forkIO $ do
                    threadDelay 300000
                    enc <- QUIC.unidirectionalStream conn
                    -- Set Dynamic Table Capacity 100, Insert x-foo: bar.
                    QUIC.sendStream enc $ B.pack [0x02, 0x3f, 0x45, 0x45] <> "x-foo\x03\&bar"
                    -- Required Insert Count 1; :status 200 and the entry.
                    QUIC.sendStream strm $ B.pack [0x01, 0x04, 0x02, 0x00, 0xd9, 0x80]
    threadDelay 500000 -- give enough time to the server
    return tid

-- | A plain QUIC server granting the client no unidirectional stream.
setupNoUni :: IO ThreadId
setupNoUni = do
    sc0 <- makeTestServerConfig
    let sc = sc0{QUIC.scParameters = (QUIC.scParameters sc0){QUIC.initialMaxStreamsUni = 0}}
    tid <- forkIO $ QUICS.run sc $ \conn -> forever $ QUIC.acceptStream conn
    threadDelay 500000 -- give enough time to the server
    return tid

-- | Reset with H3_REQUEST_REJECTED, which says the request may be retried
-- (RFC 9114, section 4.1.1).
rejecting :: QUIC.Stream -> IO ()
rejecting strm = QUIC.resetStream strm C.H3RequestRejected

-- | Reading nothing, and telling the reset of the client's, if one comes.
awaitReset :: MVar (Maybe QUIC.ApplicationProtocolError) -> QUIC.Stream -> IO ()
awaitReset reset strm = void $ forkIO $ loop (100 :: Int)
  where
    loop 0 = putMVar reset Nothing
    loop n = do
        mr <- QUIC.resetReceived strm
        case mr of
            Just code -> putMVar reset $ Just code
            Nothing -> threadDelay 20000 >> loop (n - 1)

-- | Never answered.
silent :: QUIC.Stream -> IO ()
silent _ = return ()

-- | Succeeding once the microseconds have passed.
deadline :: Int -> IO (STM ())
deadline us = (\late -> readTVar late >>= check) <$> registerDelay us

-- | Ended with no response at all.
ending :: QUIC.Stream -> IO ()
ending = QUIC.shutdownStream

-- | The outcome of a request, and how long it took in nanoseconds.  It used
-- to be a connection error, and to come a tenth of a second late.
request :: IO (Either HTTPError (), Word64)
request = QUICC.run testClientConfig $ \conn ->
    E.bracket C.allocSimpleConfig C.freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
            t0 <- getMonotonicTimeNSec
            r <- E.try $ sendRequest (C.requestNoBody methodGet "/" []) $ \_ -> return ()
            t1 <- getMonotonicTimeNSec
            return (r, t1 - t0)

-- | What a stream error means, and whether the request may be retried.
kind :: HTTPError -> (Maybe ErrorKind, Bool)
kind e = (k e, isRetryable e)
  where
    k (StreamErrorIsReceived c _) = Just $ errorKind c
    k (StreamErrorIsSent c _ _) = Just $ errorKind c
    k _ = Nothing
