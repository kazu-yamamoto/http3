{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HTTP3.ClientSpec where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
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
                                C.sendRequestWith sendRequest req giveUp $ \_ -> return ()
                    )
                    `shouldReturn` Just (Left ())
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
