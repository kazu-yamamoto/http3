{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module HTTP3.ClientSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Internal (
    ApplicationProtocolError (..),
    StreamResetIsReceived (..),
    StreamResetIsSent (..),
 )
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Client as QUICC
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
                fmap fst <$> timeout 3000000 (request @StreamResetIsReceived)
                    `shouldReturn` Just (Left (StreamResetIsReceived H3RequestRejected))
        it "tells the application of a rejection at once" $
            E.bracket (setupWith rejecting) teardown $ \_ -> do
                r <- timeout 3000000 (request @StreamResetIsReceived)
                fmap snd r `shouldSatisfy` maybe False (< 50_000_000)
        it "resets a stream that ends with no response, as a stream error" $
            E.bracket (setupWith ending) teardown $ \_ ->
                fmap fst <$> timeout 3000000 (request @StreamResetIsSent)
                    `shouldReturn` Just (Left (StreamResetIsSent H3MessageError))

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

-- | Reset with H3_REQUEST_REJECTED, which says the request may be retried
-- (RFC 9114, section 4.1.1).
rejecting :: QUIC.Stream -> IO ()
rejecting strm = QUIC.resetStream strm H3RequestRejected

-- | Ended with no response at all.
ending :: QUIC.Stream -> IO ()
ending = QUIC.shutdownStream

-- | The outcome of a request, and how long it took in nanoseconds.  It used
-- to be a connection error, and to come a tenth of a second late.
request :: E.Exception e => IO (Either e (), Word64)
request = QUICC.run testClientConfig $ \conn ->
    E.bracket C.allocSimpleConfig C.freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
            t0 <- getMonotonicTimeNSec
            r <- E.try $ sendRequest (C.requestNoBody methodGet "/" []) $ \_ -> return ()
            t1 <- getMonotonicTimeNSec
            return (r, t1 - t0)
