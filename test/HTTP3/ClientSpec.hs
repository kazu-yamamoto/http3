{-# LANGUAGE OverloadedStrings #-}

module HTTP3.ClientSpec where

import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Internal (
    ApplicationProtocolError (..),
    StreamResetIsReceived (..),
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
    describe "H3 client" $
        it "tells the application of a request the server rejected" $
            E.bracket setupRejecting teardown $ \_ ->
                timeout 3000000 rejectedRequest
                    `shouldReturn` Just (Left (StreamResetIsReceived H3RequestRejected))

-- | A server that resets every request stream with H3_REQUEST_REJECTED,
-- which says the request may be retried (RFC 9114, section 4.1.1).
setupRejecting :: IO ThreadId
setupRejecting = do
    sc <- makeTestServerConfig
    tid <- forkIO $ QUICS.run sc $ \conn -> forever $ do
        strm <- QUIC.acceptStream conn
        when (QUIC.isClientInitiatedBidirectional $ QUIC.streamId strm) $
            QUIC.resetStream strm H3RequestRejected
    threadDelay 500000 -- give enough time to the server
    return tid

-- | The client used to reset the stream back with H3_MESSAGE_ERROR and throw
-- a generic error, so the application could not tell it might retry.
rejectedRequest :: IO (Either StreamResetIsReceived ())
rejectedRequest = QUICC.run testClientConfig $ \conn ->
    E.bracket C.allocSimpleConfig C.freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux ->
            E.try $ sendRequest (C.requestNoBody methodGet "/" []) $ \_ -> return ()
