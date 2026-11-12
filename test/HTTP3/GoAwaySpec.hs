{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTTP3.GoAwaySpec where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (forever, when)
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.QUIC as QUIC
import qualified Network.QUIC.Client as QUICC
import qualified Network.QUIC.Server as QUICS
import System.Timeout (timeout)
import Test.Hspec

import HTTP3.Config
import HTTP3.Server (teardown)

spec :: Spec
spec = do
    describe "H3 client told to go away" $
        -- RFC 9114, section 5.2: a request at or above the GOAWAY's ID will
        -- not be processed, and none is to be made after it.
        it "fails a request the server will not process, and refuses new ones" $
            E.bracket setupIgnoring teardown $ \_ ->
                timeout 3000000 ignoredRequests `shouldReturn` Just (True, True)
    describe "H3 server going away" $
        -- RFC 9114, section 5.2: the request taken before GOAWAY is answered,
        -- one opened after it is rejected, and the connection then ends.
        do
            it "answers the request it took and rejects a later one" $ do
                stop <- newTVarIO False
                returned <- newEmptyMVar
                E.bracket (setupGoAway stop returned) teardown $ \_ -> do
                    -- Long after the second GOAWAY.
                    timeout 5000000 (slowThenFast 500000 stop)
                        `shouldReturn` Just (Just ok200, False)
                    timeout 3000000 (takeMVar returned) `shouldReturn` Just ()
            -- The first GOAWAY has the largest identifier, and the second
            -- comes a PTO later: a request sent in between is taken.
            it "answers a request sent between its two GOAWAYs" $ do
                stop <- newTVarIO False
                returned <- newEmptyMVar
                E.bracket (setupGoAway stop returned) teardown $ \_ -> do
                    timeout 5000000 (slowThenFast 0 stop) `shouldReturn` Just (Just ok200, True)
                    timeout 3000000 (takeMVar returned) `shouldReturn` Just ()

-- | A server going away once the variable is set; the 'MVar' is filled when
-- 'runWith' returns.  "/slow" is answered after a second.
setupGoAway :: TVar Bool -> MVar () -> IO ThreadId
setupGoAway stop returned = do
    sc <- makeTestServerConfig
    tid <- forkIO $ QUICS.run sc $ \conn ->
        E.bracket allocSimpleConfig freeSimpleConfig $ \conf -> do
            runWith (readTVar stop >>= check) conn conf svr
            putMVar returned ()
    threadDelay 500000 -- give enough time to the server
    return tid
  where
    svr req _aux sendResponse = do
        case requestPath req of
            Just "/slow" -> threadDelay 1000000
            _ -> return ()
        sendResponse (responseBuilder ok200 [] "hello") []

-- | A request for "/slow"; the server told to go away; then, after the
-- delay, a request for "/".  The status of the first, and whether the
-- second succeeded.
slowThenFast :: Int -> TVar Bool -> IO (Maybe Status, Bool)
slowThenFast delay stop = QUICC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
            let get path = sendRequest (C.requestNoBody methodGet path []) $ return . C.responseStatus
            slow <- async $ get "/slow"
            threadDelay 100000
            atomically $ writeTVar stop True
            threadDelay delay
            fast <- E.try $ get "/"
            s <- wait slow
            return (s, either (\(_ :: E.SomeException) -> False) (const True) fast)

-- | A bare HTTP/3 server: SETTINGS, and a GOAWAY with ID 0 for each request,
-- which it never answers.
setupIgnoring :: IO ThreadId
setupIgnoring = do
    sc <- makeTestServerConfig
    tid <- forkIO $ QUICS.run sc $ \conn -> do
        ctl <- QUIC.unidirectionalStream conn
        -- Control stream, then SETTINGS with no parameters.
        QUIC.sendStream ctl "\x00\x04\x00"
        forever $ do
            strm <- QUIC.acceptStream conn
            when (QUIC.isClientInitiatedBidirectional $ QUIC.streamId strm) $
                QUIC.sendStream ctl "\x07\x01\x00"
    threadDelay 500000 -- give enough time to the server
    return tid

-- | Whether a request the GOAWAY came after, and one made after it, each
-- ended in 'QUIC.ConnectionIsClosed'.  The first used to wait for ever.
ignoredRequests :: IO (Bool, Bool)
ignoredRequests = QUICC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
            let get = E.try $ sendRequest (C.requestNoBody methodGet "/" []) $ \_ -> return ()
                gone (Left (QUIC.ConnectionIsClosed _)) = True
                gone _ = False
            r1 <- get
            r2 <- get
            return (gone r1, gone r2)
