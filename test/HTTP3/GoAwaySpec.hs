{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTTP3.GoAwaySpec where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Network.HTTP.Types
import qualified Network.HTTP3.Client as C
import Network.HTTP3.Server
import qualified Network.QUIC.Client as QUICC
import qualified Network.QUIC.Server as QUICS
import System.Timeout (timeout)
import Test.Hspec

import HTTP3.Config
import HTTP3.Server (teardown)

spec :: Spec
spec =
    describe "H3 server going away" $
        -- RFC 9114, section 5.2: the request taken before GOAWAY is answered,
        -- one opened after it is rejected, and the connection then ends.
        it "answers the request it took and rejects a later one" $ do
            stop <- newTVarIO False
            returned <- newEmptyMVar
            E.bracket (setupGoAway stop returned) teardown $ \_ -> do
                timeout 5000000 (slowThenFast stop) `shouldReturn` Just (Just ok200, False)
                timeout 3000000 (takeMVar returned) `shouldReturn` Just ()

-- | A server going away once the variable is set; the 'MVar' is filled when
-- 'runWith' returns.  "/slow" is answered after 300 milliseconds.
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
            Just "/slow" -> threadDelay 300000
            _ -> return ()
        sendResponse (responseBuilder ok200 [] "hello") []

-- | A request for "/slow"; the server told to go away; then a request for
-- "/".  The status of the first, and whether the second succeeded.
slowThenFast :: TVar Bool -> IO (Maybe Status, Bool)
slowThenFast stop = QUICC.run testClientConfig $ \conn ->
    E.bracket allocSimpleConfig freeSimpleConfig $ \conf ->
        C.run conn testH3ClientConfig conf $ \sendRequest _aux -> do
            let get path = sendRequest (C.requestNoBody methodGet path []) $ return . C.responseStatus
            slow <- async $ get "/slow"
            threadDelay 100000
            atomically $ writeTVar stop True
            threadDelay 100000
            fast <- E.try $ get "/"
            s <- wait slow
            return (s, either (\(_ :: E.SomeException) -> False) (const True) fast)
