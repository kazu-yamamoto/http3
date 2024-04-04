{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module ClientX (
    Aux (..),
    Cli,
    clientHQ,
    clientH3,
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.QUIC
import qualified UnliftIO.Exception as E

import qualified Network.HQ.Client as HQ
import qualified Network.HTTP3.Client as H3

data Aux = Aux
    { auxAuthority :: String
    , auxDebug :: String -> IO ()
    , auxShow :: ByteString -> IO ()
    , auxCheckClose :: IO Bool
    }

type Cli = Aux -> [H3.Path] -> Connection -> IO ()

clientHQ :: Int -> Cli
clientHQ n = clientX n HQ.run

clientH3 :: Int -> Cli
clientH3 n = clientX n H3.run

clientX
    :: Int
    -> (Connection -> H3.ClientConfig -> H3.Config -> H3.Client () -> IO ())
    -> Cli
clientX n0 run aux@Aux{..} paths conn = E.bracket H3.allocSimpleConfig H3.freeSimpleConfig $ \conf ->
    run conn cliconf conf $ client n0 aux paths
  where
    cliconf =
        H3.ClientConfig
            { scheme = "https"
            , authority = auxAuthority
            }

client :: Int -> Aux -> [H3.Path] -> H3.SendRequest -> H3.Aux -> IO ()
client n0 aux paths sendRequest _aux =
    foldr1 concurrently_ $
        map (client' n0 aux sendRequest) paths

client' :: Int -> Aux -> H3.SendRequest -> H3.Path -> IO ()
client' n0 Aux{..} sendRequest path = loop n0
  where
    req =
        H3.requestNoBody
            methodGet
            path
            [("User-Agent", "HaskellQuic/0.0.0")]
    loop 0 = return ()
    loop n = do
        () <- sendRequest req $ \rsp -> do
            auxDebug "GET"
            auxShow "------------------------"
            consume rsp
            auxShow "------------------------"
        when (n /= 1) $ do
            threadDelay 100000
            loop (n - 1)
    consume rsp = do
        bs <- H3.getResponseBodyChunk rsp
        if bs == ""
            then do
                auxDebug "Fin received"
            else do
                auxShow bs
                auxDebug $ show (C8.length bs) ++ " bytes received"
                consume rsp
