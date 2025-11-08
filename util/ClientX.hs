{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module ClientX (
    Misc (..),
    Cli,
    clientHQ,
    clientH3,
) where

import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.QUIC

import qualified Network.HQ.Client as HQ
import Network.HTTP3.Client (
    Client,
    ClientConfig (..),
    Config,
    Path,
    SendRequest,
 )
import qualified Network.HTTP3.Client as H3

data Misc = Misc
    { miscAuthority :: String
    , miscDebug :: String -> IO ()
    , miscShow :: ByteString -> IO ()
    , miscCheckClose :: IO Bool
    }

type Cli = Misc -> [Path] -> Connection -> IO ()

clientHQ :: Int -> Cli
clientHQ n = clientX n HQ.run

clientH3 :: Int -> Cli
clientH3 n = clientX n H3.run

clientX
    :: Int
    -> (Connection -> ClientConfig -> Config -> Client () -> IO ())
    -> Cli
clientX n0 run misc@Misc{..} paths conn = E.bracket H3.allocSimpleConfig H3.freeSimpleConfig $ \conf ->
    run conn cliconf conf $ client misc n0 paths
  where
    cliconf =
        H3.defaultClientConfig
            { authority = miscAuthority
            }

client :: Misc -> Int -> [Path] -> Client ()
client misc n0 paths sendRequest _misc =
    foldr1 concurrently_ $
        map (client' misc n0 sendRequest) paths

client' :: Misc -> Int -> SendRequest -> Path -> IO ()
client' Misc{..} n0 sendRequest path = loop n0
  where
    req =
        H3.requestNoBody
            methodGet
            path
            [("User-Agent", "HaskellQuic/0.0.0")]
    loop 0 = return ()
    loop n = do
        () <- sendRequest req $ \rsp -> do
            miscDebug "GET"
            miscShow "------------------------"
            consume rsp
            miscShow "------------------------"
        when (n /= 1) $ do
            threadDelay 100000
            loop (n - 1)
    consume rsp = do
        bs <- H3.getResponseBodyChunk rsp
        if bs == ""
            then do
                miscDebug "Fin received"
            else do
                miscShow bs
                miscDebug $ show (C8.length bs) ++ " bytes received"
                consume rsp
