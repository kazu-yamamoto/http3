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
import System.IO

import qualified Network.HQ.Client as HQ
import Network.HTTP3.Client (
    Client,
    ClientConfig (..),
    Config,
    Path,
    SendRequest,
 )
import qualified Network.HTTP3.Client as H3
import qualified Network.QUIC.Internal as QUIC

data Misc = Misc
    { miscAuthority :: String
    , miscDebug :: String -> IO ()
    , miscShow :: ByteString -> IO ()
    , miscCheckClose :: IO Bool
    , miscInteractive :: Bool
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
    run conn cliconf conf $ client conn misc n0 paths
  where
    cliconf =
        H3.defaultClientConfig
            { authority = miscAuthority
            }

client :: Connection -> Misc -> Int -> [Path] -> Client ()
client conn misc n0 paths sendRequest aux
    | miscInteractive misc = do
        console paths aux conn go
        return ()
    | otherwise = go
  where
    go =
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

console :: [Path] -> H3.Aux -> Connection -> IO () -> IO ()
console paths aux conn go = do
    waitEstablished conn
    putStrLn "q -- quit"
    putStrLn "g -- get"
    putStrLn "p -- ping"
    putStrLn "n -- NAT rebinding"
    mvar <- newEmptyMVar
    loop mvar `E.catch` \(E.SomeException _) -> return ()
  where
    loop mvar = do
        hSetBuffering stdout NoBuffering
        putStr "> "
        hSetBuffering stdout LineBuffering
        l <- getLine
        case l of
            "q" -> putStrLn "bye"
            "g" -> do
                mapM_ (\p -> putStrLn $ "GET " ++ C8.unpack p) paths
                _ <- go >> putMVar mvar ()
                takeMVar mvar
                loop mvar
            "p" -> do
                putStrLn "Ping"
                H3.auxSendPing aux
                loop mvar
            "n" -> do
                QUIC.controlConnection conn QUIC.NATRebinding >>= print
                loop mvar
            _ -> do
                putStrLn "No such command"
                loop mvar
