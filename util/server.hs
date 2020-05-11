{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.List as L
import qualified Network.HTTP.Types as H
import Network.HTTP2.Server hiding (run)
import Network.HTTP3.Server
import Network.QUIC
import qualified Network.TLS.SessionManager as SM
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO

import Common

data Options = Options {
    optDebugLogDir :: Maybe FilePath
  , optQLogDir     :: Maybe FilePath
  , optKeyLogFile  :: Maybe FilePath
  , optGroups      :: Maybe String
  , optCertFile    :: FilePath
  , optKeyFile     :: FilePath
  , optRetry       :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions = Options {
    optDebugLogDir = Nothing
  , optQLogDir     = Nothing
  , optKeyLogFile  = Nothing
  , optGroups      = Nothing
  , optCertFile    = "servercert.pem"
  , optKeyFile     = "serverkey.pem"
  , optRetry       = False
  }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['d'] ["debug-log-dir"]
    (ReqArg (\dir o -> o { optDebugLogDir = Just dir }) "<dir>")
    "directory to store a debug file"
  , Option ['q'] ["qlog-dir"]
    (ReqArg (\dir o -> o { optQLogDir = Just dir }) "<dir>")
    "directory to store qlog"
  , Option ['l'] ["key-log-file"]
    (ReqArg (\file o -> o { optKeyLogFile = Just file }) "<file>")
    "a file to store negotiated secrets"
  , Option ['g'] ["groups"]
    (ReqArg (\gs o -> o { optGroups = Just gs }) "<groups>")
    "groups for key exchange"
  , Option ['c'] ["cert"]
    (ReqArg (\fl o -> o { optCertFile = fl }) "<file>")
    "certificate file"
  , Option ['k'] ["key"]
    (ReqArg (\fl o -> o { optKeyFile = fl }) "<file>")
    "key file"
  , Option ['S'] ["retry"]
    (NoArg (\o -> o { optRetry = True }))
    "require stateless retry"
  ]

usage :: String
usage = "Usage: server [OPTION] addr [addrs] port"

showUsageAndExit :: String -> IO a
showUsageAndExit msg = do
    putStrLn msg
    putStrLn $ usageInfo usage options
    exitFailure

serverOpts :: [String] -> IO (Options, [String])
serverOpts argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> showUsageAndExit $ concat errs

chooseALPN :: Version -> [ByteString] -> IO ByteString
chooseALPN ver protos = return $ case mh3idx of
    Nothing    -> case mhqidx of
      Nothing    -> ""
      Just _     -> hqX
    Just h3idx ->  case mhqidx of
      Nothing    -> h3X
      Just hqidx -> if h3idx < hqidx then h3X else hqX
  where
    (h3X, hqX) = makeProtos ver
    mh3idx = h3X `L.elemIndex` protos
    mhqidx = hqX `L.elemIndex` protos

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    (Options{..}, ips) <- serverOpts args
    when (length ips < 2) $ showUsageAndExit "cannot recognize <addr> and <port>\n"
    let port = read (last ips)
        addrs = read <$> init ips
        aps = (,port) <$> addrs
    smgr <- SM.newSessionManager SM.defaultConfig
    let conf = defaultServerConfig {
            scAddresses    = aps
          , scKey          = optKeyFile
          , scCert         = optCertFile
          , scALPN         = Just chooseALPN
          , scRequireRetry = optRetry
          , scSessionManager = smgr
          , scEarlyDataSize  = 1024
          , scConfig     = defaultConfig {
                confParameters = exampleParameters
              , confKeyLog     = getLogger optKeyLogFile
              , confGroups     = getGroups optGroups
              , confDebugLog   = getDirLogger optDebugLogDir ".txt"
              , confQLog       = getDirLogger optQLogDir ".qlog"
              }
          }
    runQUICServer conf $ \conn -> do
        info <- getConnectionInfo conn
        let server = case alpn info of
              Just proto | "hq" `BS.isPrefixOf` proto -> serverHQ
              _                                       -> serverH3
        server conn

onE :: IO b -> IO a -> IO a
h `onE` b = b `E.onException` h

serverHQ :: Connection -> IO ()
serverHQ conn = connDebugLog conn "Connection terminated" `onE` loop
  where
    loop = do
        es <- acceptStream conn
        case es of
          Left  e -> print e
          Right s -> do
              bs <- recvStream s 1024
              sendStream s html
              shutdownStream s
              if bs == "" then
                  connDebugLog conn "Connection finished"
                else do
                  connDebugLog conn $ C8.unpack bs
                  loop

html :: ByteString
html = "<html><head><title>Welcome to QUIC in Haskell</title></head><body><p>Welcome to QUIC in Haskell.</p></body></html>"

serverH3 :: Connection -> IO ()
serverH3 conn = run conn $ \_req _aux sendResponse -> do
    let hdr = [ ("Content-Type", "text/html; charset=utf-8")
              , ("Server", "HaskellQuic/0.0.0")
              ]
        rsp = responseBuilder H.ok200 hdr "Hello, world!"
    sendResponse rsp []
