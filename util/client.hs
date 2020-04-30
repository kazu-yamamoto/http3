{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Types
import Network.HTTP3.Client
import Network.QUIC
import Network.TLS.Extra.Cipher
import Network.TLS.QUIC
import System.Console.GetOpt
import System.Environment
import System.Exit

import Common

data Options = Options {
    optDebugLog   :: Bool
  , optQLogDir    :: Maybe FilePath
  , optKeyLogFile :: Maybe FilePath
  , optGroups     :: Maybe String
  , optValidate   :: Bool
  , optHQ         :: Bool
  , optVerNego    :: Bool
  , optResumption :: Bool
  , opt0RTT       :: Bool
  , optRetry      :: Bool
  , optQuantum    :: Bool
  , optMigration  :: Maybe Migration
  } deriving Show

defaultOptions :: Options
defaultOptions = Options {
    optDebugLog   = False
  , optQLogDir    = Nothing
  , optKeyLogFile = Nothing
  , optGroups     = Nothing
  , optHQ         = False
  , optValidate   = False
  , optVerNego    = False
  , optResumption = False
  , opt0RTT       = False
  , optRetry      = False
  , optQuantum    = False
  , optMigration  = Nothing
  }

usage :: String
usage = "Usage: client [OPTION] addr port"

options :: [OptDescr (Options -> Options)]
options = [
    Option ['d'] ["debug"]
    (NoArg (\o -> o { optDebugLog = True }))
    "print debug info"
  , Option ['q'] ["qlog-dir"]
    (ReqArg (\dir o -> o { optQLogDir = Just dir }) "<dir>")
    "directory to store qlog"
  , Option ['l'] ["key-log-file"]
    (ReqArg (\file o -> o { optKeyLogFile = Just file }) "<file>")
    "a file to store negotiated secrets"
  , Option ['g'] ["groups"]
    (ReqArg (\gs o -> o { optGroups = Just gs }) "<groups>")
    "specify groups"
  , Option ['c'] ["validate"]
    (NoArg (\o -> o { optValidate = True }))
    "validate server's certificate"
  , Option ['r'] ["hq"]
    (NoArg (\o -> o { optHQ = True }))
    "prefer hq (HTTP/0.9)"
  , Option ['V'] ["vernego"]
    (NoArg (\o -> o { optVerNego = True }))
    "try version negotiation"
  , Option ['R'] ["resumption"]
    (NoArg (\o -> o { optResumption = True }))
    "try session resumption"
  , Option ['Z'] ["0rtt"]
    (NoArg (\o -> o { opt0RTT = True }))
    "try sending early data"
  , Option ['S'] ["stateless-retry"]
    (NoArg (\o -> o { optRetry = True }))
    "check stateless retry"
  , Option ['Q'] ["quantum"]
    (NoArg (\o -> o { optQuantum = True }))
    "try sending large Initials"
  , Option ['M'] ["change-server-cid"]
    (NoArg (\o -> o { optMigration = Just ChangeServerCID }))
    "use a new server CID"
  , Option ['N'] ["change-client-cid"]
    (NoArg (\o -> o { optMigration = Just ChangeClientCID }))
    "use a new client CID"
  , Option ['B'] ["nat-rebinding"]
    (NoArg (\o -> o { optMigration = Just NATRebiding }))
    "use a new local port"
  , Option ['A'] ["address-mobility"]
    (NoArg (\o -> o { optMigration = Just MigrateTo }))
    "use a new address and a new server CID"
  ]

showUsageAndExit :: String -> IO a
showUsageAndExit msg = do
    putStrLn msg
    putStrLn $ usageInfo usage options
    exitFailure

clientOpts :: [String] -> IO (Options, [String])
clientOpts argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> showUsageAndExit $ concat errs

main :: IO ()
main = do
    args <- getArgs
    (opts@Options{..}, ips) <- clientOpts args
    let ipslen = length ips
    when (ipslen /= 2 && ipslen /= 3) $
        showUsageAndExit "cannot recognize <addr> and <port>\n"
    let path | ipslen == 3 = "/" ++ (ips !! 2)
             | otherwise   = "/"
        cmd = C8.pack ("GET " ++ path ++ "\r\n")
        addr:port:_ = ips
        conf = defaultClientConfig {
            ccServerName = addr
          , ccPortName   = port
          , ccALPN       = \ver -> let (h3X, hqX) = makeProtos ver
                                       protos
                                         | optHQ     = [hqX,h3X]
                                         | otherwise = [h3X,hqX]
                                   in return $ Just protos
          , ccValidate   = optValidate
          , ccConfig     = defaultConfig {
                confVersions   = if optVerNego then
                                   GreasingVersion : confVersions defaultConfig
                                 else
                                   confVersions defaultConfig
              , confParameters = if optQuantum then
                                   exampleParameters {
                                       greaseParameter = Just (BS.pack (replicate 1200 0))
                                     }
                                 else
                                   exampleParameters
              , confKeyLog     = getLogger optKeyLogFile
              , confGroups     = getGroups optGroups
              , confCiphers    = [ cipher_TLS13_AES256GCM_SHA384
                                 , cipher_TLS13_AES128GCM_SHA256
                                 , cipher_TLS13_AES128CCM_SHA256
                                 ]
              , confDebugLog   = getStdoutLogger optDebugLog
              , confQLog       = getDirLogger optQLogDir ".qlog"
              }
          }
        debug | optDebugLog = putStrLn
              | otherwise   = \_ -> return ()
    runClient conf opts cmd addr debug

runClient :: ClientConfig -> Options -> ByteString -> String -> (String -> IO ()) -> IO ()
runClient conf opts@Options{..} cmd addr debug = do
    debug "------------------------"
    (info1,info2,res,mig) <- runQUICClient conf $ \conn -> do
        i1 <- getConnectionInfo conn
        let client = case alpn i1 of
              Just proto | "hq" `BS.isPrefixOf` proto -> clientHQ cmd
              _                                       -> clientH3 addr
        m <- case optMigration of
          Nothing   -> return False
          Just mtyp -> do
              debug $ "Migration by " ++ show mtyp
              migration conn mtyp
        debug "------------------------"
        client conn debug
        debug "\n------------------------"
        i2 <- getConnectionInfo conn
        r <- getResumptionInfo conn
        return (i1, i2, r, m)
    if optVerNego then do
        putStrLn "Result: (V) version negotiation  ... OK"
        exitSuccess
      else if optQuantum then do
        putStrLn "Result: (Q) quantum ... OK"
        exitSuccess
      else if optResumption then do
        if isResumptionPossible res then do
            info3 <- runClient2 conf opts cmd addr debug res
            if handshakeMode info3 == PreSharedKey then do
                putStrLn "Result: (R) TLS resumption ... OK"
                exitSuccess
              else do
                putStrLn "Result: (R) TLS resumption ... NG"
                exitFailure
          else do
            putStrLn "Result: (R) TLS resumption ... NG"
            exitFailure
      else if opt0RTT then do
        if is0RTTPossible res then do
            info3 <- runClient2 conf opts cmd addr debug res
            if handshakeMode info3 == RTT0 then do
                putStrLn "Result: (Z) 0-RTT ... OK"
                exitSuccess
              else do
                putStrLn "Result: (Z) 0-RTT ... NG"
                exitFailure
          else do
            putStrLn "Result: (Z) 0-RTT ... NG"
            exitFailure
      else if optRetry then do
        if retry info1 then do
            putStrLn "Result: (S) retry ... OK"
            exitSuccess
          else do
            putStrLn "Result: (S) retry ... NG"
            exitFailure
      else case optMigration of
             Just ChangeServerCID -> do
                 let changed = remoteCID info1 /= remoteCID info2
                 if mig && remoteCID info1 /= remoteCID info2 then do
                     putStrLn "Result: (M) change server CID ... OK"
                     exitSuccess
                   else do
                     putStrLn $ "Result: (M) change server CID ... NG " ++ show (mig,changed)
                     exitFailure
             Just ChangeClientCID -> do
                 let changed = localCID info1 /= localCID info2
                 if mig && changed then do
                     putStrLn "Result: (N) change client CID ... OK"
                     exitSuccess
                   else do
                     putStrLn $ "Result: (N) change client CID ... NG " ++ show (mig,changed)
                     exitFailure
             Just NATRebiding -> do
                 putStrLn "Result: (B) NAT rebinding ... OK"
                 exitSuccess
             Just MigrateTo -> do
                 let changed = remoteCID info1 /= remoteCID info2
                 if mig && changed then do
                     putStrLn "Result: (A) address mobility ... OK"
                     exitSuccess
                   else do
                     putStrLn $ "Result: (A) address mobility ... NG " ++ show (mig,changed)
                     exitFailure
             Nothing -> do
                 putStrLn "Result: (H) handshake ... OK"
                 putStrLn "Result: (D) stream data ... OK"
                 exitSuccess

runClient2 :: ClientConfig -> Options -> ByteString -> String -> (String -> IO ()) -> ResumptionInfo -> IO ConnectionInfo
runClient2 conf Options{..} cmd addr debug res = do
    threadDelay 100000
    debug "<<<< next connection >>>>"
    debug "------------------------"
    runQUICClient conf' $ \conn -> do
        info <- getConnectionInfo conn
        if rtt0 then do
            debug "------------------------ Response for early data"
            es0 <- acceptStream conn
            case es0 of
              Right s0 -> do
                  bs <- recvStream s0 1024
                  debug $ "SID: " ++ show (streamId s0)
                  debug $ show $ BS.unpack bs
                  debug "------------------------ Response for early data"
              _ -> return ()
          else do
            let client = case alpn info of
                  Just proto | "hq" `BS.isPrefixOf` proto -> clientHQ cmd
                  _                                       -> clientH3 addr
            void $ client conn debug
        return info
  where
    rtt0 = opt0RTT && is0RTTPossible res
    conf' | rtt0 = conf {
                ccResumption = res
              , ccEarlyData  = Just cmd
              }
          | otherwise = conf { ccResumption = res }

clientHQ :: ByteString -> Connection -> (String -> IO ()) -> IO ()
clientHQ cmd conn debug = do
    s <- stream conn
    sendStream s cmd
    shutdownStream s
    loop s
  where
    loop s = do
        bs <- recvStream s 1024
        if bs == "" then
            debug "Connection finished"
          else do
            debug $ C8.unpack bs
            loop s

clientH3 :: String -> Connection -> (String -> IO ()) -> IO ()
clientH3 authority conn debug = run conn "http" auth client
  where
    auth = C8.pack authority
    client sendRequest = do
        let req = requestNoBody methodGet "/" [("User-Agent", "HaskellQuic/0.0.0")]
        sendRequest req $ \rsp -> do
            debug $ show rsp
            getResponseBodyChunk rsp >>= C8.putStrLn
