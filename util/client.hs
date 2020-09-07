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
import Data.Char
import Data.IORef
import Data.UnixTime
import Foreign.C.Types
import Network.HTTP.Types
import Network.HTTP3.Client
import qualified Network.QUIC as QUIC
import Network.TLS.QUIC
import System.Console.GetOpt
import System.Environment
import System.Exit
import Text.Printf

import Common

data Options = Options {
    optDebugLog   :: Bool
  , optShow       :: Bool
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
  , optThroughput :: Bool
  , optMigration  :: Maybe QUIC.Migration
  , optPacketSize :: Maybe Int
  } deriving Show

defaultOptions :: Options
defaultOptions = Options {
    optDebugLog   = False
  , optShow       = False
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
  , optThroughput = False
  , optMigration  = Nothing
  , optPacketSize = Nothing
  }

usage :: String
usage = "Usage: client [OPTION] addr port"

options :: [OptDescr (Options -> Options)]
options = [
    Option ['d'] ["debug"]
    (NoArg (\o -> o { optDebugLog = True }))
    "print debug info"
  , Option ['v'] ["show-content"]
    (NoArg (\o -> o { optShow = True }))
    "print downloaded content"
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
  , Option ['s'] ["packet-size"]
    (ReqArg (\n o -> o { optPacketSize = Just (read n) }) "<int>")
    "specify QUIC packet size (UDP payload size)"
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
    (NoArg (\o -> o { optMigration = Just QUIC.ChangeServerCID }))
    "use a new server CID"
  , Option ['N'] ["change-client-cid"]
    (NoArg (\o -> o { optMigration = Just QUIC.ChangeClientCID }))
    "use a new client CID"
  , Option ['B'] ["nat-rebinding"]
    (NoArg (\o -> o { optMigration = Just QUIC.NATRebiding }))
    "use a new local port"
  , Option ['A'] ["address-mobility"]
    (NoArg (\o -> o { optMigration = Just QUIC.MigrateTo }))
    "use a new address and a new server CID"
  , Option ['T'] ["throughput"]
    (NoArg (\o -> o { optThroughput = True }))
    "try measuring throughput. path must be specified"
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

data Aux = Aux {
    auxPath :: String
  , auxAuthority :: String
  , auxDebug :: String -> IO ()
  , auxShow :: ByteString -> IO ()
  , auxCheckClose :: IO Bool
  }

type Cli = Aux -> QUIC.Connection -> IO ()

main :: IO ()
main = do
    args <- getArgs
    (opts@Options{..}, ips) <- clientOpts args
    let ipslen = length ips
    when (ipslen /= 2 && ipslen /= 3) $
        showUsageAndExit "cannot recognize <addr> and <port>\n"
    cref <- newIORef False
    let path | ipslen == 3 = ips !! 2
             | otherwise   = "/"
        addr:port:_ = ips
        conf = QUIC.defaultClientConfig {
            QUIC.ccServerName = addr
          , QUIC.ccPortName   = port
          , QUIC.ccALPN       = \ver -> let (h3X, hqX) = makeProtos ver
                                            protos
                                              | optHQ     = [hqX,h3X]
                                              | otherwise = [h3X,hqX]
                                        in return $ Just protos
          , QUIC.ccValidate   = optValidate
          , QUIC.ccPacketSize = optPacketSize
          , QUIC.ccDebugLog   = optDebugLog
          , QUIC.ccConfig     = QUIC.defaultConfig {
                QUIC.confVersions   = if optVerNego then
                                        QUIC.GreasingVersion : QUIC.confVersions QUIC.defaultConfig
                                      else
                                        QUIC.confVersions QUIC.defaultConfig
              , QUIC.confParameters = if optQuantum then
                                        QUIC.defaultParameters {
                                            QUIC.greaseParameter = Just (BS.pack (replicate 1200 0))
                                        }
                                 else
                                   QUIC.defaultParameters
              , QUIC.confKeyLog     = getLogger optKeyLogFile
              , QUIC.confGroups     = getGroups optGroups
              , QUIC.confQLog       = optQLogDir
              , QUIC.confHooks      = QUIC.defaultHooks {
                    QUIC.onCloseReceived = writeIORef cref True
                  }
              }
          }
        debug | optDebugLog = putStrLn
              | otherwise   = \_ -> return ()
        showContent | optShow = C8.putStrLn
                    | otherwise = \_ -> return ()
        aux = Aux {
            auxPath = path
          , auxAuthority = addr
          , auxDebug = debug
          , auxShow = showContent
          , auxCheckClose = readIORef cref
          }
    runClient conf opts aux

runClient :: QUIC.ClientConfig -> Options -> Aux -> IO ()
runClient conf opts@Options{..} aux@Aux{..} = do
    auxDebug "------------------------"
    (info1,info2,res,mig,client') <- QUIC.runQUICClient conf $ \conn -> do
        i1 <- QUIC.getConnectionInfo conn
        let client = case QUIC.alpn i1 of
              Just proto | "hq" `BS.isPrefixOf` proto -> clientHQ
              _                                       -> clientH3
        m <- case optMigration of
          Nothing   -> return False
          Just mtyp -> do
              x <- QUIC.migration conn mtyp
              auxDebug $ "Migration by " ++ show mtyp
              return x
        t1 <- getUnixTime
        client aux conn
        t2 <- getUnixTime
        i2 <- QUIC.getConnectionInfo conn
        r <- QUIC.getResumptionInfo conn
        printThroughput t1 t2 aux
        return (i1, i2, r, m, client)
    if optVerNego then do
        putStrLn "Result: (V) version negotiation ... OK"
        exitSuccess
      else if optQuantum then do
        putStrLn "Result: (Q) quantum ... OK"
        exitSuccess
      else if optResumption then do
        if QUIC.isResumptionPossible res then do
            info3 <- runClient2 conf opts aux res client'
            if QUIC.handshakeMode info3 == PreSharedKey then do
                putStrLn "Result: (R) TLS resumption ... OK"
                exitSuccess
              else do
                putStrLn "Result: (R) TLS resumption ... NG"
                exitFailure
          else do
            putStrLn "Result: (R) TLS resumption ... NG"
            exitFailure
      else if opt0RTT then do
        if QUIC.is0RTTPossible res then do
            info3 <- runClient2 conf opts aux res client'
            if QUIC.handshakeMode info3 == RTT0 then do
                putStrLn "Result: (Z) 0-RTT ... OK"
                exitSuccess
              else do
                putStrLn "Result: (Z) 0-RTT ... NG"
                exitFailure
          else do
            putStrLn "Result: (Z) 0-RTT ... NG"
            exitFailure
      else if optRetry then do
        if QUIC.retry info1 then do
            putStrLn "Result: (S) retry ... OK"
            exitSuccess
          else do
            putStrLn "Result: (S) retry ... NG"
            exitFailure
      else case optMigration of
             Just QUIC.ChangeServerCID -> do
                 let changed = QUIC.remoteCID info1 /= QUIC.remoteCID info2
                 if mig && QUIC.remoteCID info1 /= QUIC.remoteCID info2 then do
                     putStrLn "Result: (M) change server CID ... OK"
                     exitSuccess
                   else do
                     putStrLn $ "Result: (M) change server CID ... NG " ++ show (mig,changed)
                     exitFailure
             Just QUIC.ChangeClientCID -> do
                 let changed = QUIC.localCID info1 /= QUIC.localCID info2
                 if mig && changed then do
                     putStrLn "Result: (N) change client CID ... OK"
                     exitSuccess
                   else do
                     putStrLn $ "Result: (N) change client CID ... NG " ++ show (mig,changed)
                     exitFailure
             Just QUIC.NATRebiding -> do
                 putStrLn "Result: (B) NAT rebinding ... OK"
                 exitSuccess
             Just QUIC.MigrateTo -> do
                 let changed = QUIC.remoteCID info1 /= QUIC.remoteCID info2
                 if mig && changed then do
                     putStrLn "Result: (A) address mobility ... OK"
                     exitSuccess
                   else do
                     putStrLn $ "Result: (A) address mobility ... NG " ++ show (mig,changed)
                     exitFailure
             Nothing -> do
                 putStrLn "Result: (H) handshake ... OK"
                 putStrLn "Result: (D) stream data ... OK"
                 closeReceived <- auxCheckClose
                 when closeReceived $ putStrLn "Result: (C) close received ... OK"
                 case QUIC.alpn info1 of
                   Nothing   -> return ()
                   Just alpn -> when ("h3" `BS.isPrefixOf` alpn) $
                     putStrLn "Result: (3) H3 transaction ... OK"
                 exitSuccess

runClient2 :: QUIC.ClientConfig -> Options -> Aux -> QUIC.ResumptionInfo -> Cli
           -> IO QUIC.ConnectionInfo
runClient2 conf Options{..} aux@Aux{..} res client = do
    threadDelay 100000
    auxDebug "<<<< next connection >>>>"
    auxDebug "------------------------"
    QUIC.runQUICClient conf' $ \conn -> do
        void $ client aux conn
        QUIC.getConnectionInfo conn
  where
    conf' = conf {
        QUIC.ccResumption = res
      , QUIC.ccUse0RTT    = opt0RTT && QUIC.is0RTTPossible res
      }

clientHQ :: Cli
clientHQ Aux{..} conn = do
    let cmd = C8.pack ("GET " ++ auxPath ++ "\r\n")
    s <- QUIC.stream conn
    QUIC.sendStream s cmd
    QUIC.shutdownStream s
    loop s
  where
    loop s = do
        bs <- QUIC.recvStream s 1024
        if bs == "" then do
            auxDebug "Connection finished"
            QUIC.closeStream conn s
            QUIC.getConnectionStats conn >>= print
          else do
            auxShow bs
            loop s

clientH3 :: Cli
clientH3 Aux{..} conn = run conn cliconf defaultConfig client
  where
    cliconf = ClientConfig {
        scheme = "https"
      , authority = C8.pack auxAuthority
      }
    client sendRequest = do
        let req = requestNoBody methodGet (C8.pack auxPath) [("User-Agent", "HaskellQuic/0.0.0")]
        _ <- sendRequest req $ \rsp -> do
            auxShow "------------------------"
            loop rsp
            auxShow "------------------------"
        QUIC.getConnectionStats conn >>= print
    loop rsp = do
        x <- getResponseBodyChunk rsp
        auxShow x
        when (x /= "") $ loop rsp

printThroughput :: UnixTime -> UnixTime -> Aux -> IO ()
printThroughput t1 t2 Aux{..}
  | amount /= 0 = printf "Throughput %.2f MB/s (%d bytes in %d msecs)\n" bytesPerSeconds amount millisecs
  | otherwise   = return ()
  where
    UnixDiffTime (CTime s) u = t2 `diffUnixTime` t1
    millisecs :: Int
    millisecs = fromIntegral s * 1000 + fromIntegral u `div` 1000
    strnum = reverse $ takeWhile isNumber $ reverse auxPath
    amount :: Int
    amount = case strnum of
      [] -> 0
      _  -> read strnum
    bytesPerSeconds :: Double
    bytesPerSeconds = fromIntegral amount * (1000 :: Double) / fromIntegral millisecs / 1024 / 1024
