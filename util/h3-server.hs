{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Network.TLS (Credentials (..), credentialLoadX509)
import qualified Network.TLS.SessionManager as SM
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
#ifndef mingw32_HOST_OS
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)
#endif
import Text.Read (readMaybe)

import Common
import Network.QUIC
import Network.QUIC.Internal
import Network.QUIC.Server
import ServerX

data Options = Options
    { optDebugLogDir :: Maybe FilePath
    , optQLogDir :: Maybe FilePath
    , optKeyLogFile :: Maybe FilePath
    , optGroups :: Maybe String
    , optCertFile :: FilePath
    , optKeyFile :: FilePath
    , optRetry :: Bool
    }
    deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options
        { optDebugLogDir = Nothing
        , optQLogDir = Nothing
        , optKeyLogFile = Nothing
        , optGroups = Nothing
        , optCertFile = "servercert.pem"
        , optKeyFile = "serverkey.pem"
        , optRetry = False
        }

options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['d']
        ["debug-log-dir"]
        (ReqArg (\dir o -> o{optDebugLogDir = Just dir}) "<dir>")
        "directory to store a debug file"
    , Option
        ['q']
        ["qlog-dir"]
        (ReqArg (\dir o -> o{optQLogDir = Just dir}) "<dir>")
        "directory to store qlog"
    , Option
        ['l']
        ["key-log-file"]
        (ReqArg (\file o -> o{optKeyLogFile = Just file}) "<file>")
        "a file to store negotiated secrets"
    , Option
        ['g']
        ["groups"]
        (ReqArg (\gs o -> o{optGroups = Just gs}) "<groups>")
        "groups for key exchange"
    , Option
        ['c']
        ["cert"]
        (ReqArg (\fl o -> o{optCertFile = fl}) "<file>")
        "certificate file"
    , Option
        ['k']
        ["key"]
        (ReqArg (\fl o -> o{optKeyFile = fl}) "<file>")
        "key file"
    , Option
        ['S']
        ["retry"]
        (NoArg (\o -> o{optRetry = True}))
        "require stateless retry"
    ]

usage :: String
usage = "Usage: h3-server [OPTION] addr [addrs] port"

showUsageAndExit :: String -> IO a
showUsageAndExit msg = do
    putStrLn msg
    putStrLn $ usageInfo usage options
    exitFailure

serverOpts :: [String] -> IO (Options, [String])
serverOpts argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> showUsageAndExit $ concat errs

chooseALPN :: Version -> [ByteString] -> IO ByteString
chooseALPN ver protos = return $ case mh3idx of
    Nothing -> case mhqidx of
        Nothing -> ""
        Just _ -> hqX
    Just h3idx -> case mhqidx of
        Nothing -> h3X
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
    aps <- case (,) <$> mapM readMaybe (init ips) <*> readMaybe (last ips) of
        Nothing -> showUsageAndExit "cannot parse <addr> or <port>\n"
        Just (addrs, port) -> return $ (,port) <$> addrs
    smgr <- SM.newSessionManager SM.defaultConfig
    draining <- newTVarIO False
    active <- newTVarIO (0 :: Int)
    let loadCred =
            credentialLoadX509 optCertFile optKeyFile
                `E.catch` \e -> return $ Left $ show (e :: E.IOException)
    cred@(!_cc, !_priv) <-
        loadCred >>= either (showUsageAndExit . ("cannot load credentials: " ++)) return
    let sc0 = defaultServerConfig
        sc =
            sc0
                { scAddresses = aps
                , scALPN = Just chooseALPN
                , scRequireRetry = optRetry
                , scSessionManager = smgr
                , scUse0RTT = True
                , scDebugLog = optDebugLogDir
                , scKeyLog = getLogger optKeyLogFile
                , scGroups = getGroups (scGroups sc0) optGroups
                , scQLog = optQLogDir
                , scCredentials = Credentials [cred]
                , -- Room for unidirectional streams beyond the three an HTTP/3
                  -- client needs.  quic's default is exactly three, and then
                  -- h3spec cannot open the second control or QPACK stream it
                  -- needs to see this server refuse one; those cases were
                  -- left pending.  RFC 9114, section 6.2, suggests allowing
                  -- more anyway, for extensions and reserved stream types.
                  scParameters =
                    (scParameters sc0){initialMaxStreamsUni = 10}
                , -- H3_NO_ERROR, for what is still open once the drain is over.
                  scCloseReason = (ApplicationProtocolError 0x100, "server is closing")
                , scInstallShutdownHandler = onSignals . drainThen draining active
                }
    run sc $ \conn -> counted active $ do
        info <- getConnectionInfo conn
        let server = case alpn info of
                Just proto | "hq" `BS.isPrefixOf` proto -> serverHQ
                _ -> serverH3 $ readTVar draining >>= check
        server conn

-- | Running a connection, counted while it runs.
counted :: TVar Int -> IO a -> IO a
counted active =
    E.bracket_
        (atomically $ modifyTVar' active (+ 1))
        (atomically $ modifyTVar' active (subtract 1))

-- | Draining, then stopping: HTTP/3 connections are told GOAWAY and
--   answer what they have taken, for 'drainTime' at most.  The server is
--   then stopped, which closes what is left.
drainThen :: TVar Bool -> TVar Int -> IO () -> IO ()
drainThen draining active stopIt = do
    atomically $ writeTVar draining True
    expired <- registerDelay drainTime
    atomically $
        (readTVar active >>= check . (== 0)) `orElse` (readTVar expired >>= check)
    stopIt

-- | How long a stopping server waits for its connections (microseconds).
drainTime :: Int
drainTime = 5000000

-- | Draining on SIGINT or SIGTERM, on a thread of its own, since a signal
--   handler should return.  On Windows, Ctrl-C interrupts 'run' instead.
onSignals :: IO () -> IO ()
#ifndef mingw32_HOST_OS
onSignals act =
    forM_ [sigINT, sigTERM] $ \sig ->
        installHandler sig (CatchOnce $ void $ forkIO act) Nothing
#else
onSignals _ = return ()
#endif
