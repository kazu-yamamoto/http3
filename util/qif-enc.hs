{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString as BS
import Data.Serialize.Put
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.FilePath
import System.IO

import Network.QPACK

import QIF

data Options = Options
    { optDebug :: Bool
    }

defaultOptions :: Options
defaultOptions =
    Options
        { optDebug = False
        }

options :: [OptDescr (Options -> Options)]
options =
    [ Option
        ['d']
        ["debug"]
        (NoArg (\o -> o{optDebug = True}))
        "dump encode instructions"
    ]

usage :: String
usage = "Usage: qif-enc <qif-file> <capacity> <blocked> <ack>"

showUsageAndExit :: String -> IO a
showUsageAndExit msg = do
    putStrLn msg
    putStrLn $ usageInfo usage options
    exitFailure

encoderOpts :: [String] -> IO (Options, [String])
encoderOpts argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
        (_, _, errs) -> showUsageAndExit $ concat errs

main :: IO ()
main = do
    args <- getArgs
    (Options{..}, spec) <- encoderOpts args
    case spec of
        [qfile, cap, blk, ack] -> do
            let efile = takeBaseName qfile ++ ".out." ++ cap ++ "." ++ blk ++ "." ++ ack
                capacity = read cap
                blocked = read blk
                immack = ack == "1"
            encode qfile efile capacity blocked immack optDebug
        _ -> showUsageAndExit "Illegal arguments"

encode :: FilePath -> FilePath -> Int -> Int -> Bool -> Bool -> IO ()
encode qfile efile capacity blocked immack debug = do
    let encConf = defaultQEncoderConfig{ecMaxTableCapacity = capacity}
        save sid bs = do
            let header = runPut $ do
                    putWord64be $ fromIntegral (sid :: Int)
                    putWord32be $ fromIntegral $ BS.length bs
            BS.appendFile efile $ header <> bs
    enc <- newQEncoderS encConf (save 0) blocked immack debug
    withFile qfile ReadMode $ qencode enc save

qencode :: QEncoderS -> (Int -> BS.ByteString -> IO ()) -> Handle -> IO ()
qencode enc save hdl = loop 1
  where
    loop sid = do
        hs <- headerlist hdl
        if null hs
            then return ()
            else do
                bs <- enc sid hs
                save sid bs
                loop (sid + 1)
