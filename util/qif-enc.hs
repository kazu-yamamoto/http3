module Main where

import qualified Data.ByteString as BS
import Data.Serialize.Put
import System.Environment
import System.FilePath
import System.IO

import Network.QPACK

import QIF

main :: IO ()
main = do
    args <- getArgs
    case args of
        [qfile, cap, blk, ack] -> do
            let efile = takeBaseName qfile ++ ".out." ++ cap ++ "." ++ blk ++ "." ++ ack
                capacity = read cap
                blocked = read blk
                immack = ack == "1"
            encode qfile efile capacity blocked immack
        _ -> putStrLn "qif-enc <qif-file> <capacity> <blocked> <ack>"

encode :: FilePath -> FilePath -> Int -> Int -> Bool -> IO ()
encode qfile efile capacity blocked immack = do
    let encConf = defaultQEncoderConfig{ecMaxTableCapacity = capacity}
        save sid bs = do
            let header = runPut $ do
                    putWord64be $ fromIntegral (sid :: Int)
                    putWord32be $ fromIntegral $ BS.length bs
            BS.appendFile efile $ header <> bs
    enc <- newQEncoderS encConf (save 0) blocked immack
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
