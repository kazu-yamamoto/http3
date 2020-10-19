{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Conduit
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString, ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Conduit.Attoparsec
import Network.ByteOrder
import System.Environment
import System.IO

import Network.QPACK
import Network.QPACK.Internal

data Block = Block Int ByteString deriving Show

----------------------------------------------------------------

main :: IO ()
main = do
    [efile] <- getArgs
    test efile

test :: FilePath -> IO ()
test efile = do
    dyntbl <- newDynamicTableForDecoding 4096 4096
    runConduitRes (sourceFile efile .| conduitParser block .| mapM_C (liftIO . switch dyntbl))

switch :: DynamicTable
       -> (PositionRange, Block)
       -> IO ()
switch dyntbl (_, Block n bs)
  | n == 0    = do
        putStrLn "---- Stream 0:"
        (inss, "") <- decodeEncoderInstructions hufdec bs
        mapM_ print inss
  | otherwise = do
        putStrLn $ "---- Stream " ++ show n ++ ":"
        _ <- withReadBuffer bs $ decodeTokenHeaderS dyntbl
        return ()
  where
    hufdec = getHuffmanDecoder dyntbl

block :: Parser Block
block = do
    num <- toInt <$> P.take 8
    len <- toInt <$> P.take 4
    dat <- P.take len
    return $ Block num dat

toInt :: ByteString -> Int
toInt bs = BS.foldl' f 0 bs
  where
    f n w8 = n * 256 + fromIntegral w8
