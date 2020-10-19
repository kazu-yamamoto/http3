{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Conduit
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString, ByteString)
import qualified Data.ByteString as BS
import Data.Conduit.Attoparsec
import System.Environment

import Network.QPACK

data Block = Block Int ByteString deriving Show

----------------------------------------------------------------

main :: IO ()
main = do
    [efile] <- getArgs
    test efile

test :: FilePath -> IO ()
test efile = do
    (dec, insthdr, cleanup) <- newQDecoderS defaultQDecoderConfig
    runConduitRes (sourceFile efile .| conduitParser block .| mapM_C (liftIO . switch dec insthdr))
    cleanup

switch :: (ByteString -> IO HeaderList)
       -> InstructionHandlerS
       -> (a, Block)
       -> IO ()
switch dec insthdr (_, Block n bs)
  | n == 0    = do
        putStrLn "---- Stream 0:"
        insthdr bs
  | otherwise = do
        putStrLn $ "---- Stream " ++ show n ++ ":"
        _ <- dec bs
        return ()

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
