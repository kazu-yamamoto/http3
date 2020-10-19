{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Conduit
import qualified Control.Exception as E
import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString, ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Conduit.Attoparsec
import Network.QPACK
import System.Environment
import System.Exit
import System.IO

data Block = Block Int ByteString deriving Show

----------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
      [efile]       -> dump efile
      [efile,qfile] -> test efile qfile
      _ -> putStrLn "qif <encode-file> [<qif-file>]"

----------------------------------------------------------------

dump :: FilePath -> IO ()
dump efile = do
    (dec, insthdr, cleanup) <- newQDecoderS defaultQDecoderConfig True
    runConduitRes (sourceFile efile .| conduitParser block .| mapM_C (liftIO . dumpSwitch dec insthdr))
    cleanup

dumpSwitch :: (ByteString -> IO HeaderList)
       -> InstructionHandlerS
       -> (a, Block)
       -> IO ()
dumpSwitch dec insthdr (_, Block n bs)
  | n == 0    = do
        putStrLn "---- Stream 0:"
        insthdr bs
  | otherwise = do
        putStrLn $ "---- Stream " ++ show n ++ ":"
        _ <- dec bs
        return ()

----------------------------------------------------------------

test :: FilePath -> FilePath -> IO ()
test efile qfile = do
    (dec, insthdr, cleanup) <- newQDecoderS defaultQDecoderConfig False
    withFile qfile ReadMode $ \h ->
        runConduitRes (sourceFile efile .| conduitParser block .| mapM_C (liftIO . testSwitch (decode dec h) insthdr))
    cleanup

testSwitch :: (ByteString -> IO ())
       -> InstructionHandlerS
       -> (a, Block) -> IO ()
testSwitch send insthdr (_, Block n bs)
  | n == 0    = insthdr bs
  | otherwise = send bs

decode :: (ByteString -> IO HeaderList)
       -> Handle
       -> ByteString
       -> IO ()
decode dec h bs = do
    hdr <- dec bs
    hdr' <- fromCaseSensitive <$> headerlist h
    when (hdr /= hdr') $ do
        print hdr
        exitFailure

fromCaseSensitive :: HeaderList -> HeaderList
fromCaseSensitive = map (\(k,v) -> (foldedCase $ mk k,v))

----------------------------------------------------------------

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

----------------------------------------------------------------

headerlist :: Handle -> IO HeaderList
headerlist h = loop id
  where
    loop b = do
        ml <- line h
        case ml of
          Nothing -> return $ b []
          Just l
            | l == ""   -> return $ b []
            | otherwise -> do
                  let (k,v0) = BS8.break (== '\t') l
                      v = BS8.drop 1 v0
                  loop (b . ((k,v) :))

line :: Handle -> IO (Maybe ByteString)
line h = do
    el <- E.try $ BS8.hGetLine h
    case el of
      Left (_ :: E.IOException) -> return Nothing
      Right l
        | BS8.take 1 l == "#" -> line h
        | otherwise           -> return $ Just l
