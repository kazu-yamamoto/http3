{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QIF (
    processBlock,
    Block (..),
    headerlist,
    fromCaseSensitive,
    headerSize,
) where

import qualified Control.Exception as E
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.IO

import Network.QPACK

import Conduit
import Data.Conduit.Attoparsec

----------------------------------------------------------------

data Block = Block Int ByteString deriving (Show)

----------------------------------------------------------------

processBlock :: FilePath -> ((PositionRange, Block) -> IO ()) -> IO ()
processBlock efile func =
    runConduitRes
        ( sourceFile efile
            .| conduitParser block
            .| mapM_C (liftIO . func)
        )

----------------------------------------------------------------

block :: Parser Block
block = do
    num <- toInt <$> P.take 8
    len <- toInt <$> P.take 4
    dat <- P.take len
    return $ Block num dat

toInt :: ByteString -> Int
toInt = BS.foldl' f 0
  where
    f n w8 = n * 256 + fromIntegral w8

----------------------------------------------------------------

headerlist :: Handle -> IO [Header]
headerlist h = loop id
  where
    loop b = do
        ml <- line h
        case ml of
            Nothing -> return $ b []
            Just l
                | l == "" -> return $ b []
                | otherwise -> do
                    let (k, v0) = BS8.break (== '\t') l
                        v = BS8.drop 1 v0
                    loop (b . ((mk k, v) :))

line :: Handle -> IO (Maybe ByteString)
line h = do
    el <- E.try $ BS8.hGetLine h
    case el of
        Left (_ :: E.IOException) -> return Nothing
        Right l
            | BS8.take 1 l == "#" -> line h
            | otherwise -> return $ Just l

----------------------------------------------------------------

fromCaseSensitive :: [Header] -> [Header]
fromCaseSensitive = map (\(k, v) -> (foldedCase $ mk k, v))

headerSize :: Header -> Int
headerSize (k, v) = BS.length (foldedCase k) + BS.length v
