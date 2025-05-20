{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Conduit hiding (yield)
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Conduit.Attoparsec
import Network.QUIC (StreamId)
import System.Environment
import System.Exit
import System.IO

import Network.QPACK
import Network.QPACK.Internal

data Block = Block Int ByteString deriving (Show)

----------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [size, efile] -> dump (read size) efile
        [size, efile, qfile] -> test (read size) efile qfile
        _ -> putStrLn "qif size <encode-file> [<qif-file>]"

----------------------------------------------------------------

dump :: Int -> FilePath -> IO ()
dump size efile = do
    (dec, insthdr) <-
        newQDecoderS
            defaultQDecoderConfig{dcDynamicTableSize = size}
            (\_ -> return ())
            True
    bs <- encodeEncoderInstructions [SetDynamicTableCapacity size] False
    insthdr bs
    runConduitRes
        ( sourceFile efile
            .| conduitParser block
            .| mapM_C (liftIO . dumpSwitch dec insthdr)
        )

dumpSwitch
    :: (StreamId -> ByteString -> IO [Header])
    -> EncoderInstructionHandlerS
    -> (a, Block)
    -> IO ()
dumpSwitch dec insthdr (_, Block n bs)
    | n == 0 = do
        putStrLn "---- Encoder Stream"
        insthdr bs
    | otherwise = do
        putStrLn $ "---- Stream " ++ show n
        _ <- dec n bs
        return ()

----------------------------------------------------------------

test :: Int -> FilePath -> FilePath -> IO ()
test size efile qfile = do
    (dec, insthdr') <-
        newQDecoderS
            defaultQDecoderConfig{dcDynamicTableSize = size}
            (\_ -> return ())
            False
    ins <- encodeEncoderInstructions [SetDynamicTableCapacity size] False
    insthdr' ins
    q <- newTQueueIO
    let recv = atomically $ readTQueue q
        send x = atomically $ writeTQueue q x
        insthdr bs = do
            emp <- atomically $ isEmptyTQueue q
            unless emp yield
            insthdr' bs
            yield
    mvar <- newEmptyMVar
    withFile qfile ReadMode $ \h -> do
        tid <- forkIO $ decode dec h recv mvar
        runConduitRes
            ( sourceFile efile
                .| conduitParser block
                .| mapM_C (liftIO . testSwitch send insthdr)
            )
        takeMVar mvar
        killThread tid

testSwitch
    :: (Block -> IO ())
    -> EncoderInstructionHandlerS
    -> (a, Block)
    -> IO ()
testSwitch send insthdr (_, blk@(Block n bs))
    | n == 0 = insthdr bs
    | otherwise = send blk

decode :: QDecoderS -> Handle -> IO Block -> MVar () -> IO ()
decode dec h recv mvar = loop
  where
    loop = do
        hdr' <- fromCaseSensitive <$> headerlist h
        if null hdr'
            then putMVar mvar ()
            else do
                Block n bs <- recv
                hdr <- fromCaseSensitive <$> dec n bs
                if hdr == hdr'
                    then loop
                    else do
                        putStrLn $ "---- Stream " ++ show n
                        mapM_ print hdr
                        putStrLn "----"
                        mapM_ print hdr'
                        putStrLn "----"
                        exitFailure

fromCaseSensitive :: [Header] -> [Header]
fromCaseSensitive = map (\(k, v) -> (foldedCase $ mk k, v))

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
