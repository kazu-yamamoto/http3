{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Conduit.Attoparsec
import Data.IORef
import Data.Sequence (Seq, ViewR (..), viewr, (<|))
import qualified Data.Sequence as Seq
import Network.QUIC (StreamId)
import System.Environment
import System.Exit
import System.IO

import Network.QPACK
import Network.QPACK.Internal

import QIF

----------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [size, efile] -> dump (read size) efile
        [size, efile, qfile] -> test (read size) efile qfile
        _ -> putStrLn "qif-dec <size> <encode-file> [<qif-file>]"

----------------------------------------------------------------

dump :: Int -> FilePath -> IO ()
dump size efile = do
    (dec, insthdr) <-
        newQDecoderS
            defaultQDecoderConfig{dcDynamicTableSize = size}
            (\_ -> return ())
            True
    encodeEncoderInstructions [SetDynamicTableCapacity size] False >>= insthdr
    ref <- newIORef Seq.empty
    processBlock efile $ dumpSwitch dec insthdr ref

dumpSwitch
    :: (StreamId -> ByteString -> IO (Maybe [Header]))
    -> EncoderInstructionHandlerS
    -> IORef (Seq Block)
    -> (PositionRange, Block)
    -> IO ()
dumpSwitch dec insthdr ref (_, blk@(Block n bs))
    | n == 0 = do
        putStrLn "---- Encoder Stream"
        insthdr bs
        fifo <- readIORef ref
        loop fifo
    | otherwise = do
        mhdr <- dec n bs
        case mhdr of
            Nothing -> modifyIORef' ref (blk <|)
            Just _ -> return ()
  where
    loop fifo = do
        case viewr fifo of
            EmptyR -> writeIORef ref Seq.empty
            fifo' :> Block n1 bs1 -> do
                mhdr <- dec n1 bs1
                case mhdr of
                    Nothing -> writeIORef ref fifo
                    Just _ -> loop fifo'

----------------------------------------------------------------

data Ratio = Ratio
    { ratioInst :: Int
    , ratioHeader :: Int
    }

test :: Int -> FilePath -> FilePath -> IO ()
test size efile qfile = do
    (dec, insthdr) <-
        newQDecoderS
            defaultQDecoderConfig{dcDynamicTableSize = size}
            (\_ -> return ())
            False
    encodeEncoderInstructions [SetDynamicTableCapacity size] False >>= insthdr
    ref <- newIORef Seq.empty
    ratio <- newIORef $ Ratio{ratioInst = 0, ratioHeader = 0}
    withFile qfile ReadMode $ \h -> do
        processBlock efile $ testSwitch dec insthdr ref h ratio
        r <- readIORef ratio
        let (x, y) = ((ratioInst r * 1000) `div` ratioHeader r) `divMod` 10
        putStrLn $ show x ++ "." ++ show y

testSwitch
    :: (StreamId -> ByteString -> IO (Maybe [Header]))
    -> EncoderInstructionHandlerS
    -> IORef (Seq Block)
    -> Handle
    -> IORef Ratio
    -> (PositionRange, Block)
    -> IO ()
testSwitch dec insthdr ref h ratio (_, blk@(Block n bs))
    | n == 0 = do
        insthdr bs
        modifyIORef' ratio $ \r -> r{ratioInst = ratioInst r + BS.length bs}
        fifo <- readIORef ref
        loop fifo
    -- to avoid blocking by "dec", ask decoding to the other thread
    | otherwise = do
        mhdr <- dec n bs
        case mhdr of
            Nothing -> modifyIORef' ref (blk <|)
            Just hdr -> compareHeaders hdr
  where
    loop fifo = do
        case viewr fifo of
            EmptyR -> writeIORef ref Seq.empty
            fifo' :> Block n1 bs1 -> do
                mhdr <- dec n1 bs1
                case mhdr of
                    Nothing -> writeIORef ref fifo
                    Just hdr -> do
                        compareHeaders hdr
                        loop fifo'
    compareHeaders hdr = do
        hdr' <- fromCaseSensitive <$> headerlist h
        if fromCaseSensitive hdr == hdr'
            then modifyIORef' ratio $ \r ->
                r
                    { ratioInst = ratioInst r + BS.length bs
                    , ratioHeader = ratioHeader r + sum (map headerSize hdr)
                    }
            else do
                putStrLn $ "---- Stream " ++ show n
                let hdrt = zip hdr hdr'
                mapM_ printDiff hdrt
                exitFailure

----------------------------------------------------------------

printDiff :: (Header, Header) -> IO ()
printDiff (kv0, kv1)
    | kv0 == kv1 = print kv1
    | otherwise = do
        putStrLn $ "EXPECT: " ++ show kv1
        putStrLn $ "ACTUAL: " ++ show kv0
