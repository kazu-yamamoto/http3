{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Conduit hiding (yield)
import qualified Control.Exception as E
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
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

data Block = Block Int ByteString deriving (Show)

----------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [size, efile] -> dump (read size) efile
        [size, efile, qfile] -> test (read size) efile qfile
        _ -> putStrLn "qif size <encode-file> [<qif-file> \"0\"/\"1\"]"

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
    runConduitRes
        ( sourceFile efile
            .| conduitParser block
            .| mapM_C (liftIO . dumpSwitch dec insthdr ref)
        )

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
            Nothing -> modifyIORef' ref (\fifo -> blk <| fifo)
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
        runConduitRes
            ( sourceFile efile
                .| conduitParser block
                .| mapM_C (liftIO . testSwitch dec insthdr ref h ratio)
            )
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
            Nothing -> modifyIORef' ref (\fifo -> blk <| fifo)
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

fromCaseSensitive :: [Header] -> [Header]
fromCaseSensitive = map (\(k, v) -> (foldedCase $ mk k, v))

headerSize :: Header -> Int
headerSize (k, v) = BS.length (foldedCase k) + BS.length v

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
