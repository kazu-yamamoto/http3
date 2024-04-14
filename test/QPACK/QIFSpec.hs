{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QPACK.QIFSpec where

import Conduit hiding (yield)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Conduit.Attoparsec
import System.IO
import Test.Hspec
import qualified UnliftIO.Exception as E

import Network.QPACK

spec :: Spec
spec = do
    describe "simple decoder" $ do
        it "decodes well" $ do
            forM_ ["fb-req-hq", "fb-resp-hq", "netbsd-hq"] $ \svc ->
                forM_ ["f5", "ls-qpack", "nghttp3", "proxygen", "qthingey", "quinn"] $ \impl -> do
                    putStrLn $ impl ++ " with " ++ svc
                    let inp = "qifs/encoded/qpack-05/" ++ impl ++ "/" ++ svc ++ ".out.4096.0.0"
                        out = "qifs/qifs/" ++ svc ++ ".qif"
                    test inp out

data Block = Block Int ByteString deriving (Show)

----------------------------------------------------------------

test :: FilePath -> FilePath -> IO ()
test efile qfile = do
    (dec, insthdr) <- newQDecoderS defaultQDecoderConfig False
    q <- newTQueueIO
    let recv = atomically $ readTQueue q
        send x = atomically $ writeTQueue q x
    mvar <- newEmptyMVar
    withFile qfile ReadMode $ \h -> do
        tid <- forkIO $ decode dec h recv mvar
        runConduitRes
            ( sourceFile efile .| conduitParser block .| mapM_C (liftIO . switch send insthdr)
            )
        takeMVar mvar
        killThread tid

switch
    :: (Block -> IO ())
    -> EncoderInstructionHandlerS
    -> (PositionRange, Block)
    -> IO ()
switch send insthdr (_, blk@(Block n bs))
    | n == 0 = do
        insthdr bs
        yield
    | otherwise = send blk

decode
    :: (ByteString -> IO HeaderList)
    -> Handle
    -> IO Block
    -> MVar ()
    -> IO ()
decode dec h recv mvar = loop
  where
    loop = do
        hdr' <- fromCaseSensitive <$> headerlist h
        if hdr' == []
            then
                putMVar mvar ()
            else do
                Block _ bs <- recv
                hdr <- dec bs
                hdr `shouldBe` hdr'
                loop

fromCaseSensitive :: HeaderList -> HeaderList
fromCaseSensitive = map (\(k, v) -> (foldedCase $ mk k, v))

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
                | l == "" -> return $ b []
                | otherwise -> do
                    let (k, v0) = BS8.break (== '\t') l
                        v = BS8.drop 1 v0
                    loop (b . ((k, v) :))

line :: Handle -> IO (Maybe ByteString)
line h = do
    el <- E.try $ BS8.hGetLine h
    case el of
        Left (_ :: E.IOException) -> return Nothing
        Right l
            | BS8.take 1 l == "#" -> line h
            | otherwise -> return $ Just l
