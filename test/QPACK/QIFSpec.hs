{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QPACK.QIFSpec where

import Conduit
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as P
import Data.ByteString (ByteString, ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Conduit.Attoparsec
import System.IO
import Test.Hspec

import Network.QPACK

spec :: Spec
spec = do
    describe "simple decoder" $ do
        it "decodes well" $ do
            forM_ ["fb-req-hq","fb-resp-hq","netbsd-hq"] $ \svc ->
              forM_ ["f5","ls-qpack","nghttp3","proxygen","qthingey","quinn"] $ \impl -> do
                let inp = "qifs/encoded/qpack-05/" ++ impl ++ "/" ++ svc ++ ".out.4096.0.1"
                    out = "qifs/qifs/" ++ svc ++ ".qif"
                test inp out

data Block = Block Int ByteString deriving Show

----------------------------------------------------------------

test :: FilePath -> FilePath -> IO ()
test efile qfile = do
    (dec, insthdr, cleanup) <- newQDecoderS defaultQDecoderConfig
    q1 <- newTQueueIO
    q2 <- newTQueueIO
    let recv1 _ = atomically $ readTQueue  q1
        send1 x = atomically $ writeTQueue q1 x
        recv2   = atomically $ readTQueue  q2
        send2 x = atomically $ writeTQueue q2 x
    withFile qfile ReadMode $ \h -> do
        tid0 <- forkIO $ insthdr recv1
        tid1 <- forkIO $ decode dec h recv2
        runConduitRes (sourceFile efile .| conduitParser block .| mapM_C (liftIO . switch send1 send2))
        killThread tid0
        killThread tid1
    cleanup

switch :: (ByteString -> IO ())
       -> (ByteString -> IO ())
       -> (PositionRange, Block)
       -> IO ()
switch send1 send2 (_, Block n bs)
  | n == 0    = send1 bs
  | otherwise = send2 bs

decode :: (ByteString -> IO HeaderList) -> Handle -> IO ByteString -> IO ()
decode dec h recv2 = do
    hdr <- recv2 >>= dec
    hdr' <- fromCaseSensitive <$> headerlist h
    hdr `shouldBe` hdr'

fromCaseSensitive :: HeaderList -> HeaderList
fromCaseSensitive = map (\(k,v) -> (foldedCase $ mk k,v))

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
