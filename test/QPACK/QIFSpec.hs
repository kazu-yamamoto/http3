{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QPACK.QIFSpec where

import Conduit
import Control.Concurrent
import qualified Control.Exception as E
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
        it "decodes well " $ test "qifs/encoded/qpack-05/quinn/fb-resp-hq.out.4096.0.1" "qifs/qifs/fb-resp-hq.qif"

data Block = Block Int ByteString deriving Show

----------------------------------------------------------------

test :: FilePath -> FilePath -> IO ()
test efile qfile = do
    (dec, insthdr, cleanup) <- newQDecoderS defaultQDecoderConfig
    var <- newEmptyMVar
    let recv _ = takeMVar var
        send = putMVar var
    _ <- forkIO $ insthdr recv
    withFile qfile ReadMode $ \h -> do
      runConduitRes (sourceFile efile .| conduitParser block .| mapM_C (liftIO . decode h dec send))
    cleanup

----------------------------------------------------------------

decode :: Handle -> QDecoderS -> (ByteString -> IO ()) -> (PositionRange, Block) -> IO ()
decode _ _   send (_, Block 0 inst) = send inst
decode h dec _    (_, Block _ wire) = do
    hdr <- dec wire
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
