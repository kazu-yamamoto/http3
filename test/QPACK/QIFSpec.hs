{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QPACK.QIFSpec where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Conduit.Attoparsec
import Data.IORef
import Data.Sequence (Seq, ViewR (..), viewr, (<|))
import qualified Data.Sequence as Seq
import Network.QUIC (StreamId)
import System.IO
import Test.Hspec

import Network.QPACK
import Network.QPACK.Internal

import QIF

spec :: Spec
spec = do
    describe "simple decoder" $ do
        it "decodes with dynamic table" $ do
            forM_ ["fb-req-hq", "fb-resp-hq", "netbsd-hq"] $ \svc ->
                forM_ ["f5", "ls-qpack", "nghttp3", "proxygen", "qthingey", "quinn"] $ \impl ->
                    forM_ [".0.0", ".0.1", ".100.0", ".100.1"] $ \suffix ->
                        forM_ [256, 512, 4096 :: Int] $ \size -> do
                            let inp =
                                    "qifs/encoded/qpack-05/" ++ impl ++ "/" ++ svc ++ ".out." ++ show size ++ suffix
                                out = "qifs/qifs/" ++ svc ++ ".qif"
                            putStrLn inp
                            test size inp out

        it "decodes only with static table" $ do
            forM_ ["fb-req-hq", "fb-resp-hq", "netbsd-hq"] $ \svc ->
                forM_ ["ls-qpack", "nghttp3", "qthingey", "quinn"] $ \impl ->
                    forM_ [".0.0", ".0.1", ".100.0", ".100.1"] $ \suffix -> do
                        let size = 0
                        let inp =
                                "qifs/encoded/qpack-05/" ++ impl ++ "/" ++ svc ++ ".out." ++ show size ++ suffix
                            out = "qifs/qifs/" ++ svc ++ ".qif"
                        putStrLn inp
                        test size inp out

----------------------------------------------------------------

test :: Int -> FilePath -> FilePath -> IO ()
test size efile qfile = do
    (dec, insthdr) <-
        newQDecoderS
            defaultQDecoderConfig{dcMaxTableCapacity = size}
            (\_ -> return ())
            False
    encodeEncoderInstructions [SetDynamicTableCapacity size] False >>= insthdr
    ref <- newIORef Seq.empty
    withFile qfile ReadMode $ \h -> do
        processBlock efile $ testSwitch dec insthdr ref h

testSwitch
    :: (StreamId -> ByteString -> IO (Maybe [Header]))
    -> EncoderInstructionHandlerS
    -> IORef (Seq Block)
    -> Handle
    -> (PositionRange, Block)
    -> IO ()
testSwitch dec insthdr ref h (_, blk@(Block n bs))
    | n == 0 = do
        insthdr bs
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
        fromCaseSensitive hdr `shouldBe` hdr'
