{-# LANGUAGE OverloadedStrings #-}

module HTTP3.FrameSpec where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Network.HTTP3.Internal
import Test.Hspec

-- | A byte source in the shape 'recvStream' has: hands out at most the
-- requested number of octets, then empty strings for ever after.
sourceOf :: ByteString -> IO (Int -> IO ByteString)
sourceOf bs0 = do
    ref <- newIORef bs0
    return $ \n -> atomicModifyIORef' ref $ \bs ->
        let (taken, rest) = BS.splitAt n bs in (rest, taken)

spec :: Spec
spec = do
    describe "recvQInt" $ do
        it "reads a variable-length integer of each width" $ do
            -- RFC 9000 section 16: one, two, four and eight octet forms.
            -- The stream type of a unidirectional stream is one of these, and
            -- reading only the first octet cut everything from 0x40 up in half.
            recvOn "\x03" `shouldReturn` Just 3
            recvOn "\x40\x03" `shouldReturn` Just 3
            recvOn "\x80\x00\x00\x03" `shouldReturn` Just 3
            recvOn "\x25" `shouldReturn` Just 0x25
            recvOn "\x40\x25" `shouldReturn` Just 0x25
            recvOn "\x7b\xbd" `shouldReturn` Just 15293

        it "leaves the rest of the stream alone" $ do
            src <- sourceOf "\x40\x03rest"
            recvQInt src `shouldReturn` Just 3
            src 4 `shouldReturn` "rest"

        it "gives up when the stream ends first" $ do
            -- A peer may open a unidirectional stream and close it without
            -- ever saying what it was for.
            recvOn "" `shouldReturn` Nothing
            recvOn "\x40" `shouldReturn` Nothing
            recvOn "\x80\x00" `shouldReturn` Nothing
  where
    recvOn bs = sourceOf bs >>= recvQInt
