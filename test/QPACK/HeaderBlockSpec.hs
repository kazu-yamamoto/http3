{-# LANGUAGE OverloadedStrings #-}

module QPACK.HeaderBlockSpec where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import Network.HPACK.Token (toToken)
import Network.QPACK
import Network.QPACK.Internal (
    EncoderInstruction (..),
    encodeEncoderInstructions,
 )
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
    describe "field section round trip" $ do
        it "decodes a header value larger than the decoder's scratch buffer" $ do
            -- The scratch buffer for Huffman decoding used to be a fixed 2048
            -- octets, so a value above that failed to decode however small the
            -- section carrying it was: 2100 octets here comes to well under
            -- 1.4K encoded, far inside the SETTINGS_MAX_FIELD_SECTION_SIZE we
            -- advertise.
            mapM_ roundTrip [1000, 2100, 8000, 20000]

        it "encodes a field larger than the encoder's buffers" $ do
            -- With the default 4096-octet buffers.  4083 is the value that
            -- makes the field come to exactly the buffer by the encoder's
            -- estimate, which used to loop forever; anything larger was
            -- refused with BufferOverrun.  100000 is also long enough for
            -- its Huffman code to need a four-octet length.
            forM_ [4083, 4084, 10000, 100000] $ \n -> do
                r <- timeout 5000000 $ roundTripWith defaultQEncoderConfig n
                void r `shouldBe` Just ()

        it "agrees with the decoder on MaxEntries when the encoder uses less" $ do
            -- The decoder offers 64K, the encoder settles for 4K.  Required
            -- Insert Count is encoded against the 64K both ends know about;
            -- encoding it against the 4K made the two ends disagree once
            -- 2*128 entries had been inserted.
            roundTripMany 4096 65536 600

        it "does not refer to an unacknowledged entry when blocking is not allowed" $ do
            -- SETTINGS_QPACK_BLOCKED_STREAMS is 0, and no acknowledgement
            -- ever arrives: every section has to be decodable with what the
            -- decoder is known to hold, i.e. Required Insert Count 0.  The
            -- third one used to refer to the entry the second one inserted.
            (enc, _, tblop) <- newQEncoder defaultQEncoderConfig (\_ -> return ())
            setCapacity tblop 4096
            setBlockedStreams tblop 0
            forM_ [0 .. 4] $ \i -> do
                blk <- enc (i * 4) [(toToken "x-foo", "bar")]
                encodedInsertCount blk `shouldBe` 0

        it "keeps the number of blocked streams within the decoder's limit" $ do
            -- One blocked stream allowed, and nothing is ever acknowledged, so
            -- every section that refers to the dynamic table is blocked: at
            -- most one of them may do so.
            (enc, _, tblop) <- newQEncoder defaultQEncoderConfig (\_ -> return ())
            setCapacity tblop 4096
            setBlockedStreams tblop 1
            blks <- forM [0 .. 29] $ \i -> do
                let hdr = [(toToken "x-foo", C8.pack (show (i `div` 3 :: Int)))]
                enc (i * 4) hdr
            length (filter ((/= 0) . encodedInsertCount) blks) `shouldSatisfy` (<= 1)

        it "takes two outstanding sections on one stream one at a time" $ do
            -- Headers and trailers on the same stream, both referring to the
            -- dynamic table and so both acknowledged.  The second used to
            -- replace the first, and the second acknowledgement then found
            -- nothing and was taken for a decoder stream error.
            eiRef <- newIORef []
            diRef <- newIORef []
            let save ref bs = modifyIORef' ref (++ [bs])
            (enc, handleDI, tblop) <- newQEncoder defaultQEncoderConfig (save eiRef)
            (dec, handleEI) <- newQDecoder defaultQDecoderConfig (save diRef)
            setCapacity tblop 4096
            setBlockedStreams tblop 100
            let hdr = [(toToken "x-foo", "bar")]
                exchange sid = do
                    blk <- enc sid hdr
                    drain eiRef handleEI
                    (ths, _) <- dec sid blk
                    ths `shouldBe` hdr
                    return blk
            -- Get the field into the table and acknowledged.
            forM_ [0, 4, 8] $ \sid -> exchange sid >> drain diRef handleDI
            blk1 <- exchange 12
            blk2 <- exchange 12
            map encodedInsertCount [blk1, blk2] `shouldSatisfy` all (/= 0)
            drain diRef handleDI

        it "keeps the decoder's entries across a change of capacity" $ do
            -- The encoder may change the capacity at any time.  The decoder
            -- used to reallocate its table when told, so the entry inserted
            -- before the change was gone when referred to after it.
            eiRef <- newIORef []
            (dec, handleEI) <- newQDecoder defaultQDecoderConfig (\_ -> return ())
            ins <-
                encodeEncoderInstructions
                    [ SetDynamicTableCapacity 4096
                    , InsertWithLiteralName (toToken "x-foo") "bar"
                    , SetDynamicTableCapacity 2048
                    ]
                    False
            writeIORef eiRef [ins]
            drain eiRef handleEI
            -- Required Insert Count 1 (encoded as 2 against 128 entries),
            -- Delta Base 0, then an indexed field line for relative index 0.
            (ths, _) <- dec 0 $ BS.pack [0x02, 0x00, 0x80]
            ths `shouldBe` [(toToken "x-foo", "bar")]

-- | Feeding an instruction handler what has been queued for it, then an end
-- of stream, so that it returns -- or throws -- here.
drain :: IORef [BS.ByteString] -> ((Int -> IO BS.ByteString) -> IO ()) -> IO ()
drain ref handler = do
    bss <- readIORef ref
    writeIORef ref []
    queue <- newIORef [BS.concat bss]
    handler $ \_ -> atomicModifyIORef' queue $ \xs -> case xs of
        [] -> ([], "")
        y : ys -> (ys, y)

roundTrip :: Int -> IO ()
roundTrip n = do
    blk <- roundTripWith defaultQEncoderConfig{ecHeaderBlockBufferSize = 262144} n
    -- The encoded section stays well under the announced limit; it is only
    -- the decoded value that is large.
    BS.length blk `shouldSatisfy` (< dcMaxFieldSectionSize defaultQDecoderConfig)

-- | Encoding and decoding a field with an n-octet value; the encoded field
-- section is returned.
roundTripWith :: QEncoderConfig -> Int -> IO BS.ByteString
roundTripWith conf n = do
    (enc, _, _) <- newQEncoder conf (\_ -> return ())
    (dec, _) <- newQDecoder defaultQDecoderConfig (\_ -> return ())
    let val = C8.replicate n 'a'
    blk <- enc 0 [(toToken ":status", "200"), (toToken "x-big", val)]
    (ths, _) <- dec 0 blk
    lookup (toToken "x-big") ths `shouldBe` Just val
    return blk

roundTripMany :: Int -> Int -> Int -> IO ()
roundTripMany encCap decCap n = do
    eiQ <- newTQueueIO
    diQ <- newTQueueIO
    (enc, handleDI, tblop) <-
        newQEncoder
            defaultQEncoderConfig{ecMaxTableCapacity = encCap}
            (atomically . writeTQueue eiQ)
    (dec, handleEI) <-
        newQDecoder
            defaultQDecoderConfig{dcMaxTableCapacity = decCap}
            (atomically . writeTQueue diQ)
    -- What the decoder's SETTINGS would carry.
    setCapacity tblop decCap
    setBlockedStreams tblop 100
    let recvFrom q _ = atomically $ readTQueue q
    ricMax <- newIORef 0
    withAsync (handleEI $ recvFrom eiQ) $ \_ ->
        withAsync (handleDI $ recvFrom diQ) $ \_ ->
            forM_ [0 .. n - 1] $ \i -> do
                -- Twice each, since a field is only inserted the second
                -- time it is seen.
                forM_ [0, 1] $ \j -> do
                    let sid = (i * 2 + j) * 4
                        hdr = [(toToken "x-count", C8.pack (show i))]
                    blk <- enc sid hdr
                    modifyIORef' ricMax (max (encodedInsertCount blk))
                    (ths, _) <- dec sid blk
                    ths `shouldBe` hdr
                    -- Let acknowledgements reach the encoder, so that
                    -- entries can be evicted and inserting goes on.
                    atomically $ isEmptyTQueue diQ >>= check
    -- Our own decoder would accept either reading of MaxEntries as long as
    -- the encoder used the same one, so look at the prefix itself: against
    -- the decoder's 2048 entries nothing here wraps, and the encoded count
    -- goes past what 2*128 would have allowed.
    readIORef ricMax >>= (`shouldSatisfy` (> 2 * (encCap `div` 32)))

-- | The encoded Required Insert Count: an integer with an 8-bit prefix.
encodedInsertCount :: BS.ByteString -> Int
encodedInsertCount blk = case BS.unpack blk of
    w : ws
        | w < 255 -> fromIntegral w
        | otherwise -> 255 + go 0 0 ws
    [] -> 0
  where
    go acc sh (x : xs)
        | x < 128 = acc + fromIntegral x * 2 ^ (sh :: Int)
        | otherwise = go (acc + fromIntegral (x - 128) * 2 ^ sh) (sh + 7) xs
    go acc _ [] = acc
