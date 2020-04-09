{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QPACK.HeaderBlock.Encode (
    encodeHeader
  , encodeTokenHeader
  ) where

import qualified Control.Exception as E
import Data.IORef
import Network.ByteOrder
import Network.HPACK (HeaderList, EncodeStrategy(..), TokenHeaderList)
import Network.HPACK.Internal
import Network.HPACK.Token

import Imports
import Network.QPACK.Instruction
import Network.QPACK.Table
import Network.QPACK.Types

encodeHeader :: EncodeStrategy -> Size -> Size -> DynamicTable -> HeaderList -> IO (ByteString,ByteString)
encodeHeader stgy siz1 siz2 dyntbl hs =
    withWriteBuffer' siz1 $ \wbuf1 ->
      withWriteBuffer siz2 $ \wbuf2 -> do
         hs1 <- encodeTokenHeader wbuf1 wbuf2 stgy dyntbl ts
         unless (null hs1) $ E.throwIO BufferOverrun
  where
    ts = map (\(k,v) -> let t = toToken k in (t,v)) hs

-- | Converting 'TokenHeaderList' to the HPACK format directly in the buffer.
encodeTokenHeader :: WriteBuffer
                  -> WriteBuffer
                  -> EncodeStrategy
                  -> DynamicTable
                  -> TokenHeaderList
                  -> IO TokenHeaderList
encodeTokenHeader wbuf1 wbuf2 EncodeStrategy{..} dyntbl ts0 = do
    setBasePointToInsersionPoint dyntbl
    let revidx = getRevIndex dyntbl
    ref <- newIORef ts0
    encodeTokenHeader' wbuf1 wbuf2 dyntbl revidx useHuffman ref ts0  `E.catch` \BufferOverrun -> return ()
    ts <- readIORef ref
    unless (null ts) $ do
        goBack wbuf1
        goBack wbuf2
    return ts

encodeTokenHeader' :: WriteBuffer -> WriteBuffer
                   -> DynamicTable -> RevIndex -> Bool
                   -> IORef TokenHeaderList
                   -> TokenHeaderList -> IO ()
encodeTokenHeader' wbuf1 wbuf2 dyntbl revidx huff ref ts0 = loop ts0
  where
    loop [] = return ()
    loop ((t,val):ts) = do
        rr <- lookupRevIndex t val revidx
        case rr of
          KV hi -> do
              -- 4.5.2.  Indexed Header Field
              encodeIndexedHeaderField wbuf1 dyntbl hi
          K  hi
            | shouldBeIndexed t -> do
                  insidx <- case hi of
                              SIndex i -> return $ Left i
                              DIndex i -> do
                                  ip <- getInsertionPoint dyntbl
                                  return $ Right $ toInsRelativeIndex i ip
                  let ins = InsertWithNameReference insidx val
                  encodeEI wbuf2 True ins
                  dai <- insertEntry (toEntryToken t val) dyntbl
                  -- 4.5.3.  Indexed Header Field With Post-Base Index
                  encodeIndexedHeaderFieldWithPostBaseIndex wbuf1 dyntbl dai
            | otherwise         -> do
                  -- 4.5.4.  Literal Header Field With Name Reference
                  encodeLiteralHeaderFieldWithNameReference wbuf1 dyntbl hi val huff
          N
            | shouldBeIndexed t -> do
                  let ins = InsertWithoutNameReference t val
                  encodeEI wbuf2 True ins
                  dai <- insertEntry (toEntryToken t val) dyntbl
                  encodeIndexedHeaderFieldWithPostBaseIndex wbuf1 dyntbl dai
            | otherwise         -> do
                  -- 4.5.6.  Literal Header Field Without Name Reference
                  encodeLiteralHeaderFieldWithoutNameReference wbuf1 t val huff
        save wbuf1
        save wbuf2
        writeIORef ref ts
        loop ts

-- 4.5.2.  Indexed Header Field
encodeIndexedHeaderField :: WriteBuffer -> DynamicTable -> HIndex -> IO ()
encodeIndexedHeaderField wbuf dyntbl hi = do
    (idx, set) <- case hi of
      SIndex (AbsoluteIndex i) -> return (i, set11)
      DIndex ai-> do
          updateLargestReference dyntbl ai
          bp <- getBasePoint dyntbl
          let HBRelativeIndex i = toHBRelativeIndex ai bp
          return (i, set10)
    encodeI wbuf set 6 idx

-- 4.5.3.  Indexed Header Field With Post-Base Index
encodeIndexedHeaderFieldWithPostBaseIndex :: WriteBuffer
                                          -> DynamicTable
                                          -> AbsoluteIndex -- in Dynamic table
                                          -> IO ()
encodeIndexedHeaderFieldWithPostBaseIndex wbuf dyntbl ai = do
    bp <- getBasePoint dyntbl
    let HBRelativeIndex idx = toHBRelativeIndex ai bp
    encodeI wbuf set0001 4 idx

-- 4.5.4.  Literal Header Field With Name Reference
encodeLiteralHeaderFieldWithNameReference :: WriteBuffer -> DynamicTable -> HIndex -> ByteString -> Bool -> IO ()
encodeLiteralHeaderFieldWithNameReference wbuf dyntbl hi val huff = do
    (idx, set) <- case hi of
      SIndex (AbsoluteIndex i) -> return (i, set0101)
      DIndex ai-> do
          updateLargestReference dyntbl ai
          bp <- getBasePoint dyntbl
          let HBRelativeIndex i = toHBRelativeIndex ai bp
          return (i, set0100)
    encodeI wbuf set 4 idx
    encodeS wbuf huff id set1 7 val

-- 4.5.5.  Literal Header Field With Post-Base Name Reference
-- not implemented

-- 4.5.6.  Literal Header Field Without Name Reference
encodeLiteralHeaderFieldWithoutNameReference :: WriteBuffer -> Token -> ByteString -> Bool -> IO ()
encodeLiteralHeaderFieldWithoutNameReference wbuf token val huff = do
    let key = tokenCIKey token
    encodeS wbuf huff set0010 set00001 3 key
    encodeS wbuf huff id set1 7 val
