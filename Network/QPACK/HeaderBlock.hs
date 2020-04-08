{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QPACK.HeaderBlock where

import Network.HPACK (HeaderList, EncodeStrategy(..))

import Network.ByteOrder
import Network.HPACK (TokenHeaderList)
import Network.HPACK.Internal
import Network.HPACK.Token

import Network.QPACK.Instruction
import Network.QPACK.Table
import Network.QPACK.Types

encodeHeader :: EncodeStrategy -> Size -> DynamicTable -> HeaderList -> IO ByteString
encodeHeader = undefined

-- | Converting 'TokenHeaderList' to the HPACK format directly in the buffer.
--
--   4th argument is relating to dynamic table size update.
--   When calling this function for a new 'TokenHeaderList',
--   it must be 'True'.
--   If 'True' and set by 'setLimitForEncoding',
--   dynamic table size update is generated at the beginning of
--   the HPACK format.
--
--   The return value is a pair of leftover 'TokenHeaderList' and
--   how many bytes are filled in the buffer.
--   If the leftover is empty, the encoding is finished.
--   Otherwise, this function should be called with it again.
--   4th argument must be 'False'.
--
encodeTokenHeader :: Buffer
                  -> BufferSize
                  -> Buffer
                  -> BufferSize
                  -> EncodeStrategy
                  -> Bool -- ^ 'True' at the first time, 'False' when continued.
                  -> DynamicTable
                  -> TokenHeaderList
                  -> IO (TokenHeaderList, Int) -- ^ Leftover, filled length
encodeTokenHeader buf siz ibuf isiz EncodeStrategy{..} first dyntbl ts0 = do
    wbuf <- newWriteBuffer buf siz
    wbufi <- newWriteBuffer ibuf isiz
    -- fixme: set base point
    let revidx = getRevIndex dyntbl
    loop wbuf wbufi revidx useHuffman ts0
    return undefined
  where
    loop _ _ _ _ [] = return ()
    loop wbuf wbufi revidx huff ((t,val):ts) = do
        rr <- lookupRevIndex t val revidx
        case rr of
          KV hi -> do
              -- 4.5.2.  Indexed Header Field
              encodeIndexedHeaderField wbuf dyntbl hi
          K  hi
            | shouldBeIndexed t -> do
                  insidx <- case hi of
                              SIndex i -> return $ Left i
                              DIndex i -> do
                                  ip <- getInsertionPoint dyntbl
                                  return $ Right $ toInsRelativeIndex i ip
                  let ins = InsertWithNameReference insidx val
                  encodeEI wbufi True ins
                  dai <- insertEntry (toEntryToken t val) dyntbl
                  -- 4.5.3.  Indexed Header Field With Post-Base Index
                  encodeIndexedHeaderFieldWithPostBaseIndex wbuf dyntbl dai
            | otherwise         -> do
                  -- 4.5.4.  Literal Header Field With Name Reference
                  encodeLiteralHeaderFieldWithNameReference wbuf dyntbl hi val huff
          N
            | shouldBeIndexed t -> do
                  let ins = InsertWithoutNameReference t val
                  encodeEI wbufi True ins
                  dai <- insertEntry (toEntryToken t val) dyntbl
                  encodeIndexedHeaderFieldWithPostBaseIndex wbuf dyntbl dai
            | otherwise         -> do
                  -- 4.5.6.  Literal Header Field Without Name Reference
                  encodeLiteralHeaderFieldWithoutNameReference wbuf t val huff
        -- fixme: size check and call loop

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
