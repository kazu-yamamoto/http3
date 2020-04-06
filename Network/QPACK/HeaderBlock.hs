{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QPACK.HeaderBlock where

import Network.HPACK (HeaderList, EncodeStrategy(..))

import Network.ByteOrder
import Network.HPACK (TokenHeaderList)
import Network.HPACK.Internal
import Network.HPACK.Token

import Imports
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
    ibuf <- newWriteBuffer ibuf isiz
    -- fixme: set base point
    let revidx = getRevIndex dyntbl
    loop wbuf ibuf revidx ts0
    return undefined
  where
    loop _ _ _  [] = return ()
    loop wbuf ibuf revidx ((t,val):ts) = do
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
                                  ip <- getInsertPoint dyntbl
                                  return $ Right $ toInsRelativeIndex i ip
                  let ins = InsertWithNameReference insidx val
                  encodeEI ibuf True ins
                  dai <- insertEntry (toEntryToken t val) dyntbl
                  -- 4.5.3.  Indexed Header Field With Post-Base Index
                  encodeIndexedHeaderFieldWithPostBaseIndex wbuf dyntbl dai
            | otherwise         -> do
                  -- 4.5.4.  Literal Header Field With Name Reference
                  undefined
          N
            | shouldBeIndexed t -> do
                  let ins = InsertWithoutNameReference t val
                  encodeEI ibuf True ins
                  dai <- insertEntry (toEntryToken t val) dyntbl
                  encodeIndexedHeaderFieldWithPostBaseIndex wbuf dyntbl dai
            | otherwise         -> do
                  -- 4.5.6.  Literal Header Field Without Name Reference
                  undefined
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
encodeIndexedHeaderFieldWithPostBaseIndex = undefined


set10, set11 :: Word8 -> Word8
set10 = (.|. 0b10000000)
set11 = (.|. 0b11000000)
