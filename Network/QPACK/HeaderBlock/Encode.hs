{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QPACK.HeaderBlock.Encode (
    encodeHeader
  , encodeTokenHeader
  , EncodedFieldSection
  , EncodedEncoderInstruction
  , EncodeStrategy(..)
  , CompressionAlgo(..)
  ) where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import Data.IORef
import Network.ByteOrder
import Network.HPACK (HeaderList, EncodeStrategy(..), TokenHeaderList, CompressionAlgo(..))
import Network.HPACK.Internal
import Network.HPACK.Token

import Imports
import Network.QPACK.HeaderBlock.Prefix
import Network.QPACK.Instruction
import Network.QPACK.Table
import Network.QPACK.Types

-- | Encoded field section including prefix.
type EncodedFieldSection = B.ByteString
-- | Encoded encoder instruction.
type EncodedEncoderInstruction = B.ByteString

-- | Encoding headers with QPACK.
--   Header block with prefix and instructions are returned.
--   2048, 32, and 2048 bytes-buffers are
--   temporally allocated for header block, prefix and encoder instructions.
encodeHeader :: EncodeStrategy -> DynamicTable -> HeaderList -> IO (EncodedFieldSection,EncodedEncoderInstruction)
encodeHeader stgy dyntbl hs = do
    (hb0, insb) <- withWriteBuffer' 2048 $ \wbuf1 ->
                       withWriteBuffer 2048 $ \wbuf2 -> do
                           hs1 <- encodeTokenHeader wbuf1 wbuf2 stgy dyntbl ts
                           unless (null hs1) $ E.throwIO BufferOverrun
    prefix <- withWriteBuffer 32 $ \wbuf -> encodePrefix wbuf dyntbl
    let hb = prefix `B.append` hb0
    return (hb,insb)
  where
    ts = map (\(k,v) -> let t = toToken k in (t,v)) hs

-- | Converting 'TokenHeaderList' to the QPACK format.
encodeTokenHeader :: WriteBuffer -- ^ Workspace for the body of header block
                  -> WriteBuffer -- ^ Workspace for encoder instructions
                  -> EncodeStrategy
                  -> DynamicTable
                  -> TokenHeaderList
                  -> IO TokenHeaderList -- ^ Leftover
encodeTokenHeader wbuf1 wbuf2 EncodeStrategy{..} dyntbl ts0 = do
    clearWriteBuffer wbuf1
    clearWriteBuffer wbuf2
    setBasePointToInsersionPoint dyntbl
    let revidx = getRevIndex dyntbl
    ref <- newIORef ts0
    case compressionAlgo of
      Static -> encodeStatic wbuf1 wbuf2 dyntbl revidx useHuffman ref ts0  `E.catch` \BufferOverrun -> return ()
      _      -> encodeLinear wbuf1 wbuf2 dyntbl revidx useHuffman ref ts0  `E.catch` \BufferOverrun -> return ()
    ts <- readIORef ref
    unless (null ts) $ do
        goBack wbuf1
        goBack wbuf2
    return ts

encodeStatic :: WriteBuffer -> WriteBuffer
             -> DynamicTable -> RevIndex -> Bool
             -> IORef TokenHeaderList
             -> TokenHeaderList -> IO ()
encodeStatic wbuf1 _wbuf2 dyntbl revidx huff ref ts0 = loop ts0
  where
    loop [] = return ()
    loop ((t,val):ts) = do
        rr <- lookupRevIndex t val revidx
        case rr of
          KV hi -> do
              -- 4.5.2.  Indexed Field Line
              encodeIndexedFieldLine wbuf1 dyntbl hi
          K  hi -> do
              -- 4.5.4.  Literal Field Line With Name Reference
              encodeLiteralFieldLineWithNameReference wbuf1 dyntbl hi val huff
          N     -> do
              -- 4.5.6.  Literal Field Line Without Name Reference
              encodeLiteralFieldLineWithoutNameReference wbuf1 t val huff
        save wbuf1
        writeIORef ref ts
        loop ts

encodeLinear :: WriteBuffer -> WriteBuffer
             -> DynamicTable -> RevIndex -> Bool
             -> IORef TokenHeaderList
             -> TokenHeaderList -> IO ()
encodeLinear wbuf1 wbuf2 dyntbl revidx huff ref ts0 = loop ts0
  where
    loop [] = return ()
    loop ((t,val):ts) = do
        rr <- lookupRevIndex t val revidx
        case rr of
          KV hi -> do
              -- 4.5.2.  Indexed Field Line
              encodeIndexedFieldLine wbuf1 dyntbl hi
          K  hi
            | shouldBeIndexed t -> do
                  insidx <- case hi of
                              SIndex i -> return $ Left i
                              DIndex i -> do
                                  ip <- getInsertionPoint dyntbl
                                  return $ Right $ toInsRelativeIndex i ip
                  let ins = InsertWithNameReference insidx val
                  encodeEI wbuf2 True ins
                  dai <- insertEntryToEncoder (toEntryToken t val) dyntbl
                  -- 4.5.3.  Indexed Field Line With Post-Base Index
                  encodeIndexedFieldLineWithPostBaseIndex wbuf1 dyntbl dai
            | otherwise         -> do
                  -- 4.5.4.  Literal Field Line With Name Reference
                  encodeLiteralFieldLineWithNameReference wbuf1 dyntbl hi val huff
          N
            | shouldBeIndexed t -> do
                  let ins = InsertWithoutNameReference t val
                  encodeEI wbuf2 True ins
                  dai <- insertEntryToEncoder (toEntryToken t val) dyntbl
                  encodeIndexedFieldLineWithPostBaseIndex wbuf1 dyntbl dai
            | otherwise         -> do
                  -- 4.5.6.  Literal Field Line Without Name Reference
                  encodeLiteralFieldLineWithoutNameReference wbuf1 t val huff
        save wbuf1
        save wbuf2
        writeIORef ref ts
        loop ts

-- 4.5.2.  Indexed Field Line
encodeIndexedFieldLine :: WriteBuffer -> DynamicTable -> HIndex -> IO ()
encodeIndexedFieldLine wbuf dyntbl hi = do
    (idx, set) <- case hi of
      SIndex (AbsoluteIndex i) -> return (i, set11)
      DIndex ai-> do
          updateLargestReference dyntbl ai
          bp <- getBasePoint dyntbl
          let HBRelativeIndex i = toHBRelativeIndex ai bp
          return (i, set10)
    encodeI wbuf set 6 idx

-- 4.5.3.  Indexed Field Line With Post-Base Index
encodeIndexedFieldLineWithPostBaseIndex :: WriteBuffer
                                          -> DynamicTable
                                          -> AbsoluteIndex -- in Dynamic table
                                          -> IO ()
encodeIndexedFieldLineWithPostBaseIndex wbuf dyntbl ai = do
    bp <- getBasePoint dyntbl
    let HBRelativeIndex idx = toHBRelativeIndex ai bp
    encodeI wbuf set0001 4 idx

-- 4.5.4.  Literal Field Line With Name Reference
encodeLiteralFieldLineWithNameReference :: WriteBuffer -> DynamicTable -> HIndex -> ByteString -> Bool -> IO ()
encodeLiteralFieldLineWithNameReference wbuf dyntbl hi val huff = do
    (idx, set) <- case hi of
      SIndex (AbsoluteIndex i) -> return (i, set0101)
      DIndex ai-> do
          updateLargestReference dyntbl ai
          bp <- getBasePoint dyntbl
          let HBRelativeIndex i = toHBRelativeIndex ai bp
          return (i, set0100)
    encodeI wbuf set 4 idx
    encodeS wbuf huff id set1 7 val

-- 4.5.5.  Literal Field Line With Post-Base Name Reference
-- not implemented

-- 4.5.6.  Literal Field Line Without Name Reference
encodeLiteralFieldLineWithoutNameReference :: WriteBuffer -> Token -> ByteString -> Bool -> IO ()
encodeLiteralFieldLineWithoutNameReference wbuf token val huff = do
    let key = tokenFoldedKey token
    encodeS wbuf huff set0010 set00001 3 key
    encodeS wbuf huff id set1 7 val
