{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QPACK.HeaderBlock.Encode (
    encodeHeader,
    encodeTokenHeader,
    EncodedFieldSection,
    EncodedEncoderInstruction,
) where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import Data.IORef
import Network.ByteOrder
import Network.HPACK.Internal (
    encodeI,
    encodeS,
    toEntryToken,
 )
import Network.HTTP.Semantics
import Network.HTTP.Types

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
encodeHeader
    :: DynamicTable
    -> [Header]
    -> IO (EncodedFieldSection, EncodedEncoderInstruction)
encodeHeader dyntbl hs = do
    (hb0, insb) <- withWriteBuffer' 2048 $ \wbuf1 ->
        withWriteBuffer 2048 $ \wbuf2 -> do
            hs1 <- encodeTokenHeader wbuf1 wbuf2 dyntbl ts
            unless (null hs1) $ E.throwIO BufferOverrun
    prefix <- withWriteBuffer 32 $ \wbuf -> encodePrefix wbuf dyntbl
    let hb = prefix `B.append` hb0
    return (hb, insb)
  where
    ts = map (\(k, v) -> let t = toToken (foldedCase k) in (t, v)) hs

-- | Converting 'TokenHeaderList' to the QPACK format.
encodeTokenHeader
    :: WriteBuffer
    -- ^ Workspace for the body of header block
    -> WriteBuffer
    -- ^ Workspace for encoder instructions
    -> DynamicTable
    -> TokenHeaderList
    -> IO ([AbsoluteIndex], TokenHeaderList)
    -- ^ Leftover
encodeTokenHeader wbuf1 wbuf2 dyntbl ts0 = do
    clearWriteBuffer wbuf1
    clearWriteBuffer wbuf2
    setBasePointToInsersionPoint dyntbl
    let revidx = getRevIndex dyntbl
    ref <- newIORef ts0
    ready <- isTableReady dyntbl
    dais <-
        if ready
            then
                encodeLinear wbuf1 wbuf2 dyntbl revidx True ref ts0 `E.catch` \BufferOverrun -> return []
            else
                encodeStatic wbuf1 wbuf2 dyntbl revidx True ref ts0 `E.catch` \BufferOverrun -> return []
    ts <- readIORef ref
    unless (null ts) $ do
        goBack wbuf1
        goBack wbuf2
    return (dais, ts)

encodeStatic
    :: WriteBuffer
    -> WriteBuffer
    -> DynamicTable
    -> RevIndex
    -> Bool
    -> IORef TokenHeaderList
    -> TokenHeaderList
    -> IO [AbsoluteIndex]
encodeStatic wbuf1 _wbuf2 dyntbl revidx huff ref ts0 = loop ts0
  where
    loop [] = return []
    loop ((t, val) : ts) = do
        rr <- lookupRevIndex t val revidx
        case rr of
            KV hi -> do
                -- 4.5.2.  Indexed Field Line
                encodeIndexedFieldLine wbuf1 dyntbl hi
            K hi -> do
                -- 4.5.4.  Literal Field Line With Name Reference
                encodeLiteralFieldLineWithNameReference wbuf1 dyntbl hi val huff
            N -> do
                -- 4.5.6.  Literal Field Line with Literal Name
                encodeLiteralFieldLineWithLiteralName wbuf1 t val huff
        save wbuf1
        writeIORef ref ts
        loop ts

encodeLinear
    :: WriteBuffer
    -> WriteBuffer
    -> DynamicTable
    -> RevIndex
    -> Bool
    -> IORef TokenHeaderList
    -> TokenHeaderList
    -> IO [AbsoluteIndex]
encodeLinear wbuf1 wbuf2 dyntbl revidx huff ref ts0 = loop ts0 []
  where
    loop [] dais = return dais
    loop ((t, val) : ts) dais = do
        rr <- lookupRevIndex t val revidx
        dais' <- case rr of
            KV hi -> do
                -- 4.5.2.  Indexed Field Line
                encodeIndexedFieldLine wbuf1 dyntbl hi
                case hi of
                    SIndex _ -> return dais
                    DIndex dai -> do
                        increaseReference dyntbl dai
                        return $ dai : dais
            K hi
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
                    increaseReference dyntbl dai
                    return $ dai : dais
                | otherwise -> do
                    -- 4.5.4.  Literal Field Line With Name Reference
                    encodeLiteralFieldLineWithNameReference wbuf1 dyntbl hi val huff
                    case hi of
                        SIndex _ -> return dais
                        DIndex dai -> do
                            increaseReference dyntbl dai
                            return $ dai : dais
            N
                | shouldBeIndexed t -> do
                    let ins = InsertWithLiteralName t val
                    encodeEI wbuf2 True ins
                    dai <- insertEntryToEncoder (toEntryToken t val) dyntbl
                    -- 4.5.3.  Indexed Field Line with Post-Base Index
                    encodeIndexedFieldLineWithPostBaseIndex wbuf1 dyntbl dai
                    increaseReference dyntbl dai
                    return $ dai : dais
                | otherwise -> do
                    -- 4.5.6.  Literal Field Line with Literal Name
                    encodeLiteralFieldLineWithLiteralName wbuf1 t val huff
                    return dais
        save wbuf1
        save wbuf2
        writeIORef ref ts
        loop ts dais'

-- 4.5.2.  Indexed Field Line
encodeIndexedFieldLine :: WriteBuffer -> DynamicTable -> HIndex -> IO ()
encodeIndexedFieldLine wbuf dyntbl hi = do
    (idx, set) <- case hi of
        SIndex (AbsoluteIndex i) -> return (i, set11)
        DIndex ai -> do
            updateRequiredInsertCount dyntbl ai
            bp <- getBasePoint dyntbl
            let HBRelativeIndex i = toHBRelativeIndex ai bp
            return (i, set10)
    encodeI wbuf set 6 idx

-- 4.5.3.  Indexed Field Line With Post-Base Index
encodeIndexedFieldLineWithPostBaseIndex
    :: WriteBuffer
    -> DynamicTable
    -> AbsoluteIndex -- in Dynamic table
    -> IO ()
encodeIndexedFieldLineWithPostBaseIndex wbuf dyntbl ai = do
    updateRequiredInsertCount dyntbl ai
    bp <- getBasePoint dyntbl
    let PostBaseIndex idx = toPostBaseIndex ai bp
    encodeI wbuf set0001 4 idx

-- 4.5.4.  Literal Field Line With Name Reference
encodeLiteralFieldLineWithNameReference
    :: WriteBuffer -> DynamicTable -> HIndex -> ByteString -> Bool -> IO ()
encodeLiteralFieldLineWithNameReference wbuf dyntbl hi val huff = do
    (idx, set) <- case hi of
        SIndex (AbsoluteIndex i) -> return (i, set0101)
        DIndex ai -> do
            updateRequiredInsertCount dyntbl ai
            bp <- getBasePoint dyntbl
            let HBRelativeIndex i = toHBRelativeIndex ai bp
            return (i, set0100)
    encodeI wbuf set 4 idx
    encodeS wbuf huff id set1 7 val

-- 4.5.5.  Literal Field Line With Post-Base Name Reference
-- not implemented

-- 4.5.6.  Literal Field Line with Literal Name
encodeLiteralFieldLineWithLiteralName
    :: WriteBuffer -> Token -> ByteString -> Bool -> IO ()
encodeLiteralFieldLineWithLiteralName wbuf token val huff = do
    let key = tokenFoldedKey token
    encodeS wbuf huff set0010 set00001 3 key
    encodeS wbuf huff id set1 7 val
