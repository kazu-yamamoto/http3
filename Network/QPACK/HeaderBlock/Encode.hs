{-# LANGUAGE BinaryLiterals #-}

module Network.QPACK.HeaderBlock.Encode (
    encodeHeader,
    encodeTokenHeader,
    EncodedFieldSection,
    EncodedEncoderInstruction,
) where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Network.ByteOrder
import Network.Control
import Network.HPACK.Internal (
    encodeI,
    encodeS,
    entrySize,
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
--   If headers are too large, 'BufferOverrun' is thrown.
encodeHeader
    :: DynamicTable
    -> [Header]
    -> IO (EncodedFieldSection, EncodedEncoderInstruction)
encodeHeader dyntbl hs = do
    setBasePointToInsersionPoint dyntbl
    clearRequiredInsertCount dyntbl
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
--  'TokenHeaderList' must be smaller than or equal to workspaces.
--  Otherwise, 'BufferOverrun' is thrown.
encodeTokenHeader
    :: WriteBuffer
    -- ^ Workspace for the body of header block
    -> WriteBuffer
    -- ^ Workspace for encoder instructions
    -> DynamicTable
    -> TokenHeaderList
    -> IO [AbsoluteIndex]
encodeTokenHeader wbuf1 wbuf2 dyntbl ts0 = do
    clearWriteBuffer wbuf1
    clearWriteBuffer wbuf2
    let revidx = getRevIndex dyntbl
    ready <- isTableReady dyntbl
    maxBlocked <- getMaxBlockedStreams dyntbl
    blocked <- getPossiblyBlocked dyntbl
    immACK <- getImmediateAck dyntbl
    -- this one would be blocked, so <, not <=
    if ready && (blocked < maxBlocked || immACK)
        then encodeLinear wbuf1 wbuf2 dyntbl revidx True ts0
        else encodeStatic wbuf1 wbuf2 dyntbl revidx True ts0

encodeStatic
    :: WriteBuffer
    -> WriteBuffer
    -> DynamicTable
    -> RevIndex
    -> Bool
    -> TokenHeaderList
    -> IO [AbsoluteIndex]
encodeStatic wbuf1 _wbuf2 dyntbl revidx huff ts0 = do
    mapM_ (encStatic wbuf1 dyntbl revidx huff) ts0
    return []

encStatic
    :: WriteBuffer -> DynamicTable -> RevIndex -> Bool -> (Token, FieldValue) -> IO ()
encStatic wbuf1 dyntbl revidx huff (t, val) = do
    rr <- lookupRevIndex t val revidx
    case rr of
        KV hi -> do
            -- 4.5.2.  Indexed Field Line
            encodeIndexedFieldLine wbuf1 dyntbl hi
        K i -> do
            -- 4.5.4.  Literal Field Line With Name Reference
            encodeLiteralFieldLineWithNameReference wbuf1 dyntbl i val huff
        N -> do
            -- 4.5.6.  Literal Field Line with Literal Name
            encodeLiteralFieldLineWithLiteralName wbuf1 dyntbl t val huff

encodeLinear
    :: WriteBuffer
    -> WriteBuffer
    -> DynamicTable
    -> RevIndex
    -> Bool
    -> TokenHeaderList
    -> IO [AbsoluteIndex]
encodeLinear wbuf1 wbuf2 dyntbl revidx huff ts0 =
    catMaybes <$> mapM (encLinear wbuf1 wbuf2 dyntbl revidx huff) ts0

encLinear
    :: WriteBuffer
    -> WriteBuffer
    -> DynamicTable
    -> RevIndex
    -> Bool
    -> (Token, FieldValue)
    -> IO (Maybe AbsoluteIndex)
encLinear wbuf1 wbuf2 dyntbl revidx huff (t, val) = do
    rr <- lookupRevIndex t val revidx
    qpackDebug dyntbl $ do
        tblsiz <- getTableCapacity dyntbl
        putStrLn $ "Table size: " ++ show tblsiz
        printReferences dyntbl
        putStrLn $
            show rr ++ ": " ++ show (tokenKey t) ++ " " ++ show val
    immACK <- getImmediateAck dyntbl
    case rr of
        KV hi@(SIndex _) -> do
            -- 4.5.2.  Indexed Field Line
            encodeIndexedFieldLine wbuf1 dyntbl hi
            return Nothing
        KV hi@(DIndex ai) -> do
            draining <- isDraining dyntbl ai
            if draining
                then do
                    qpackDebug dyntbl $ putStrLn "DRAINING"
                    tryInsert Nothing $ do
                        ridx <- toInsRelativeIndex ai <$> getInsertionPoint dyntbl
                        let ins = Duplicate ridx
                        encodeEI wbuf2 True ins
                        ai' <- duplicate dyntbl hi
                        qpackDebug dyntbl $ putStrLn $ show ins ++ " " ++ show ai'
                        -- 4.5.3.  Indexed Field Line with Post-Base Index
                        encodeIndexedFieldLineWithPostBaseIndex wbuf1 dyntbl ai'
                        increaseReference dyntbl ai'
                        when immACK $ setInsersionPointToKnownReceivedCount dyntbl
                        return $ Just ai'
                else do
                    -- 4.5.2.  Indexed Field Line
                    encodeIndexedFieldLine wbuf1 dyntbl hi
                    increaseReference dyntbl ai
                    when immACK $ setInsersionPointToKnownReceivedCount dyntbl
                    return $ Just ai
        K i -> tryInsert (Just i) $ do
            let ins = InsertWithNameReference (Left i) val
            encodeEI wbuf2 True ins
            dai <- insertEntryToEncoder ent dyntbl
            qpackDebug dyntbl $ putStrLn $ show ins ++ " " ++ show dai
            -- 4.5.3.  Indexed Field Line With Post-Base Index
            encodeIndexedFieldLineWithPostBaseIndex wbuf1 dyntbl dai
            increaseReference dyntbl dai
            when immACK $ setInsersionPointToKnownReceivedCount dyntbl
            return $ Just dai
        N -> do
            tryInsert Nothing $ do
                let ins = InsertWithLiteralName t val
                encodeEI wbuf2 True ins
                dai <- insertEntryToEncoder ent dyntbl
                qpackDebug dyntbl $ putStrLn $ show ins ++ " " ++ show dai
                -- 4.5.3.  Indexed Field Line with Post-Base Index
                encodeIndexedFieldLineWithPostBaseIndex wbuf1 dyntbl dai
                increaseReference dyntbl dai
                return $ Just dai
  where
    ent = toEntryToken t val
    tryInsert mi action = do
        let lru = getLruCache dyntbl
        (_, exist) <- cached lru (tokenFoldedKey t, val) (return ())
        qpackDebug dyntbl $ putStrLn $ if exist then "HIT" else "not HIT"
        if exist
            then do
                okWithoutEviction <- canInsertEntry dyntbl ent
                if okWithoutEviction
                    then action
                    else do
                        tryDrop dyntbl $ entrySize ent
                        okWithEviction <- canInsertEntry dyntbl ent
                        if okWithEviction
                            then action
                            else do
                                qpackDebug dyntbl $ putStrLn "NO SPACE"
                                defaultAction mi
            else do
                let mi' = case mi of
                        Nothing -> tokenToStaticIndex t
                        Just x -> Just x
                defaultAction mi'
    defaultAction Nothing = do
        -- 4.5.6.  Literal Field Line with Literal Name
        encodeLiteralFieldLineWithLiteralName wbuf1 dyntbl t val huff
        adjustDrainingPoint dyntbl
        return Nothing
    defaultAction (Just i) = do
        -- 4.5.4.  Literal Field Line With Name Reference
        encodeLiteralFieldLineWithNameReference wbuf1 dyntbl i val huff
        return Nothing

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
    qpackDebug dyntbl $ putStrLn "IndexedFieldLine "

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
    qpackDebug dyntbl $ putStrLn "IndexedFieldLineWithPostBaseIndex "

-- 4.5.4.  Literal Field Line With Name Reference
encodeLiteralFieldLineWithNameReference
    :: WriteBuffer -> DynamicTable -> AbsoluteIndex -> ByteString -> Bool -> IO ()
encodeLiteralFieldLineWithNameReference wbuf dyntbl (AbsoluteIndex idx) val huff = do
    encodeI wbuf set0101 4 idx
    encodeS wbuf huff id set1 7 val
    qpackDebug dyntbl $ putStrLn $ "LiteralFieldLineWithNameReference "

-- 4.5.5.  Literal Field Line With Post-Base Name Reference
-- not implemented

-- 4.5.6.  Literal Field Line with Literal Name
encodeLiteralFieldLineWithLiteralName
    :: WriteBuffer -> DynamicTable -> Token -> ByteString -> Bool -> IO ()
encodeLiteralFieldLineWithLiteralName wbuf dyntbl token val huff = do
    let key = tokenFoldedKey token
    encodeS wbuf huff set0010 set00001 3 key
    encodeS wbuf huff id set1 7 val
    qpackDebug dyntbl $
        putStrLn $
            "LiteralFieldLineWithLiteralName " ++ showHeader key val

showHeader :: ByteString -> ByteString -> String
showHeader key val = "\"" ++ C8.unpack key ++ "\" \"" ++ C8.unpack val ++ "\""
