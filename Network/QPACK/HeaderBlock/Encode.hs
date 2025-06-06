{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.QPACK.HeaderBlock.Encode (
    encodeHeader,
    encodeTokenHeader,
    EncodedFieldSection,
    EncodedEncoderInstruction,
) where

import Control.Concurrent.STM
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Network.ByteOrder
import Network.Control
import Network.HPACK.Internal (
    encodeI,
    encodeS,
    entryFieldValue,
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
    if ready
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
        putStrLn $ "    Table size: " ++ show tblsiz
        putStr "    "
        printReferences dyntbl
        putStrLn $ show rr ++ ": " ++ show (tokenKey t) ++ " " ++ show val ++ ""
    case rr of
        KV hi@(SIndex _) -> do
            -- 4.5.2.  Indexed Field Line
            encodeIndexedFieldLine wbuf1 dyntbl hi
            return Nothing
        KV hi@(DIndex ai) -> do
            qpackDebug dyntbl $ checkAbsoluteIndex dyntbl ai "KV (1)"
            withDIndex ai $ do
                -- 4.5.2.  Indexed Field Line
                encodeIndexedFieldLine wbuf1 dyntbl hi
                increaseReference dyntbl ai
                return $ Just ai
        K hi@(SIndex i) -> tryInsertVal hi $ do
            insertWithNameReference val ent Nothing $ Left i
        K hi@(DIndex ai) -> do
            qpackDebug dyntbl $ checkAbsoluteIndex dyntbl ai "K (1)"
            withDIndex ai $ tryInsertVal hi $ do
                ridx <- toInsRelativeIndex ai <$> getInsertionPoint dyntbl
                insertWithNameReference val ent (Just ai) $ Right ridx
        N -> tryInsertKeyVal $ insertWithLiteralName val ent
  where
    ent = toEntryToken t val
    key = tokenFoldedKey t
    lru = getLruCache dyntbl

    withDIndex ai action = do
        blocked <- wouldInstructionBeBlocked dyntbl ai
        canUseDynamicTable <- checkBlockedStreams dyntbl
        if canUseDynamicTable || not blocked
            then action
            else encodeLiteralFieldLineStatic

    insertWithNameReference v e mai insidx =
        insertWith mai e $ InsertWithNameReference insidx v

    insertWithLiteralName v e =
        insertWith Nothing e $ InsertWithLiteralName t v

    insertWith maiForKey e ins = do
        encodeEI wbuf2 True ins
        ai <- insertEntryToEncoder e dyntbl
        qpackDebug dyntbl $ putStrLn $ show ins ++ ": " ++ show ai
        canUseDynamicTable <- checkBlockedStreams dyntbl
        if canUseDynamicTable
            then do
                entVal <- atomically (entryFieldValue <$> toDynamicEntry dyntbl ai)
                case entVal of
                    "" -> do
                        -- 4.5.5.  Literal Field Line With Post-Base Name Reference
                        encodeLiteralFieldLineWithPostBaseNameReference wbuf1 dyntbl ai val True
                    _ -> do
                        -- 4.5.3.  Indexed Field Line with Post-Base Index
                        encodeIndexedFieldLineWithPostBaseIndex wbuf1 dyntbl ai
                increaseReference dyntbl ai
                return $ Just ai
            else encodeLiteralValue maiForKey

    encodeLiteralValue Nothing = encodeLiteralFieldLineStatic
    encodeLiteralValue (Just ai) = do
        blocked <- wouldInstructionBeBlocked dyntbl ai
        canUseDynamicTable <- checkBlockedStreams dyntbl
        if canUseDynamicTable || not blocked
            then do
                -- 4.5.4.  Literal Field Line With Name Reference
                encodeLiteralFieldLineWithNameReference wbuf1 dyntbl (DIndex ai) val huff
                increaseReference dyntbl ai
                return $ Just ai
            else encodeLiteralFieldLineStatic

    tryInsertVal hi action = do
        let possiblelyDropMySelf = case hi of
                SIndex _ -> Nothing
                DIndex ai -> Just ai
        ok <- checkExistenceAndSpace ent key val possiblelyDropMySelf "Val"
        if ok
            then action
            else do
                -- 4.5.4.  Literal Field Line With Name Reference
                encodeLiteralFieldLineWithNameReference wbuf1 dyntbl hi val huff
                case hi of
                    SIndex _ -> return Nothing
                    DIndex dai -> do
                        increaseReference dyntbl dai
                        return $ Just dai

    tryInsertKeyVal action = do
        ok <- checkExistenceAndSpace ent key val Nothing "KeyVal"
        case ok of
            True -> action
            False -> tryInsertKey

    tryInsertKey
        | isJust (tokenToStaticIndex t) = encodeLiteralFieldLineStatic
        | otherwise = do
            mdai <- isKeyRegistered key revidx
            case mdai of
                Nothing -> do
                    let val' = ""
                        ent' = toEntryToken t val'
                    okK <- checkExistenceAndSpace ent' key val' Nothing "Key"
                    if okK
                        then insertWithLiteralName val' ent'
                        else encodeLiteralFieldLineStatic
                Just dai -> encodeLiteralFieldLineDynamic dai

    encodeLiteralFieldLineDynamic dai = do
        canUseDynamicTable <- checkBlockedStreams dyntbl
        if canUseDynamicTable
            then do
                -- 4.5.4.  Literal Field Line With Name Reference
                encodeLiteralFieldLineWithNameReference wbuf1 dyntbl (DIndex dai) val huff
                increaseReference dyntbl dai
                return $ Just dai
            else do
                -- 4.5.6.  Literal Field Line with Literal Name
                encodeLiteralFieldLineWithLiteralName wbuf1 dyntbl t val huff
                return Nothing

    encodeLiteralFieldLineStatic = do
        case tokenToStaticIndex t of
            Just i -> do
                -- 4.5.4.  Literal Field Line With Name Reference
                encodeLiteralFieldLineWithNameReference wbuf1 dyntbl (SIndex i) val huff
            Nothing -> do
                -- 4.5.6.  Literal Field Line with Literal Name
                encodeLiteralFieldLineWithLiteralName wbuf1 dyntbl t val huff
        return Nothing

    checkExistence k v tag = do
        (_, exist) <- cached lru (k, v) (return ())
        qpackDebug dyntbl $
            putStrLn $
                (if exist then "    HIT for " ++ tag else "    not HIT for " ++ tag)
                    ++ " "
                    ++ show k
                    ++ " "
                    ++ show v
        return exist

    checkSpace e possiblelyDropMySelf tag = do
        spaceOK <- canInsertEntry dyntbl e possiblelyDropMySelf
        unless spaceOK $ do
            adjustDrainingPoint dyntbl
            qpackDebug dyntbl $ putStrLn $ "    NO SPACE for " ++ tag
        return spaceOK

    checkExistenceAndSpace e k v possiblelyDropMySelf tag = do
        exist <- checkExistence k v tag
        if exist
            then checkSpace e possiblelyDropMySelf tag
            else return False

-- 4.5.2.  Indexed Field Line
encodeIndexedFieldLine :: WriteBuffer -> DynamicTable -> HIndex -> IO ()
encodeIndexedFieldLine wbuf dyntbl hi = do
    (idx, set) <- case hi of
        SIndex (AbsoluteIndex i) -> return (i, set11)
        DIndex ai -> do
            qpackDebug dyntbl $ checkAbsoluteIndex dyntbl ai "encodeIndexedFieldLine"
            updateRequiredInsertCount dyntbl ai
            bp <- getBasePoint dyntbl
            let PreBaseIndex i = toPreBaseIndex ai bp
            return (i, set10)
    encodeI wbuf set 6 idx
    qpackDebug dyntbl $ putStrLn $ "IndexedFieldLine (" ++ show hi ++ ")"

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
    :: WriteBuffer -> DynamicTable -> HIndex -> ByteString -> Bool -> IO ()
encodeLiteralFieldLineWithNameReference wbuf dyntbl hidx val huff = do
    (idx, set) <- case hidx of
        SIndex (AbsoluteIndex i) -> return (i, set0101)
        DIndex ai -> do
            updateRequiredInsertCount dyntbl ai
            bp <- getBasePoint dyntbl
            let PreBaseIndex i = toPreBaseIndex ai bp
            return (i, set0100)
    encodeI wbuf set 4 idx
    encodeS wbuf huff id set1 7 val
    qpackDebug dyntbl $
        putStrLn $
            "LiteralFieldLineWithNameReference (" ++ show hidx ++ ")"

-- 4.5.5.  Literal Field Line With Post-Base Name Reference
encodeLiteralFieldLineWithPostBaseNameReference
    :: WriteBuffer
    -> DynamicTable
    -> AbsoluteIndex -- in Dynamic table
    -> ByteString
    -> Bool
    -> IO ()
encodeLiteralFieldLineWithPostBaseNameReference wbuf dyntbl ai val huff = do
    updateRequiredInsertCount dyntbl ai
    bp <- getBasePoint dyntbl
    let PostBaseIndex idx = toPostBaseIndex ai bp
    encodeI wbuf set00000 3 idx
    encodeS wbuf huff id set1 7 val
    qpackDebug dyntbl $
        putStrLn $
            "LiteralFieldLineWithPostBaseNameReference (DIndex " ++ show ai ++ ")"

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
