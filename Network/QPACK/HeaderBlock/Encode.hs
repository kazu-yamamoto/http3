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
            withDIndex ai "KV (2)" $ do
                -- 4.5.2.  Indexed Field Line
                encodeIndexedFieldLine wbuf1 dyntbl hi
                increaseReference dyntbl ai
                return $ Just ai
        K (SIndex i) -> tryInsert $ do
            insertWithNameReference val ent $ Left i
        K (DIndex ai) -> do
            qpackDebug dyntbl $ checkAbsoluteIndex dyntbl ai "K (1)"
            withDIndex ai "K (2)" $ tryInsert $ do
                ridx <- toInsRelativeIndex ai <$> getInsertionPoint dyntbl
                insertWithNameReference val ent $ Right ridx
        N -> tryInsert $ insertWithLiteralName val ent
  where
    ent = toEntryToken t val
    key = tokenFoldedKey t
    lru = getLruCache dyntbl

    withDIndex ai tag action = do
        blocked <- wouldInstructionBeBlocked dyntbl ai
        canUseDynamicTable <- checkBlockedStreams dyntbl
        if canUseDynamicTable || not blocked
            then maybeDuplicate ai tag action
            else encodeLiteralFieldLineStatic

    insertWithNameReference v e insidx =
        insertWith e $ InsertWithNameReference insidx v

    insertWithLiteralName v e =
        insertWith e $ InsertWithLiteralName t v

    insertWith e ins = do
        encodeEI wbuf2 True ins
        dai <- insertEntryToEncoder e dyntbl
        qpackDebug dyntbl $ putStrLn $ show ins ++ ": " ++ show dai
        useInsertedOrLiteral dai

    -- directly call this with SIndex
    tryInsert action = do
        (_, exist) <- cached lru (key, val) (return ())
        qpackDebug dyntbl $
            putStrLn $
                (if exist then "    HIT" else "    not HIT")
                    ++ " "
                    ++ show key
                    ++ " "
                    ++ show val
        ok <-
            if exist
                then do
                    spaceOK <- canInsertEntry dyntbl ent
                    unless spaceOK $ qpackDebug dyntbl $ putStrLn "    NO SPACE"
                    return spaceOK
                else return False
        if ok
            then action
            else tryInsertKey

    tryInsertKey
        | tokenToStaticIndex t == Nothing = do
            mdai <- isKeyRegistered key revidx
            case mdai of
                Just dai -> do
                    draining <- isDraining dyntbl dai
                    if draining
                        then encodeLiteralFieldLineStatic
                        else encodeLiteralFieldLineDynamic dai
                Nothing -> do
                    let val' = ""
                    (_, exist) <- cached lru (key, val') (return ())
                    qpackDebug dyntbl $
                        putStrLn $
                            (if exist then "    HIT for Key" else "    not HIT for Key") ++ " " ++ show key
                    let ent' = toEntryToken t val'
                    ok <-
                        if exist
                            then do
                                spaceOK <- canInsertEntry dyntbl ent'
                                unless spaceOK $ qpackDebug dyntbl $ putStrLn "    NO SPACE for Key"
                                return spaceOK
                            else return False
                    if ok
                        then insertWithLiteralName val' ent'
                        else encodeLiteralFieldLineStatic
        | otherwise = encodeLiteralFieldLineStatic

    -- call this with DIndex
    maybeDuplicate ai tag action = do
        draining <- isDraining dyntbl ai
        if draining
            then do
                qpackDebug dyntbl $ putStrLn "DRAINING"
                tryInsert $ do
                    ridx <- toInsRelativeIndex ai <$> getInsertionPoint dyntbl
                    let ins = Duplicate ridx
                    encodeEI wbuf2 True ins
                    ai' <- duplicate dyntbl ai
                    qpackDebug dyntbl $ do
                        checkAbsoluteIndex dyntbl ai' tag
                        putStrLn $
                            show ins ++ ": " ++ show ai ++ " -> " ++ show ai'

                    useInsertedOrLiteral ai'
            else action

    useInsertedOrLiteral ai = do
        notBlocked <- checkBlockedStreams dyntbl
        if notBlocked
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
            else
                encodeLiteralFieldLineStatic

    encodeLiteralFieldLineDynamic dai = do
        canUseDynamicTable <- checkBlockedStreams dyntbl
        if canUseDynamicTable
            then do
                -- 4.5.4.  Literal Field Line With Name Reference
                encodeLiteralFieldLineWithNameReference wbuf1 dyntbl (DIndex dai) val huff

                return $ Just dai
            else do
                -- 4.5.6.  Literal Field Line with Literal Name
                encodeLiteralFieldLineWithLiteralName wbuf1 dyntbl t val huff
                return Nothing

    encodeLiteralFieldLineStatic = do
        case tokenToStaticIndex t of
            Nothing -> do
                -- 4.5.6.  Literal Field Line with Literal Name
                encodeLiteralFieldLineWithLiteralName wbuf1 dyntbl t val huff
                adjustDrainingPoint dyntbl
            Just i -> do
                -- 4.5.4.  Literal Field Line With Name Reference
                encodeLiteralFieldLineWithNameReference wbuf1 dyntbl (SIndex i) val huff
        return Nothing

-- 4.5.2.  Indexed Field Line
encodeIndexedFieldLine :: WriteBuffer -> DynamicTable -> HIndex -> IO ()
encodeIndexedFieldLine wbuf dyntbl hi = do
    (idx, set) <- case hi of
        SIndex (AbsoluteIndex i) -> return (i, set11)
        DIndex ai -> do
            qpackDebug dyntbl $ checkAbsoluteIndex dyntbl ai "encodeIndexedFieldLine"
            updateRequiredInsertCount dyntbl ai
            bp <- getBasePoint dyntbl
            let HBRelativeIndex i = toHBRelativeIndex ai bp
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
            let HBRelativeIndex i = toHBRelativeIndex ai bp
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
