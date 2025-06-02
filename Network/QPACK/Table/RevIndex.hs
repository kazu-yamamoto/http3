{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.QPACK.Table.RevIndex (
    RevResult (..),
    RevIndex,
    newRevIndex,
    renewRevIndex,
    lookupRevIndex,
    lookupRevIndex',
    insertRevIndex,
    deleteRevIndex,
    tokenToStaticIndex,
    isKeyRegistered,
    lookupRevIndexS,
) where

import Data.Array (Array)
import qualified Data.Array as A
import Data.Array.Base (unsafeAt)
import Data.Function (on)
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Network.HPACK.Internal (Entry (..))
import Network.HTTP.Semantics

import Imports
import Network.QPACK.Table.Static
import Network.QPACK.Token
import Network.QPACK.Types

----------------------------------------------------------------

data RevResult
    = N
    | K HIndex
    | KV HIndex
    deriving (Eq, Show)

----------------------------------------------------------------

data RevIndex = RevIndex DynamicRevIndex OtherRevIndex

----------------------------------------------------------------

type DynamicRevIndex = Array Int (IORef DynamicValueMap)

type DynamicValueMap = Map FieldValue AbsoluteIndex

----------------------------------------------------------------

type OtherRevIndex = IORef (Map FieldName OtherValueMap) -- dynamic table only

-- Priority is negated AbsoluteIndex to find the largest
type OtherValueMap = OrdPSQ FieldValue Int AbsoluteIndex

----------------------------------------------------------------

type StaticRevIndex = Array Int StaticEntry

data StaticEntry = StaticEntry AbsoluteIndex (Maybe StaticValueMap)
    deriving (Show)

type StaticValueMap = Map FieldValue AbsoluteIndex

----------------------------------------------------------------

staticRevIndex :: StaticRevIndex
staticRevIndex = A.array (minTokenIx, 51) $ map toEnt zs
  where
    toEnt (k, xs) = (quicIx $ tokenIx $ toToken $ foldedCase k, m)
      where
        m = case xs of
            ("", i) :| [] -> StaticEntry i Nothing
            (_, i) :| _ -> StaticEntry i $ Just $ M.fromList $ NE.toList xs
    zs = map extract $ NE.groupBy ((==) `on` fst) $ sort lst
      where
        lst =
            zipWith (\(k, v) i -> (k, (v, i))) staticTableList $
                map AbsoluteIndex [0 ..]
        extract xs = (fst (NE.head xs), NE.map snd xs)

{-# INLINE lookupStaticRevIndex #-}
lookupStaticRevIndex :: Int -> FieldValue -> RevResult
lookupStaticRevIndex ix v = case staticRevIndex `unsafeAt` ix of
    StaticEntry i Nothing -> K $ SIndex i
    StaticEntry i (Just m) -> case M.lookup v m of
        Nothing -> K $ SIndex i
        Just j -> KV $ SIndex j

lookupRevIndexS
    :: Token
    -> FieldValue
    -> RevResult
lookupRevIndexS Token{..} v
    | ix < 0 = N
    | otherwise = lookupStaticRevIndex ix v
  where
    ix = quicIx tokenIx

----------------------------------------------------------------

newDynamicRevIndex :: IO DynamicRevIndex
newDynamicRevIndex = A.listArray (minTokenIx, maxStaticTokenIx) <$> mapM mk' lst
  where
    mk' _ = newIORef M.empty
    lst = [minTokenIx .. maxStaticTokenIx]

renewDynamicRevIndex :: DynamicRevIndex -> IO ()
renewDynamicRevIndex drev = mapM_ clear [minTokenIx .. maxStaticTokenIx]
  where
    clear t = writeIORef (drev `unsafeAt` t) M.empty

{-# INLINE lookupDynamicStaticRevIndex #-}
lookupDynamicStaticRevIndex
    :: Int -> FieldValue -> DynamicRevIndex -> IO RevResult
lookupDynamicStaticRevIndex ix v drev = do
    let ref = drev `unsafeAt` ix
    m <- readIORef ref
    case M.lookup v m of
        Just i -> return $ KV $ DIndex i
        Nothing -> return $ lookupStaticRevIndex ix v

{-# INLINE insertDynamicRevIndex #-}
insertDynamicRevIndex
    :: Token -> FieldValue -> AbsoluteIndex -> DynamicRevIndex -> IO ()
insertDynamicRevIndex t v i drev = modifyIORef ref $ M.insert v i
  where
    ref = drev `unsafeAt` quicIx (tokenIx t)

{-# INLINE deleteDynamicRevIndex #-}
deleteDynamicRevIndex
    :: Token -> FieldValue -> AbsoluteIndex -> DynamicRevIndex -> IO ()
deleteDynamicRevIndex t v ai drev = modifyIORef ref $ M.alter adjust v
  where
    ref = drev `unsafeAt` quicIx (tokenIx t)
    adjust Nothing = Nothing
    adjust x@(Just ai')
        | ai == ai' = Nothing
        -- This previous entry is already deleted by "duplicate"
        | otherwise = x

----------------------------------------------------------------

newOtherRevIndex :: IO OtherRevIndex
newOtherRevIndex = newIORef M.empty

renewOtherRevIndex :: OtherRevIndex -> IO ()
renewOtherRevIndex ref = writeIORef ref M.empty

{-# INLINE lookupOtherRevIndex #-}
lookupOtherRevIndex :: (FieldName, FieldValue) -> OtherRevIndex -> IO RevResult
lookupOtherRevIndex (k, v) ref = do
    oth <- readIORef ref
    case M.lookup k oth of
        Nothing -> return N
        Just psq -> case PSQ.lookup v psq of
            Nothing -> case PSQ.findMin psq of
                Nothing -> return N
                Just (_, _, ai) -> return $ K $ DIndex ai
            Just (_, i) -> return $ KV $ DIndex i

isKeyRegistered :: FieldName -> RevIndex -> IO (Maybe AbsoluteIndex)
isKeyRegistered k (RevIndex _ ref) = do
    oth <- readIORef ref
    return $ case M.lookup k oth of
        Nothing -> Nothing
        Just psq -> case PSQ.findMin psq of
            Nothing -> Nothing
            Just (_, _, ai) -> Just ai

{-# INLINE insertOtherRevIndex #-}
insertOtherRevIndex
    :: Token -> FieldValue -> AbsoluteIndex -> OtherRevIndex -> IO ()
insertOtherRevIndex t v ai@(AbsoluteIndex i) ref = modifyIORef' ref $ M.alter adjust k
  where
    adjust Nothing = Just $ PSQ.singleton v (negate i) ai
    adjust (Just psq) = Just $ PSQ.insert v (negate i) ai psq
    k = tokenFoldedKey t

{-# INLINE deleteOtherRevIndex #-}
deleteOtherRevIndex
    :: Token -> FieldValue -> AbsoluteIndex -> OtherRevIndex -> IO ()
deleteOtherRevIndex t v ai ref = modifyIORef' ref $ M.alter adjust k
  where
    k = tokenFoldedKey t
    adjust Nothing =
        error $
            "deleteOtherRevIndex "
                ++ show (tokenFoldedKey t)
                ++ " "
                ++ show v
                ++ " "
                ++ show ai
    adjust (Just psq)
        | PSQ.null psq' = Nothing
        | otherwise = Just psq'
      where
        psq' = snd $ PSQ.alter adj v psq
        adj x@(Just (_, ai'))
            | ai == ai' = ((), Nothing)
            -- This previous entry is already deleted by "duplicate"
            | otherwise = ((), x)
        adj Nothing =
            error $
                "deleteOtherRevIndex (2) "
                    ++ show (tokenFoldedKey t)
                    ++ " "
                    ++ show v
                    ++ " "
                    ++ show ai

----------------------------------------------------------------

newRevIndex :: IO RevIndex
newRevIndex = RevIndex <$> newDynamicRevIndex <*> newOtherRevIndex

renewRevIndex :: RevIndex -> IO ()
renewRevIndex (RevIndex dyn oth) = do
    renewDynamicRevIndex dyn
    renewOtherRevIndex oth

{-# INLINE lookupRevIndex #-}
lookupRevIndex
    :: Token
    -> FieldValue
    -> RevIndex
    -> IO RevResult
lookupRevIndex t@Token{..} v (RevIndex dyn oth)
    | ix < 0 = lookupOtherRevIndex (k, v) oth
    | shouldBeIndexed = lookupDynamicStaticRevIndex ix v dyn
    -- path: is not indexed but ":path /" should be used, sigh.
    | otherwise = return $ lookupStaticRevIndex ix v
  where
    ix = quicIx tokenIx
    k = tokenFoldedKey t

--    ent = toEntryToken t v -- fixme

{-# INLINE lookupRevIndex' #-}
lookupRevIndex'
    :: Token
    -> FieldValue
    -> RevResult
lookupRevIndex' Token{..} v
    | ix >= 0 = lookupStaticRevIndex ix v
    | otherwise = N -- fixme
  where
    ix = quicIx tokenIx

--    k = tokenFoldedKey t -- fixme

tokenToStaticIndex :: Token -> Maybe AbsoluteIndex
tokenToStaticIndex Token{..}
    | ix >= 0 = case staticRevIndex `unsafeAt` ix of
        StaticEntry i _ -> Just i
    | otherwise = Nothing
  where
    ix = quicIx tokenIx

----------------------------------------------------------------

{-# INLINE insertRevIndex #-}
insertRevIndex :: Entry -> AbsoluteIndex -> RevIndex -> IO ()
insertRevIndex (Entry _ t v) i (RevIndex dyn oth)
    | quicIx (tokenIx t) >= 0 = insertDynamicRevIndex t v i dyn
    | otherwise = insertOtherRevIndex t v i oth

{-# INLINE deleteRevIndex #-}
deleteRevIndex :: RevIndex -> Entry -> AbsoluteIndex -> IO ()
deleteRevIndex (RevIndex dyn oth) (Entry _ t v) ai
    | quicIx (tokenIx t) >= 0 = deleteDynamicRevIndex t v ai dyn
    | otherwise = deleteOtherRevIndex t v ai oth
