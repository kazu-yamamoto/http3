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
    deleteRevIndexList,
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
import Network.HPACK.Internal (Entry (..))
import Network.HTTP.Semantics

import Imports
import Network.QPACK.Table.Static
import Network.QPACK.Token
import Network.QPACK.Types

----------------------------------------------------------------

data RevResult = N | K HIndex | KV HIndex deriving (Eq, Show)

----------------------------------------------------------------

data RevIndex = RevIndex DynamicRevIndex OtherRevIdex

type DynamicRevIndex = Array Int (IORef ValueMap)

data KeyValue = KeyValue FieldName FieldValue deriving (Eq, Ord)

-- We always create an index for a pair of an unknown header and its value
-- in Linear{H}.
type OtherRevIdex = IORef (Map KeyValue HIndex)

{-# SPECIALIZE INLINE M.lookup ::
    KeyValue -> M.Map KeyValue HIndex -> Maybe HIndex
    #-}
{-# SPECIALIZE INLINE M.delete ::
    KeyValue -> M.Map KeyValue HIndex -> M.Map KeyValue HIndex
    #-}
{-# SPECIALIZE INLINE M.insert ::
    KeyValue -> HIndex -> M.Map KeyValue HIndex -> M.Map KeyValue HIndex
    #-}

----------------------------------------------------------------

type StaticRevIndex = Array Int StaticEntry

data StaticEntry = StaticEntry HIndex (Maybe ValueMap) deriving (Show)

type ValueMap = Map FieldValue HIndex

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
                map (SIndex . AbsoluteIndex) [0 ..]
        extract xs = (fst (NE.head xs), NE.map snd xs)

{-# INLINE lookupStaticRevIndex #-}
lookupStaticRevIndex :: Int -> FieldValue -> RevResult
lookupStaticRevIndex ix v = case staticRevIndex `unsafeAt` ix of
    StaticEntry i Nothing -> K i
    StaticEntry i (Just m) -> case M.lookup v m of
        Nothing -> K i
        Just j -> KV j

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
        Just i -> return $ KV i
        Nothing -> return $ lookupStaticRevIndex ix v

{-# INLINE insertDynamicRevIndex #-}
insertDynamicRevIndex
    :: Token -> FieldValue -> HIndex -> DynamicRevIndex -> IO ()
insertDynamicRevIndex t v i drev = modifyIORef ref $ M.insert v i
  where
    ref = drev `unsafeAt` quicIx (tokenIx t)

{-# INLINE deleteDynamicRevIndex #-}
deleteDynamicRevIndex :: Token -> FieldValue -> DynamicRevIndex -> IO ()
deleteDynamicRevIndex t v drev = modifyIORef ref $ M.delete v
  where
    ref = drev `unsafeAt` quicIx (tokenIx t)

----------------------------------------------------------------

newOtherRevIndex :: IO OtherRevIdex
newOtherRevIndex = newIORef M.empty

renewOtherRevIndex :: OtherRevIdex -> IO ()
renewOtherRevIndex ref = writeIORef ref M.empty

{-# INLINE lookupOtherRevIndex #-}
lookupOtherRevIndex :: (FieldName, FieldValue) -> OtherRevIdex -> IO RevResult
lookupOtherRevIndex (k, v) ref = do
    oth <- readIORef ref
    case M.lookup (KeyValue k v) oth of
        Nothing -> return N
        Just i -> return $ KV i

{-# INLINE insertOtherRevIndex #-}
insertOtherRevIndex :: Token -> FieldValue -> HIndex -> OtherRevIdex -> IO ()
insertOtherRevIndex t v i ref = modifyIORef' ref $ M.insert (KeyValue k v) i
  where
    k = tokenFoldedKey t

{-# INLINE deleteOtherRevIndex #-}
deleteOtherRevIndex :: Token -> FieldValue -> OtherRevIdex -> IO ()
deleteOtherRevIndex t v ref = modifyIORef' ref $ M.delete (KeyValue k v)
  where
    k = tokenFoldedKey t

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

----------------------------------------------------------------

{-# INLINE insertRevIndex #-}
insertRevIndex :: Entry -> HIndex -> RevIndex -> IO ()
insertRevIndex (Entry _ t v) i (RevIndex dyn oth)
    | quicIx (tokenIx t) >= 0 = insertDynamicRevIndex t v i dyn
    | otherwise = insertOtherRevIndex t v i oth

{-# INLINE deleteRevIndex #-}
deleteRevIndex :: RevIndex -> Entry -> IO ()
deleteRevIndex (RevIndex dyn oth) (Entry _ t v)
    | quicIx (tokenIx t) >= 0 = deleteDynamicRevIndex t v dyn
    | otherwise = deleteOtherRevIndex t v oth

{-# INLINE deleteRevIndexList #-}
deleteRevIndexList :: [Entry] -> RevIndex -> IO ()
deleteRevIndexList es rev = mapM_ (deleteRevIndex rev) es
