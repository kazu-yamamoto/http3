{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.QPACK.Types where

newtype AbsoluteIndex    = AbsoluteIndex    Int deriving (Eq, Ord, Show, Num)
newtype InsRelativeIndex = InsRelativeIndex Int deriving (Eq, Ord, Show, Num)
newtype HBRelativeIndex  = HBRelativeIndex  Int deriving (Eq, Ord, Show, Num)
newtype PostBaseIndex    = PostBaseIndex    Int deriving (Eq, Ord, Show, Num)

type InsertPoint = AbsoluteIndex
type BasePoint   = AbsoluteIndex

data DynamicIndex = DynamicIndex AbsoluteIndex InsertPoint BasePoint
    deriving (Eq, Ord, Show)

data AIndex = SAIndex AbsoluteIndex
            | DAIndex DynamicIndex
            deriving (Eq, Ord, Show)

data IIndex = SIIndex AbsoluteIndex
            | DIIndex InsRelativeIndex
            deriving (Eq, Ord, Show)

data HIndex = SHIndex AbsoluteIndex
            | DHIndex HBRelativeIndex
            deriving (Eq, Ord, Show)

{-
    Dropping    Draining Index               Insertion Point
      |          |                                 |
      v          v                                 v
      +----------+---------------------------------+--------+
      | Draining |          Referenceable          | Unused |
      | Entries  |             Entries             | Space  |
      +----------+---------------------------------+--------+
|  d  |                            |n-4|n-3|n-2|n-1|          Absolute
|n-d-1|                            | 3 | 2 | 1 | 0 |          Relative ins
|n-d-3|                            | 1 | 0 |                  Relative HB
                                           | 0 | 1 |          Post-Base
                                           ^
                                           |
                                          Base = n - 2

                                                   ip = 100
|  d  |                            | 96| 97| 98| 99|          Absolute
|n-d-1|                            | 3 | 2 | 1 | 0 |          Relative ins
|n-d-3|                            | 1 | 0 |                  Relative HB
                                           | 0 | 1 |          Post-Base
                                           bp = 98
-}

-- |
--
-- >>> toInsRelativeIndex $ DynamicIndex 99 100 98
-- InsRelativeIndex 0
-- >>> toInsRelativeIndex $ DynamicIndex 98 100 98
-- InsRelativeIndex 1
-- >>> toInsRelativeIndex $ DynamicIndex 97 100 98
-- InsRelativeIndex 2
-- >>> toInsRelativeIndex $ DynamicIndex 96 100 98
-- InsRelativeIndex 3
toInsRelativeIndex :: DynamicIndex -> InsRelativeIndex
toInsRelativeIndex (DynamicIndex (AbsoluteIndex idx) (AbsoluteIndex ip) _) =
    InsRelativeIndex (ip - idx - 1)

-- |
--
-- >>> fromInsRelativeIndex 0 100
-- AbsoluteIndex 99
-- >>> fromInsRelativeIndex 1 100
-- AbsoluteIndex 98
-- >>> fromInsRelativeIndex 2 100
-- AbsoluteIndex 97
-- >>> fromInsRelativeIndex 3 100
-- AbsoluteIndex 96
fromInsRelativeIndex :: InsRelativeIndex -> InsertPoint -> AbsoluteIndex
fromInsRelativeIndex (InsRelativeIndex ri) (AbsoluteIndex ip) =
    AbsoluteIndex (ip - ri - 1)

-- |
--
-- >>> toHBRelativeIndex $ DynamicIndex 96 100 98
-- HBRelativeIndex 1
-- >>> toHBRelativeIndex $ DynamicIndex 97 100 98
-- HBRelativeIndex 0
toHBRelativeIndex :: DynamicIndex -> HBRelativeIndex
toHBRelativeIndex (DynamicIndex (AbsoluteIndex idx) _ (AbsoluteIndex bp)) =
    HBRelativeIndex (bp - idx - 1)

-- |
--
-- >>> fromHBRelativeIndex 1 98
-- AbsoluteIndex 96
-- >>> fromHBRelativeIndex 0 98
-- AbsoluteIndex 97
fromHBRelativeIndex :: HBRelativeIndex -> BasePoint -> AbsoluteIndex
fromHBRelativeIndex (HBRelativeIndex ri) (AbsoluteIndex bp) =
    AbsoluteIndex (bp - ri - 1)

-- |
--
-- >>> toPostBaseIndex $ DynamicIndex 98 100 98
-- PostBaseIndex 0
-- >>> toPostBaseIndex $ DynamicIndex 99 100 98
-- PostBaseIndex 1
toPostBaseIndex :: DynamicIndex -> PostBaseIndex
toPostBaseIndex (DynamicIndex (AbsoluteIndex idx) _ (AbsoluteIndex bp)) =
    PostBaseIndex (idx - bp)

-- |
--
-- >>> fromPostBaseIndex 0 98
-- AbsoluteIndex 98
-- >>> fromPostBaseIndex 1 98
-- AbsoluteIndex 99
fromPostBaseIndex :: PostBaseIndex -> BasePoint -> AbsoluteIndex
fromPostBaseIndex (PostBaseIndex pix) (AbsoluteIndex bp) =
    AbsoluteIndex (pix + bp)
