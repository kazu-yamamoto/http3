{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.QPACK.Types where

import Imports

newtype AbsoluteIndex = AbsoluteIndex Int deriving (Eq, Ord, Show, Num)
newtype InsRelativeIndex = InsRelativeIndex Int deriving (Eq, Ord, Show, Num)
newtype HBRelativeIndex = HBRelativeIndex Int deriving (Eq, Ord, Show, Num)
newtype PostBaseIndex = PostBaseIndex Int deriving (Eq, Ord, Show, Num)
newtype BasePoint = BasePoint Int deriving (Eq, Ord, Show, Num)

-- Point to insert an entry next
newtype InsertionPoint = InsertionPoint Int deriving (Eq, Ord, Show, Num)

-- Counter of how many entries must be inserted
newtype RequiredInsertCount = RequiredInsertCount Int
    deriving (Eq, Ord, Show, Num)

data HIndex
    = SIndex AbsoluteIndex
    | DIndex AbsoluteIndex
    deriving (Eq, Ord, Show)

{- FOURMOLU_DISABLE -}
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
{- FOURMOLU_ENABLE -}

-- |
--
-- >>> toInsRelativeIndex 99 100
-- InsRelativeIndex 0
-- >>> toInsRelativeIndex 98 100
-- InsRelativeIndex 1
-- >>> toInsRelativeIndex 97 100
-- InsRelativeIndex 2
-- >>> toInsRelativeIndex 96 100
-- InsRelativeIndex 3
toInsRelativeIndex :: AbsoluteIndex -> InsertionPoint -> InsRelativeIndex
toInsRelativeIndex (AbsoluteIndex idx) (InsertionPoint ip) =
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
fromInsRelativeIndex :: InsRelativeIndex -> InsertionPoint -> AbsoluteIndex
fromInsRelativeIndex (InsRelativeIndex ri) (InsertionPoint ip) =
    AbsoluteIndex (ip - ri - 1)

-- |
--
-- >>> toHBRelativeIndex 96 98
-- HBRelativeIndex 1
-- >>> toHBRelativeIndex 97 98
-- HBRelativeIndex 0
toHBRelativeIndex :: AbsoluteIndex -> BasePoint -> HBRelativeIndex
toHBRelativeIndex (AbsoluteIndex idx) (BasePoint bp) =
    HBRelativeIndex (bp - idx - 1)

-- |
--
-- >>> fromHBRelativeIndex 1 98
-- AbsoluteIndex 96
-- >>> fromHBRelativeIndex 0 98
-- AbsoluteIndex 97
fromHBRelativeIndex :: HBRelativeIndex -> BasePoint -> AbsoluteIndex
fromHBRelativeIndex (HBRelativeIndex ri) (BasePoint bp) =
    AbsoluteIndex (bp - ri - 1)

-- |
--
-- >>> toPostBaseIndex 98 98
-- PostBaseIndex 0
-- >>> toPostBaseIndex 99 98
-- PostBaseIndex 1
toPostBaseIndex :: AbsoluteIndex -> BasePoint -> PostBaseIndex
toPostBaseIndex (AbsoluteIndex idx) (BasePoint bp) =
    PostBaseIndex (idx - bp)

-- |
--
-- >>> fromPostBaseIndex 0 98
-- AbsoluteIndex 98
-- >>> fromPostBaseIndex 1 98
-- AbsoluteIndex 99
fromPostBaseIndex :: PostBaseIndex -> BasePoint -> AbsoluteIndex
fromPostBaseIndex (PostBaseIndex pix) (BasePoint bp) =
    AbsoluteIndex (pix + bp)

type Setter = Word8 -> Word8

set1
    , set01
    , set10
    , set11
    , set001
    , set0001
    , set0100
    , set0101
    , set0010
    , set00000
    , set00001
        :: Setter

{- FOURMOLU_DISABLE -}
set1     = (`setBit` 7)
set01    = (`setBit` 6)
set10    = (`setBit` 7)
set11    = (`setBit` 7) . (`setBit` 6)
set001   = (`setBit` 5)
set0001  = (`setBit` 4)
set0100  = (`setBit` 6)
set0101  = (`setBit` 6) . (`setBit` 4)
set0010  = (`setBit` 5)
set00001 = (`setBit` 3)

set0, set00, set000, set0000 :: Setter

set0     = id
set00    = id
set000   = id
set0000  = id
set00000 = id
{- FOURMOLU_ENABLE -}
