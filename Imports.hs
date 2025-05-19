{-# LANGUAGE CPP #-}

module Imports (
    ByteString (..),
    module Control.Applicative,
    module Control.Monad,
    module Data.Bits,
    module Data.List,
    module Data.Foldable,
    module Data.Int,
    module Data.Monoid,
    module Data.Ord,
    module Data.Word,
    module Data.Maybe,
    module Numeric,
    module Network.HTTP.Semantics,
    module Network.HTTP.Types,
    module Data.CaseInsensitive,
    withForeignPtr,
    mallocPlainForeignPtrBytes,
#if !MIN_VERSION_base(4,17,0)
    (!<<.), (!>>.),
#endif
) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString.Internal (ByteString (..))
import Data.CaseInsensitive (foldedCase, mk, original)
import Data.Foldable
import Data.Int
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Word
import Foreign.ForeignPtr
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import Network.HTTP.Semantics
import Network.HTTP.Types
import Numeric

#if !MIN_VERSION_base(4,17,0)
infixl 8 !<<.
(!<<.) :: Bits a => a -> Int -> a
(!<<.) = unsafeShiftL

infixl 8 !>>.
(!>>.) :: Bits a => a -> Int -> a
(!>>.) = unsafeShiftR
#endif
