{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides pull arrays.
--
-- These are part of a larger framework for controlling when memory is
-- allocated for an array. See @Data.Array.Polarized@.
module Data.Array.Polarized.Pull
  ( Array,

    -- * Construction
    fromFunction,
    fromVector,
    make,
    singleton,

    -- * Consumption
    toVector,
    asList,

    -- * Operations
    zip,
    zipWith,
    append,
    foldr,
    foldMap,
    findLength,
    split,
    reverse,
    index,
  )
where

import Data.Array.Polarized.Pull.Internal
-- XXX: the data constructor Pull.Array could be used unsafely, so we don't
-- export it, instead exporting a collection of functions to manipulate
-- PullArrays
-- (eg one could use an element multiple times, if the constructor was
-- available)
-- TODO: the current collection is almost certainly not complete: it would be
-- nice if there was one (or a small number) of functions which characterise
-- PullArrays, but I'm not sure what they are
-- In particular, PullArrays are incredibly unfriendly in returned-value
-- position at the moment, moreso than they should be
import qualified Data.Functor.Linear as Data
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude.Linear hiding (foldMap, foldr, reverse, zip, zipWith)
import qualified Unsafe.Linear as Unsafe

-- | Convert a pull array into a list.
asList :: Array a %1 -> [a]
asList = foldr (\x xs -> x : xs) []

-- | @zipWith f [x1,x2,...,xn] [y1,y2,...,yn] = [f x1 y1, ..., f xn yn]@
-- __Partial:__ `zipWith f [x1,x2,...,xn] [y1,y2,...,yp]` is an error
-- if @n ≠ p@.
zipWith :: (a %1 -> b %1 -> c) -> Array a %1 -> Array b %1 -> Array c
zipWith f x y = Data.fmap (uncurry f) (zip x y)

-- | Fold a pull array using a monoid.
foldMap :: Monoid m => (a %1 -> m) -> Array a %1 -> m
foldMap f = foldr ((<>) . f) mempty

-- I'm fairly sure this can be used safely

-- | Convert a Vector to a pull array.
fromVector :: Vector a %1 -> Array a
fromVector = Unsafe.toLinear $ \v -> fromFunction (v Vector.!) (Vector.length v)
