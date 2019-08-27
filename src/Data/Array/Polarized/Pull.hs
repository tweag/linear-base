{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module is intended to be imported qualified, e.g.
-- > import qualified Data.Array.Polarized.Pull as Pull
module Data.Array.Polarized.Pull
  ( Array
  , zip, zipWith
  , append
  , make
  , foldr
  , foldMap
  , findLength
  , asList
  , singleton
  , fromFunction
  , fromVector
  , toVector
  , split
  , reverse
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
import Prelude.Linear hiding (zip, zipWith, foldr, foldMap, reverse)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Unsafe.Linear as Unsafe

-- | Convert a pull array into a list.
asList :: Array a ->. [a]
asList = foldr (\x xs -> x:xs) []

-- | /!\ Partial! Only works if both arrays have the same length.
zipWith :: (a ->. b ->. c) -> Array a ->. Array b ->. Array c
zipWith f x y = Data.fmap (uncurry f) (zip x y)

-- | Fold a pull array into a monoid.
foldMap :: Monoid m => (a ->. m) -> Array a ->. m
foldMap f = foldr ((<>) . f) mempty

-- I'm fairly sure this can be used safely
-- | Convert a Vector to a pull array.
fromVector :: Vector a ->. Array a
fromVector = Unsafe.toLinear $ \v -> fromFunction (v Vector.!) (Vector.length v)
