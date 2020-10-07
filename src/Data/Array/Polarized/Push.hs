{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

-- | This module provides push arrays.
--
-- These are part of a larger framework for controlling when memory is
-- allocated for an array. See @Data.Array.Polarized@.
--
-- This module is designed to be imported qualified as @Push@.
module Data.Array.Polarized.Push where

-- XXX: it might be better to hide the data constructor, in case we wish to
-- change the implementation.

import Data.Array.Destination (DArray)
import qualified Data.Array.Destination as DArray
import qualified Data.Functor.Linear as Data
import Data.Vector (Vector)
import Prelude.Linear
import qualified Prelude

-- TODO: the below isn't really true yet since no friendly way of constructing
-- a PushArray directly is given yet (see issue #62), but the spirit holds.
-- TODO: There's also a slight lie in that one might want to consume a
-- PushArray into a commutative monoid, for instance summing all the elements,
-- and this is not yet possible, although it should be.

-- | Push arrays are un-allocated finished arrays. These are finished
-- computations passed along or enlarged until we are ready to allocate.
data Array a where
  -- The second parameter is the length of the @DArray@
  Array :: (forall b. (a %1-> b) -> DArray b %1-> ()) %1-> Int -> Array a
  deriving Prelude.Semigroup via NonLinear (Array a)

instance Data.Functor Array where
  fmap f (Array k n) = Array (\g dest -> k (g . f) dest) n

instance Semigroup (Array a) where
  (<>) = append

-- XXX: the use of Vector in the type of alloc is temporary (see also
-- "Data.Array.Destination")
-- | Convert a push array into a vector by allocating. This would be a common
-- end to a computation using push and pull arrays.
alloc :: Array a %1-> Vector a
alloc (Array k n) = DArray.alloc n (k id)

-- | @`make` x n@ creates a constant push array of length @n@ in which every
-- element is @x@.
make :: a -> Int -> Array a
make x n = Array (\k -> DArray.replicate (k x)) n

-- | Concatenate two push arrays.
append :: Array a %1-> Array a %1-> Array a
append (Array kl nl) (Array kr nr) =
    Array
      (\f dest -> parallelApply f kl kr (DArray.split nl dest))
      (nl+nr)
  where
    parallelApply :: (a %1-> b) -> ((a %1-> b) -> DArray b %1-> ()) %1-> ((a %1-> b) -> DArray b %1-> ()) %1-> (DArray b, DArray b) %1-> ()
    parallelApply f' kl' kr' (dl, dr) = kl' f' dl <> kr' f' dr
