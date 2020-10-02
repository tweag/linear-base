{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

-- | This module is intended to be imported qualified, e.g.
-- > import qualified Data.Array.Polarized.Push as Push
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

-- | PushArrays are those arrays which are friendly in returned value position.
-- They are easy to create and can be constructed by supplying elements in any
-- order. Dually, they are harder to consume as they will supply their elements
-- in an unknown order, and allocation is generally required for consumption.
data Array a where
  Array :: (forall b. (a %1-> b) -> DArray b %1-> ()) %1-> Int -> Array a
  deriving Prelude.Semigroup via NonLinear (Array a)
  -- A note on implementation: `exists b. ((a -> b), DArray b)` adjoins freely
  -- the structure of contravariant functor to `DArray`. Because it appears to
  -- the left of an arrow, we can curry the existential quantification (and,
  -- less crucially, the pair) so that we can spare an extra type definition.
  --
  -- An invariant is kept that, really, the first parameter is only passed
  -- arrays of size @n@ (the second parameter, which is the length of the
  -- array).
  --
  -- TODO: Consider changing DArray b %1-> () to something more general

instance Data.Functor Array where
  fmap f (Array k n) = Array (\g dest -> k (g . f) dest) n

instance Semigroup (Array a) where
  (<>) = append

-- XXX: the use of Vector in the type of alloc is temporary (see also "Data.Array.Destination")
-- | Convert a PushArray into a Vector by allocating. This would be a common
-- end to a fusion pipeline.
alloc :: Array a %1-> Vector a
alloc (Array k n) = DArray.alloc n (k id)

-- | @`make` x n@ creates a PushArray of length @n@ in which every element is
-- @x@.
make :: a -> Int -> Array a
make x n = Array (\k -> DArray.replicate (k x)) n

-- | Concatenate two PushArrays.
append :: Array a %1-> Array a %1-> Array a
append (Array kl nl) (Array kr nr) =
    Array
      (\f dest -> parallelApply f kl kr (DArray.split nl dest))
      (nl+nr)
  where
    parallelApply :: (a %1-> b) -> ((a %1-> b) -> DArray b %1-> ()) %1-> ((a %1-> b) -> DArray b %1-> ()) %1-> (DArray b, DArray b) %1-> ()
    parallelApply f' kl' kr' (dl, dr) = kl' f' dl <> kr' f' dr
