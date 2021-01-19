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
module Data.Array.Polarized.Push
  (
  -- * Construction
    Array(..)
  , make
  , singleton
  , cons
  , snoc
  -- * Operations
  , alloc
  , foldMap
  , unzip
  )
where

import qualified Data.Functor.Linear as Data
import qualified Data.Array.Destination as DArray
import Data.Array.Destination (DArray)
import Data.Vector (Vector)
import qualified Prelude
import Prelude.Linear hiding (unzip, foldMap)
import GHC.Stack


-- The Types
-------------------------------------------------------------------------------

-- | Push arrays are un-allocated finished arrays. These are finished
-- computations passed along or enlarged until we are ready to allocate.
data Array a where
  Array :: (forall m. Monoid m => (a -> m) -> m) %1-> Array a
  -- Developer notes:
  --
  -- Think of @(a -> m)@ as something that writes an @a@ and think of
  -- @((a -> m) -> m)@ as something that takes a way to write a single element
  -- and writes and concatenates all elements. The @m@ is something that
  -- represents a writing of some elements to an array, a delayed write.
  --
  -- Also, note that in this formulation we don't know the length beforehand.

data ArrayWriter a where
  ArrayWriter :: (DArray a %1-> ()) %1-> !Int -> ArrayWriter a
  -- The second parameter is the length of the @DArray@
  --
  -- Developer notes:
  --
  -- This is the linear monoid @m@ that we instantiate the above array with
  -- in order to allocate. An @ArrayWriter a@ is something that holds the
  -- ingredients to write some number of elements to an array, without
  -- holding the space to do so.


-- API
-------------------------------------------------------------------------------

-- | Convert a push array into a vector by allocating. This would be a common
-- end to a computation using push and pull arrays.
alloc :: Array a %1-> Vector a
alloc (Array k) = allocArrayWriter $ k singletonWriter where
  singletonWriter :: a -> ArrayWriter a
  singletonWriter a = ArrayWriter (DArray.fill a) 1

  allocArrayWriter :: ArrayWriter a %1-> Vector a
  allocArrayWriter (ArrayWriter writer len) = DArray.alloc len writer

-- | @`make` x n@ creates a constant push array of length @n@ in which every
-- element is @x@.
make :: HasCallStack => a -> Int -> Array a
make x n
  | n < 0 = error "Making a negative length push array"
  | otherwise = Array (\makeA -> mconcat $ Prelude.replicate n (makeA x))

singleton :: a -> Array a
singleton x = Array (\writeA -> writeA x)

snoc :: a -> Array a %1-> Array a
snoc x (Array k) = Array (\writeA -> (k writeA) <> (writeA x))

cons :: a -> Array a %1-> Array a
cons x (Array k) = Array (\writeA -> (writeA x) <> (k writeA))

foldMap :: Monoid b => (a -> b) -> Array a %1-> b
foldMap f (Array k) = k f

unzip :: Array (a,b) %1-> (Array a, Array b)
unzip (Array k) = k (\(a,b) -> (singleton a, singleton b))


-- # Instances
-------------------------------------------------------------------------------

instance Data.Functor Array where
  fmap f (Array k) = Array (\g -> k (\x -> (g (f x))))

instance Prelude.Semigroup (Array a) where
  (<>) x y = append x y

instance Semigroup (Array a) where
  (<>) = append

instance Prelude.Monoid (Array a) where
  mempty = empty

instance Monoid (Array a) where
  mempty = empty

empty :: Array a
empty = Array (\_ -> mempty)

append :: Array a %1-> Array a %1-> Array a
append (Array k1) (Array k2) = Array (\writeA -> k1 writeA <> k2 writeA)

instance Prelude.Semigroup (ArrayWriter a) where
  (<>) x y = addWriters x y

instance Prelude.Monoid (ArrayWriter a) where
  mempty = emptyWriter

instance Semigroup (ArrayWriter a) where
  (<>) = addWriters

instance Monoid (ArrayWriter a) where
  mempty = emptyWriter

addWriters :: ArrayWriter a %1-> ArrayWriter a %1-> ArrayWriter a
addWriters (ArrayWriter k1 l1) (ArrayWriter k2 l2) =
  ArrayWriter
    (\darr ->
      (DArray.split l1 darr) & \(darr1,darr2) -> consume (k1 darr1, k2 darr2))
    (l1+l2)

emptyWriter :: ArrayWriter a
emptyWriter = ArrayWriter DArray.dropEmpty 0
-- Remark. @emptyWriter@ assumes we can split a destination array at 0.

