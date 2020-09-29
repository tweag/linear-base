{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides destination arrays
--
-- == What are destination arrays? What are they good for?
--
-- Destination arrays are write-only arrays that are only allocated once,
-- thereby avoiding your reliance on GHC's fusion mechanisms to remove
-- unneccessary allocations.
--
-- The current status-quo for computations that have a write-only array
-- threaded along is to rely on fusion. While the optimizations in say,
-- `Data.Vector` are quite good at ensuring GHC fuses, they aren't
-- foolproof and can sometimes break by simple refactorings.
--
-- Avoiding extra allocations of a write-only array is easy in C, with
-- something the functional programming world calls destination passing style,
-- or DPS for short.
--
-- Here is a C function that manipulates an array written in DPS style; it
-- takes in the destiniation array @res@ and writes to it:
--
-- @
-- // ((a + b) * c) for vectors a,b and scalar c
-- void apbxc(int size, int *a, int *b, int c, int *res){
--   for (int i=0; i<size;++i){res[i]=a[i]+b[i];}
--   mult(size, c, res);
-- }
--
-- void mult(int size, int scalar, int* vec){
--   for (int i=0; i<size; ++i){vec[i] *= scalar;}
-- }
-- @
--
-- We could write this with @Data.Vector@ in haskell and rely on GHC to fuse
-- (but this isn't great in complex uses where some simple changes can
-- break the fusion):
--
-- @
-- import Data.Vector (Vector)
-- import qualified Data.Vector as Vector
-- import qualified Prelude
--
-- apbxc :: Vector Int -> Vector Int -> Int -> Vector Int
-- apbxc a b c = mult c Prelude.$ Vector.zipWith (Prelude.+) a b
--
-- mult :: Int -> Vector Int -> Vector Int
-- mult c = Vector.map (Prelude.* c)
-- @
--
-- Doing this with destination arrays we are guarenteed only one allocation
-- of the resulting array:
--
-- @
--  apbxcDest :: Vector Int -> Vector Int -> Int -> Vector Int
--  apbxcDest a b c = alloc (Vector.length a) writeResult
--    where
--      writeResult :: DArray Int #-> ()
--      writeResult = fromFunction (\i -> ((a ! i) + (b ! i)) * c)
-- @
--
-- == Aside: Why do we need linear types?
--
-- Linear types avoids ambigious writes to the destination array.
-- For example, this function could never be linear and hence we avoid
-- ambiguity:
--
-- @
--  nonLinearUse :: DArray Int -> ()
--  nonLinearUse arr = case (replicate 3 arr, replicate 4 arr) of
--    ((),()) -> ()
-- @
--
-- Further, linear types are used to ensure that each cell in the destination
-- array is written to exactly once. This is because the only way to create and
-- use a destination array is via
--
-- @
-- alloc :: Int -> (DArray a #-> ()) #-> Vector a
-- @
--
-- and the only way to really consume a @DArray@ is via our API
-- which requires you to completely fill the array.
--
module Data.Array.Destination
  (
  -- * The Data Type
    DArray
  -- * Create and use a @DArray@
  , alloc
  -- * Ways to write to a @DArray@
  , replicate
  , split
  , mirror
  , fromFunction
  , fill
  )
  where

import Control.Exception (evaluate)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector
import GHC.Exts (RealWorld)
import qualified Prelude as Prelude
import Prelude.Linear hiding (replicate)
import System.IO.Unsafe
import qualified Unsafe.Linear as Unsafe


-- | A destination array, or @DArray@, is a write-only array that is filled
-- by some computation which ultimately returns an array.
newtype DArray a = DArray (MVector RealWorld a)

-- XXX: use of Vector in types is temporary. I will probably move away from
-- vectors and implement most stuff in terms of Array# and MutableArray#
-- eventually, anyway. This would allow to move the MutableArray logic to
-- linear IO, possibly, and segregate the unsafe casts to the Linear IO
-- module.  @`alloc` n k@ must be called with a non-negative value of @n@.
alloc :: Int -> (DArray a #-> ()) #-> Vector a
alloc n = Unsafe.toLinear unsafeAlloc
  where
    unsafeAlloc :: (DArray a #-> ()) -> Vector a
    unsafeAlloc build = unsafeDupablePerformIO Prelude.$ do
      dest <- MVector.unsafeNew n
      evaluate (build (DArray dest))
      Vector.unsafeFreeze dest

-- | Fill a destination array with a constant
replicate :: a -> DArray a #-> ()
replicate a = fromFunction (const a)

-- | @fill a dest@ fills a singleton destination array.
-- Caution, @'fill' a dest@ will fail is @dest@ isn't of length exactly one.
fill :: a #-> DArray a #-> ()
fill = Unsafe.toLinear2 unsafeFill
    -- XXX: we will probably be able to spare this unsafe cast given a
    -- (linear) length function on destination.
  where
    unsafeFill a (DArray ds) =
      if MVector.length ds /= 1 then
        error "Destination.fill: requires a destination of size 1"
      else
        unsafeDupablePerformIO Prelude.$ MVector.write ds 0 a

-- | @'split' n dest = (destl, destr)@ such as @destl@ has length @n@.
--
-- 'split' is total: if @n@ is larger than the length of @dest@, then
-- @destr@ is empty.
split :: Int -> DArray a #-> (DArray a, DArray a)
split n = Unsafe.toLinear unsafeSplit
  where
    unsafeSplit (DArray ds) =
      let (dsl, dsr) = MVector.splitAt n ds in
        (DArray dsl, DArray dsr)

-- | Convert a Vector into a destination array to be filled, possibly with a
-- conversion function.  Assumes both arrays have the same size.
mirror :: Vector a -> (a #-> b) -> DArray b #-> ()
mirror v f = fromFunction (\t -> f (v ! t))

-- | Fill a destination array using the given index-to-value function.
fromFunction :: (Int -> b) -> DArray b #-> ()
fromFunction f = Unsafe.toLinear unsafeFromFunction
  where unsafeFromFunction (DArray ds) = unsafeDupablePerformIO Prelude.$ do
          let n = MVector.length ds
          Prelude.sequence_ [MVector.unsafeWrite ds m (f m) | m <- [0..n-1]]
-- The unsafe cast here is actually safe, since getting the length does not
-- touch any elements, and each write fills in exactly one slot, so
-- each slot of the destination array is filled.
