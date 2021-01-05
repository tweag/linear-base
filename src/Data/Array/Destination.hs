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
-- == Example: Stencil computation
--
-- One possible use of destination arrays could be the stencil computation
-- typically called
-- [jacobi](https://en.wikipedia.org/wiki/Iterative_Stencil_Loops#Example:_2D_Jacobi_iteration).
-- Here we show one time step of this computation in a single dimension:
--
-- @
-- jacobi1d :: Int -> Vector Double -> Vector Double
-- jacobi1d n oldA = case stepArr n oldA of 
--   newB -> stepArr n newB
--
-- -- @jacobi1d N A[N] B[N] = (new_A[N], new_B[N])@.
-- stepArr :: Int -> Vector Double -> Vector Double
-- stepArr n oldArr = alloc n $ \newArr -> fillArr newArr oldArr 1
--   where
--     fillArr :: DArray Double %1-> Vector Double -> Int -> ()
--     fillArr newA oldA ix
--       | ix == (n-1) = newA &
--           fill (0.33 * ((oldA ! (ix-1)) + (oldA ! ix) + (oldA ! (ix+1))))
--       | True = split 1 newA & \(fst, rest) ->
--           fill (0.33 * ((oldA ! (ix-1)) + (oldA ! ix) + (oldA ! (ix+1)))) fst &
--             \() -> fillArr rest oldA (ix+1)
-- @
--
-- We can be sure that @stepArr@ only allocates one array. In certain
-- variations and implementations of the jacobi kernel or similar dense array
-- computations, ensuring one allocation with @Data.Vector@'s fusion oriented
-- implementation may not be trivial.
--
-- For reference, the C equivalent of this code is the following:
--
-- @
-- static void jacobi_1d_time_step(int n, int *A, int *B){
--   int t, i;
--   for (i = 1; i < _PB_N - 1; i++)
--     B[i] = 0.33333 * (A[i-1] + A[i] + A[i + 1]);
--   for (i = 1; i < _PB_N - 1; i++)
--     A[i] = 0.33333 * (B[i-1] + B[i] + B[i + 1]);
-- }
-- @
--
-- This example is taken from the
-- [polybench test-suite](https://web.cse.ohio-state.edu/~pouchet.2/software/polybench/)
-- of dense array codes.
--
-- == Aside: Why do we need linear types?
--
-- Linear types avoids ambiguous writes to the destination array.
-- For example, this function could never be linear and hence we avoid
-- ambiguity:
--
-- @
--  nonLinearUse :: DArray Int -> ()
--  nonLinearUse arr = case (replicate 3 arr, replicate 4 arr) of
--    ((),()) -> ()
-- @
--
-- Furthermore, this API is safely implemented by mutating an underlying array
-- which is good for performance. The API is safe because linear types
-- enforce the fact that each reference to an underlying mutable array
-- (and there can be more than one by using @split@) is
-- linearly threaded through functions and at the end consumed by one of our
-- write functions.
--
-- Lastly, linear types are used to ensure that each cell in the destination
-- array is written to exactly once. This is because the only way to create and
-- use a destination array is via
--
-- @
-- alloc :: Int -> (DArray a %1-> ()) %1-> Vector a
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
  , size
  -- * Ways to write to a @DArray@
  , replicate
  , split
  , mirror
  , fromFunction
  , fill
  , dropEmpty
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
import GHC.Stack
import qualified Unsafe.Linear as Unsafe

-- | A destination array, or @DArray@, is a write-only array that is filled
-- by some computation which ultimately returns an array.
newtype DArray a = DArray (MVector RealWorld a)

-- XXX: use of Vector in types is temporary. I will probably move away from
-- vectors and implement most stuff in terms of Array# and MutableArray#
-- eventually, anyway. This would allow to move the MutableArray logic to
-- linear IO, possibly, and segregate the unsafe casts to the Linear IO
-- module.  @`alloc` n k@ must be called with a non-negative value of @n@.
alloc :: Int -> (DArray a %1-> ()) %1-> Vector a
alloc n = Unsafe.toLinear unsafeAlloc
  where
    unsafeAlloc :: (DArray a %1-> ()) -> Vector a
    unsafeAlloc build = unsafeDupablePerformIO Prelude.$ do
      dest <- MVector.unsafeNew n
      evaluate (build (DArray dest))
      Vector.unsafeFreeze dest

-- | Get the size of a destination array.
size :: DArray a %1-> (Ur Int, DArray a)
size (DArray vec) = Unsafe.toLinear go vec
 where
  go vec' = (Ur (MVector.length vec'), DArray vec')

-- | Fill a destination array with a constant
replicate :: a -> DArray a %1-> ()
replicate a = fromFunction (const a)

-- | @fill a dest@ fills a singleton destination array.
-- Caution, @'fill' a dest@ will fail is @dest@ isn't of length exactly one.
fill :: HasCallStack => a %1-> DArray a %1-> ()
fill = Unsafe.toLinear2 unsafeFill
    -- XXX: we will probably be able to spare this unsafe cast given a
    -- (linear) length function on destination.
  where
    unsafeFill a (DArray ds) =
      if MVector.length ds /= 1 then
        error "Destination.fill: requires a destination of size 1"
      else
        unsafeDupablePerformIO Prelude.$ MVector.write ds 0 a

-- | @dropEmpty dest@ consumes and empty array and fails otherwise.
dropEmpty :: HasCallStack => DArray a %1-> ()
dropEmpty = Unsafe.toLinear unsafeDrop where
  unsafeDrop :: DArray a -> ()
  unsafeDrop (DArray ds)
    | MVector.length ds > 0 = error "Destination.dropEmpty on non-empty array."
    | otherwise = ()

-- | @'split' n dest = (destl, destr)@ such as @destl@ has length @n@.
--
-- 'split' is total: if @n@ is larger than the length of @dest@, then
-- @destr@ is empty.
split :: Int -> DArray a %1-> (DArray a, DArray a)
split n = Unsafe.toLinear unsafeSplit
  where
    unsafeSplit (DArray ds) =
      let (dsl, dsr) = MVector.splitAt n ds in
        (DArray dsl, DArray dsr)

-- | Fills the destination array with the contents of given vector.
--
-- Errors if the given vector is smaller than the destination array.
mirror :: HasCallStack => Vector a -> (a %1-> b) -> DArray b %1-> ()
mirror v f arr =
  size arr & \(Ur sz, arr') ->
    if Vector.length v < sz
    then error "Destination.mirror: argument smaller than DArray" $ arr'
    else fromFunction (\t -> f (v ! t)) arr'

-- | Fill a destination array using the given index-to-value function.
fromFunction :: (Int -> b) -> DArray b %1-> ()
fromFunction f = Unsafe.toLinear unsafeFromFunction
  where unsafeFromFunction (DArray ds) = unsafeDupablePerformIO Prelude.$ do
          let n = MVector.length ds
          Prelude.sequence_ [MVector.unsafeWrite ds m (f m) | m <- [0..n-1]]
-- The unsafe cast here is actually safe, since getting the length does not
-- touch any elements, and each write fills in exactly one slot, so
-- each slot of the destination array is filled.
