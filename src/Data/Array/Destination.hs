{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides destination arrays
--
-- We can use linear types to release a safe API for destination arrays
-- and DPS; with this it's much easier to write memory efficient programs
-- that can often be as fast as idomatic C code (with the simplicity of
-- Haskell and FP, of course).
--
-- == What are destination arrays and DPS?
--
-- Destination arrays are arrays that are meant to written to inside a
-- function. These arrays are used for DPS (destination passing style
-- programming), a style of programming that uses memory
-- efficiently.
--
-- In this style of programming, certain functions are given
-- an array to write into instead of returning a result.
--
-- This style is commonplace in C programming. One might use it to write this
-- function, in which @sum@ is a destination array.
--
-- @
-- void add(int size, int *a, int *b, int *sum){
--   for (int i=0; i<size;++i){sum[i]=a[i]+b[i];}
-- }
-- @
--
-- C programs written in DPS use memory efficiently; that is, they allocate
-- less space from the system heap.
-- For instance: we could use @add@ to add four arrays with only one
-- temporary array, basically like so:
--
-- @
-- add(arr1, arr2, temp1);
-- add(arr3, temp1, temp1);
-- add(arr4, temp1, temp1);
-- @
--
-- If we had the type @int *add(int size, int *a, int *b)@, then
-- we would need three allocations. That is, we would be calling @malloc@
-- for arrays of size @size@ three times. (In general, for adding \(n\) arrays,
-- we would do this \(n-1\) times.
--
-- For a deeper background, see
-- [this paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/dps-fhpc17.pdf).
--
-- == Why do we need linear types?
--
-- Because of linear types, any function that uses a destination array
-- must take that 'DArray' as a linear argument. This is because the only
-- way to create a 'DArray' is through 'alloc' which takes a linear
-- continutation of type @DArray a #-> ()@.
--
-- Since 'DArray' doesn't have a 'Consumable' instance, the only way to
-- consume it is with the given API (e.g., with 'fill' or perhaps
-- 'fromFunction').
--
-- __Hence, linearity enforces that any function using a 'DArray',__
-- __consumes it and does so by writing to each cell of the array.__
--
-- == Example
--
-- > import qualified Data.Array.Destination as DPS
-- > import Prelude hiding ( Num(..), ($) )
-- > import Data.Vector (Vector, (!), fromList)
-- > import qualified Prelude as P
-- >
-- > -- | An application of a DPS computation
-- > normSumIO :: IO Double
-- > normSumIO = do
-- >   x <- inputVectorX
-- >   y <- inputVectorY
-- >   z <- inputVectorZ
-- >   return (normSum x y z)
-- >
-- > -- | Query from environment
-- > inputVectorX :: IO (Vector Int)
-- > inputVectorX = return (fromList [1..100])
-- >
-- > -- | Query from environment
-- > inputVectorY :: IO (Vector Int)
-- > inputVectorY =
-- >   return (fromList (map (\x -> (7 * (x+3)) `div` 11) [1..100]))
-- >
-- > -- | Query from environment
-- > inputVectorZ :: IO (Vector Int)
-- > inputVectorZ = return (fromList [negate i | i <- [1..100]])
-- >
-- > -- | Take the norm of the sum of three vectors
-- > normSum :: Vector Int -> Vector Int -> Vector Int -> Double
-- > normSum x y z = sqrt P.$ fromIntegral P.$ sum P.$
-- >   DPS.alloc 100 $ vecSum z P.$ DPS.alloc 100 (vecSum x y)
-- >
-- > -- | Add two vectors of the same size (crashes if
-- > -- the sizes don't match)
-- > vecSum :: Vector Int -> Vector Int -> DPS.DArray Int #-> ()
-- > vecSum xs ys = DPS.fromFunction (\i -> (xs ! i) + (ys ! i))
--
module Data.Array.Destination
  ( DArray
    -- * Running a DPS-style function that takes a @DArray@
  , alloc
    -- * Writing to and using @DArray@s in functions
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

-- | A destination array, @DArray@ is an array that's meant to be written to
-- in a DPS-computation.
newtype DArray a = DArray (MVector RealWorld a)

-- XXX: use of Vector in types is temporary. I will probably move away from
-- vectors and implement most stuff in terms of Array# and MutableArray#
-- eventually, anyway. This would allow to move the MutableArray logic to linear
-- IO, possibly, and segregate the unsafe casts to the Linear IO module.
-- @`alloc` n k@ must be called with a non-negative value of @n@.
alloc :: Int -> (DArray a #-> ()) #-> Vector a
alloc n = Unsafe.toLinear unsafeAlloc
  where
    unsafeAlloc :: (DArray a #-> ()) -> Vector a
    unsafeAlloc build = unsafeDupablePerformIO Prelude.$ do
      dest <- MVector.unsafeNew n
      evaluate (build (DArray dest))
      Vector.unsafeFreeze dest

replicate :: a -> DArray a #-> ()
replicate a = fromFunction (const a)

-- | @fill a dest@ fills a singleton destination array.
-- Caution, @'fill' a dest@ will fail is @dest@ isn't of length exactly one.
fill :: a #-> DArray a #-> ()
fill = Unsafe.toLinear2 unsafeFill
    -- XXX: we will probably be able to spare this unsafe cast given a (linear)
    -- length function on destination.
  where
    unsafeFill a (DArray ds) =
      if MVector.length ds /= 1 then
        error "Destination.fill: requires a destination of size 1"
      else
        unsafeDupablePerformIO Prelude.$ MVector.write ds 0 a

-- | @'split' n dest = (destl, destr)@ such as @destl@ has length @n@.
--
-- 'split' is total: if @n@ is larger than the length of @dest@, 
-- then @destr@ is empty.
split :: Int -> DArray a #-> (DArray a, DArray a)
split n = Unsafe.toLinear unsafeSplit
  where
    unsafeSplit (DArray ds) =
      let (dsl, dsr) = MVector.splitAt n ds in
        (DArray dsl, DArray dsr)

-- | Convert a Vector into a destination array to be filled, possibly with
-- a conversion function. Both arrays must have the same size.
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
