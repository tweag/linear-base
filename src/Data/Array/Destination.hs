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
-- This style is common place in C programming. One might use it to write this
-- function, in which @sum@ is a destiniation array.
--
-- @
-- void add(int size, int *a, int *b, int *sum){
--   for (int i=0; i<size;++i){sum[i]=a[i]+b[i];}
-- }
-- @
--
-- C programs using DPS are much more efficient.
-- [TODO why and how!? ... explain!!]
--
-- For a deeper background, see
-- [this paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/dps-fhpc17.pdf)
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
-- Hence, /linearity enforces that any function using a 'DArray' consumes it
-- and does so by writing to each cell of the array/.
--
-- Another property of the API is that we guarentee efficient stack allocation
-- of destination arrays -- i.e., unlike how C programmers must be careful to
-- maintain the DPS idioms, here the type system enforces them.
--
-- == How do I reason about and write memory performant DPS code?
--
-- [TODO because I have no earthly idea ... and this seems critical]
--
-- == Example
--
--
-- > import qualified Data.Array.Destination as DPS
-- >
-- > -- | An application of a DPS computation
-- > -- This uses a DPS-style function, and hence uses @alloc@
-- > someAxpy :: IO (Vector Int)
-- > someAxpy = do
-- >   a <- inputScalarA
-- >   x <- inputVectorX
-- >   y <- inputVectorY
-- >   return (DPS.alloc 100 (axpy x a y))
-- >
-- > -- | Query from environment
-- > inputVectorX :: IO [Int]
-- > inputVectorX = return [1..100]
-- >
-- > -- | Query from environment
-- > inputVectorY :: IO [Int]
-- > inputVectorY = return (map (\x -> (7 * (x+3)) `div` 11) [1..100])
-- >
-- > -- | Query from environment
-- > inputScalarA :: IO Int
-- > inputScalarA = return 5
-- >
-- > -- | @axpy x a b y@
-- > -- computes y = a . x + b for vectors y, x and scalars a,b.
-- > -- precondition: the two lists are of the same length
-- > --
-- > -- Because of linear types, we are sure the array is written to
-- > axpy :: [Int] -> Int -> [Int] -> DPS.DArray Int #-> ()
-- > axpy x a y = DPS.fromFunction destArrayWriter
-- >   where
-- >     destArrayWriter :: Int -> Int
-- >     destArrayWriter ix = a * (x !! ix) + (y !! ix)
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
--
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

-- | Caution, @'fill' a dest@ will fail is @dest@ isn't of length exactly one.
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
-- 'split' is total: if @n@ is larger than the length of @dest@, then @destr@ is
-- empty.
split :: Int -> DArray a #-> (DArray a, DArray a)
split n = Unsafe.toLinear unsafeSplit
  where
    unsafeSplit (DArray ds) =
      let (dsl, dsr) = MVector.splitAt n ds in
        (DArray dsl, DArray dsr)

-- | Convert a Vector into a destination array to be filled, possibly with
-- a conversion function.
-- Assumes both arrays have the same size.
mirror :: Vector a -> (a #-> b) -> DArray b #-> ()
mirror v f = fromFunction (\t -> f (v ! t))

-- | Fill a destination array using the given function.
fromFunction :: (Int -> b) -> DArray b #-> ()
fromFunction f = Unsafe.toLinear unsafeFromFunction
  where unsafeFromFunction (DArray ds) = unsafeDupablePerformIO Prelude.$ do
          let n = MVector.length ds
          Prelude.sequence_ [MVector.unsafeWrite ds m (f m) | m <- [0..n-1]]
-- The unsafe cast here is actually safe, since getting the length does not
-- touch any elements, and each write fills in exactly one slot, so
-- each slot of the destination array is filled.
