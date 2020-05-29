{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides destination arrays
--
-- We can use linear types to release a safe API for destination arrays
-- and DPS (destination passing style).
--
-- == What are destination arrays and DPS?
--
-- There are several lists which must, despite program optimizations
-- (i.e., [deforesting](https://www.sciencedirect.com/science/article/pii/030439759090147A)),
-- be allocated, filled, passed along and de-allocated. When the allocation
-- of these arrays is controlled by the programmer and not
-- done by Haskell's GC (garbage collector), programs are often more efficient.
--
-- DPS is a style of programming in which functions that produce such arrays
-- take them as arguments -- moving the responsibility of allocating, using
-- and freeing those arrays to the caller. The arrays passed as arguments
-- to functions that must be written to are called /destination arrays/.
--
-- Here is a C function that adds vectors written in DPS style; it
-- takes in the destiniation array @sum@ and writes to it:
--
-- @
-- void add(int size, int *a, int *b, int *sum){
--   for (int i=0; i<size;++i){sum[i]=a[i]+b[i];}
-- }
-- @
--
-- == Linear Types Enforce Destination Arrays are Filled
--
-- Linear types are used to ensure that the destination array
-- is always written to. Why? Well:
--
-- Because of linear types, any function that uses a destination array
-- must take that 'DArray' as a linear argument. This is because the only
-- way to create a 'DArray' is through 'alloc' which takes a linear
-- continutation of type @DArray a #-> ()@ and any function inside that
-- continuation that uses a @DArray@ must use it linearly.
--
-- Since 'DArray' doesn't have a 'Consumable' instance, the only way to
-- consume it is with the given API (e.g., with 'fill' or perhaps
-- 'fromFunction') which fills the destiniation array completely
-- (or errors as we desire).
--
-- For a deeper background on destination arrays see
-- [this paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/dps-fhpc17.pdf).
--
-- == Example
--
-- > import qualified Prelude.Linear as Linear
-- > import qualified Data.Array.Destination as DPS
-- > import qualified Data.Vector (Vector)
-- > import qualified Data.Vector as Vector
-- >
-- > -- | Computes the diff of a vector and writes to an array
-- > -- diff [1,2,3,4] is [2-1,3-2,4-3]
-- > computeDiff :: Vector Int -> DPS.DArray Int #-> ()
-- > computeDiff vec = DPS.fromFunction
-- >   (\ix -> (vec Vector.! (ix + 1)) - (vec Vector.! ix))
-- >
-- > someAction :: IO ()
-- > someAction = do
-- >   vec <- inputVector
-- >   let diffSize = (Vector.length vec) - 1
-- >   let vecToSend = DPS.alloc diffSize (computeDiff vec)
-- >   sendVectorToTwoServers vecToSend
-- >   where
-- >
-- >     sendVectorToTwoServers :: Vector Int -> IO ()
-- >     sendVectorToTwoServers _ = {- fake implementation -}
-- >       return ()
-- >
-- >     inputVector :: IO (Vector Int)
-- >     inputVector =
-- >       return (fromList (map (\x -> (7 * (x+3)) `div` 11) [1..100]))
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
