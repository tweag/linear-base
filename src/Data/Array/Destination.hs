{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Array.Destination
  ( DArray
  , alloc
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

-- | An array destination ('DArray') is a form a constructor for arrays, which
-- make it possible to create by adding a new element to it in
-- O(1).
--
-- Destination-passing style is common in C programming, where we pass a mutable
-- data-structure to a function, whose responsibility it is to fill the array.
--
-- Through linear types, however, we can expose array destinations as /pure/
-- data structures (even though, operationally, filling the destination is
-- implemented by mutations under the hood). Moreover, we make sure that each
-- cell in the destination will be filled exactly once (in particular no cell
-- will be forgotten), and we can't read cells in the destination, therefore
-- there is no risk of reading an uninitialised cell.
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
      if MVector.length ds Prelude./= 1 then
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
