{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Array.Destination.Internal where

import Data.Unrestricted.Linear
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector
import GHC.Exts (RealWorld)
import GHC.Stack
import Prelude.Linear hiding (replicate)
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as Prelude

-- | A destination array, or @DArray@, is a write-only array that is filled
-- by some computation which ultimately returns an array.
data DArray a where
  DArray :: MVector RealWorld a -> DArray a

-- XXX: use of Vector in types is temporary. I will probably move away from
-- vectors and implement most stuff in terms of Array# and MutableArray#
-- eventually, anyway. This would allow to move the MutableArray logic to
-- linear IO, possibly, and segregate the unsafe casts to the Linear IO
-- module.  @`alloc` n k@ must be called with a non-negative value of @n@.
alloc :: Int -> (DArray a %1 -> ()) %1 -> Vector a
alloc n writer = (\(Ur dest, vec) -> writer (DArray dest) `lseq` vec) $
  unsafeDupablePerformIO $ do
    destArray <- MVector.unsafeNew n
    vec <- Vector.unsafeFreeze destArray
    Prelude.return (Ur destArray, vec)

-- | Get the size of a destination array.
size :: DArray a %1 -> (Ur Int, DArray a)
size (DArray mvec) = (Ur (MVector.length mvec), DArray mvec)

-- | Fill a destination array with a constant
replicate :: a -> DArray a %1 -> ()
replicate a = fromFunction (const a)

-- | @fill a dest@ fills a singleton destination array.
-- Caution, @'fill' a dest@ will fail is @dest@ isn't of length exactly one.
fill :: (HasCallStack) => a %1 -> DArray a %1 -> ()
fill a (DArray mvec) =
  if MVector.length mvec /= 1
    then error "Destination.fill: requires a destination of size 1" $ a
    else
      a
        & Unsafe.toLinear (\x -> unsafeDupablePerformIO (MVector.write mvec 0 x))

-- | @dropEmpty dest@ consumes and empty array and fails otherwise.
dropEmpty :: (HasCallStack) => DArray a %1 -> ()
dropEmpty (DArray mvec)
  | MVector.length mvec > 0 = error "Destination.dropEmpty on non-empty array."
  | otherwise = mvec `seq` ()

-- | @'split' n dest = (destl, destr)@ such as @destl@ has length @n@.
--
-- 'split' is total: if @n@ is larger than the length of @dest@, then
-- @destr@ is empty.
split :: Int -> DArray a %1 -> (DArray a, DArray a)
split n (DArray mvec)
  | (ml, mr) <- MVector.splitAt n mvec =
      (DArray ml, DArray mr)

-- | Fills the destination array with the contents of given vector.
--
-- Errors if the given vector is smaller than the destination array.
mirror :: (HasCallStack) => Vector a -> (a %1 -> b) -> DArray b %1 -> ()
mirror v f arr =
  size arr & \(Ur sz, arr') ->
    if Vector.length v < sz
      then error "Destination.mirror: argument smaller than DArray" $ arr'
      else fromFunction (\t -> f (v ! t)) arr'

-- | Fill a destination array using the given index-to-value function.
fromFunction :: (Int -> b) -> DArray b %1 -> ()
fromFunction f (DArray mvec) = unsafeDupablePerformIO $ do
  let n = MVector.length mvec
  Prelude.sequence_ [MVector.unsafeWrite mvec m (f m) | m <- [0 .. n - 1]]

-- The use of the mutable array is linear, since getting the length does not
-- touch any elements, and each write fills in exactly one slot, so
-- each slot of the destination array is filled.
