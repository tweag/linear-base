{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- This module provides a pure linear interface for arrays with in-place
-- mutation.
--
-- To use these mutable arrays, create a linear computation of type
-- @Array a #-> Ur b@ and feed it to 'alloc' or 'fromList'.
--
-- == A Tiny Example
--
-- > {-# LANGUAGE LinearTypes #-}
-- > import Prelude.Linear
-- > import Data.Unrestricted.Linear
-- > import qualified Unsafe.Linear as Unsafe
-- > import qualified Data.Array.Mutable.Linear as Array
-- >
-- > isTrue :: Bool
-- > isTrue = unur $ Array.fromList [0..10] isFirstZero
-- >
-- > isFalse :: Bool
-- > isFalse = unur $ Array.fromList [1,2,3] isFirstZero
-- >
-- > isFirstZero :: Array.Array Int #-> Ur Bool
-- > isFirstZero arr = withReadingFirst (Array.read arr 0)
-- >   where
-- >     withReadingFirst :: (Array.Array Int, Int) #-> Ur Bool
-- >     withReadingFirst (arr, i) = lseq arr $ move (i === 0)
-- >
-- > (===) :: (Num a, Eq a) => a #-> a #-> Bool
-- > (===) = Unsafe.toLinear2 (==)
module Data.Array.Mutable.Linear
  ( -- * Mutable Linear Arrays
    Array,
    -- * Performing Computations with Arrays
    alloc,
    allocBeside,
    fromList,
    -- * Mutators
    write,
    unsafeWrite,
    resize,
    -- * Accessors
    read,
    unsafeRead,
    size,
    toList,
  )
where

import Data.Unrestricted.Linear
import GHC.Exts hiding (toList, fromList)
import GHC.Stack
import qualified Data.Functor.Linear as Control
import qualified Unsafe.Linear as Unsafe
import qualified Unsafe.MutableArray as Unsafe
import Prelude.Linear ((&), forget)
import Prelude hiding (read)

-- # Data types
-------------------------------------------------------------------------------

data Array a where
  Array :: Int -> (MutableArray# RealWorld a) -> Array a

-- # Creation
-------------------------------------------------------------------------------

-- | Allocate a constant array given a size and an initial value
-- The size must be non-negative, otherwise this errors.
alloc :: HasCallStack =>
  Int -> a -> (Array a #-> Ur b) #-> Ur b
alloc size' x f
  | size' >= 0 = f (Array size' (Unsafe.newMutArr size' x))
  | otherwise =
    (error ("Trying to allocate an array of size " ++ show size') :: x #-> x)
    (f undefined)

-- | Allocate a constant array given a size and an initial value,
-- using another array as a uniqueness proof.
allocBeside :: Int -> a -> Array b #-> (Array b, Array a)
allocBeside size val orig
  | size >= 0 = (orig, Array size (Unsafe.newMutArr size val))
  | otherwise = orig `lseq` error ("Trying to allocate an array of size " ++ show size)

-- | Allocate an array from a list
fromList :: HasCallStack =>
  [a] -> (Array a #-> Ur b) #-> Ur b
fromList list (f :: Array a #-> Ur b) =
  alloc
    (Prelude.length list)
    (error "invariant violation: unintialized array position")
    (\arr -> f (insert arr))
  where
    insert :: Array a #-> Array a
    insert = doWrites (zip list [0..])

    doWrites :: [(a,Int)] -> Array a #-> Array a
    doWrites [] arr = arr
    doWrites ((a,ix):xs) arr = doWrites xs (write arr ix a)

-- # Mutations and Reads
-------------------------------------------------------------------------------

size :: Array a #-> (Array a, Ur Int)
size (Array size' arr) =  (Array size' arr, Ur size')

-- | Writes a value to an index of an Array. The index should be less than the
-- arrays size, otherwise this errors.
write :: HasCallStack => Array a #-> Int -> a -> Array a
write = Unsafe.toLinear writeUnsafe
  where
    writeUnsafe :: Array a -> Int -> a -> Array a
    writeUnsafe arr@(Array size' _) ix val
      | indexInRange size' ix = unsafeWrite arr ix val
      | otherwise = error "Write index out of bounds."

-- | Same as 'write', but does not do bounds-checking. The behaviour is undefined
-- if an out-of-bounds index is provided.
unsafeWrite :: Array a #-> Int -> a -> Array a
unsafeWrite (Array size' arr) ix val =
  case Unsafe.writeMutArr arr ix val of
    () -> Array size' arr

-- | Read an index from an Array. The index should be less than the arrays size,
-- otherwise this errors.
read :: HasCallStack => Array a #-> Int -> (Array a, Ur a)
read = Unsafe.toLinear readUnsafe
  where
    readUnsafe :: Array a -> Int -> (Array a, Ur a)
    readUnsafe arr@(Array size _) ix
      | indexInRange size ix = unsafeRead arr ix
      | otherwise = error "Read index out of bounds."

-- | Same as read, but does not do bounds-checking. The behaviour is undefined
-- if an out-of-bounds index is provided.
unsafeRead :: Array a #-> Int -> (Array a, Ur a)
unsafeRead (Array size arr) ix =
  let !(# a #) = Unsafe.readMutArr arr ix
  in  (Array size arr, Ur a)

-- | Resize an array. That is, given an array, a target size, and a seed
-- value; resize the array to the given size using the seed value to fill
-- in the new cells when necessary and copying over all the unchanged cells.
--
-- Target size should be non-negative.
--
-- @
-- let b = resize n x a,
--   then size b = n,
--   and b[i] = a[i] for i < size a,
--   and b[i] = x for size a <= i < n.
-- @
resize :: HasCallStack => Int -> a -> Array a #-> Array a
resize newSize seed (Array _ mut)
  | newSize < 0 =
      error "Trying to resize to a negative size."
  | otherwise =
      Array newSize (Unsafe.resizeMutArr mut seed newSize)

-- XXX: Replace with toVec
toList :: Array a #-> (Array a, Ur [a])
toList arr = size arr & \case
  (arr', Ur len) ->
    toListWalk (len - 1) arr' (Ur [])
  where
  toListWalk :: Int -> Array a #-> Ur [a] -> (Array a, Ur [a])
  toListWalk ix arr accum
    | ix < 0 = (arr, accum)
    | otherwise = read arr ix & \case
        (arr', Ur x) -> toListWalk (ix - 1) arr' ((x:) Control.<$> accum)

-- # Instances
-------------------------------------------------------------------------------

instance Show a => Show (Array a) where
  show = show . forget unur . snd . (\x -> toList x)

instance Consumable (Array a) where
  consume :: Array a #-> ()
  consume (Array _ _) = ()

-- # Internal library
-------------------------------------------------------------------------------
type Size = Int

indexInRange :: Size -> Int -> Bool
indexInRange size ix = 0 <= ix && ix < size
