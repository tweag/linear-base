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
-- @Array a #-> Unrestricted b@ and feed it to 'alloc' or 'fromList'.
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
-- > isTrue = unUnrestricted $ Array.fromList [0..10] isFirstZero
-- >
-- > isFalse :: Bool
-- > isFalse = unUnrestricted $ Array.fromList [1,2,3] isFirstZero
-- >
-- > isFirstZero :: Array.Array Int #-> Unrestricted Bool
-- > isFirstZero arr = withReadingFirst (Array.read arr 0)
-- >   where
-- >     withReadingFirst :: (Array.Array Int, Int) #-> Unrestricted Bool
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
    resize,
    -- * Accessors
    read,
    length,
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
import Prelude hiding (length, read)
import qualified Prelude

-- # Data types
-------------------------------------------------------------------------------

data Array a where
  Array :: Int -> (MutableArray# RealWorld a) -> Array a

-- # Creation
-------------------------------------------------------------------------------

-- | Allocate a constant array given a size and an initial value
-- The size must be non-negative, otherwise this errors.
alloc :: HasCallStack =>
  Int -> a -> (Array a #-> Unrestricted b) #-> Unrestricted b
alloc size x f
  | size >= 0 = f (Array size (Unsafe.newMutArr size x))
  | otherwise =
    (error ("Trying to allocate an array of size " ++ show size) :: x #-> x)
    (f undefined)

-- | Allocate a constant array given a size and an initial value,
-- using another array as a uniqueness proof.
allocBeside :: Int -> a -> Array b #-> (Array b, Array a)
allocBeside size val orig
  | size >= 0 = (orig, Array size (Unsafe.newMutArr size val))
  | otherwise = orig `lseq` error ("Trying to allocate an array of size " ++ show size)

-- | Allocate an array from a list
fromList :: HasCallStack =>
  [a] -> (Array a #-> Unrestricted b) #-> Unrestricted b
fromList list (f :: Array a #-> Unrestricted b) =
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

length :: Array a #-> (Array a, Int)
length = Unsafe.toLinear unsafeLength
  where
    unsafeLength :: Array a -> (Array a, Int)
    unsafeLength v@(Array size _) = (v, size)

write :: HasCallStack => Array a #-> Int -> a -> Array a
write = Unsafe.toLinear writeUnsafe
  where
    writeUnsafe :: Array a -> Int -> a -> Array a
    writeUnsafe arr@(Array size mutArr) ix val
      | indexInRange size ix =
        case Unsafe.writeMutArr mutArr ix val of
          () -> arr
      | otherwise = error "Write index out of bounds."

read :: HasCallStack => Array a #-> Int -> (Array a, Unrestricted a)
read = Unsafe.toLinear readUnsafe
  where
    readUnsafe :: Array a -> Int -> (Array a, Unrestricted a)
    readUnsafe arr@(Array size mutArr) ix
      | indexInRange size ix =
          let !(# a #) = Unsafe.readMutArr mutArr ix
          in  (arr, Unrestricted a)
      | otherwise = error "Read index out of bounds."

-- | Resize an array. That is, given an array, a target size, and a seed
-- value; resize the array to the given size using the seed value to fill
-- in the new cells when necessary and copying over all the unchanged cells.
--
-- Target size should be non-negative.
--
-- @
-- let b = resize n x a,
--   then length b = n,
--   and b[i] = a[i] for i < length a,
--   and b[i] = x for length a <= i < n.
-- @
resize :: HasCallStack => Int -> a -> Array a #-> Array a
resize newSize seed (Array _ mut)
  | newSize < 0 =
      error "Trying to resize to a negative size."
  | otherwise =
      Array newSize (Unsafe.resizeMutArr mut seed newSize)

-- XXX: Replace with toVec
toList :: Array a #-> (Array a, Unrestricted [a])
toList arr = length arr & \case
  (arr', len) -> move len & \case
    Unrestricted len' -> toListWalk (len' - 1) arr' (Unrestricted [])
  where
  toListWalk :: Int -> Array a #-> Unrestricted [a] -> (Array a, Unrestricted [a])
  toListWalk ix arr accum
    | ix < 0 = (arr, accum)
    | otherwise = read arr ix & \case
        (arr', Unrestricted x) -> toListWalk (ix - 1) arr' ((x:) Control.<$> accum)

-- # Instances
-------------------------------------------------------------------------------

instance Show a => Show (Array a) where
  show = show . forget unUnrestricted . snd . (\x -> toList x)

instance Consumable (Array a) where
  consume :: Array a #-> ()
  consume (Array _ _) = ()

-- # Internal library
-------------------------------------------------------------------------------
type Size = Int

indexInRange :: Size -> Int -> Bool
indexInRange size ix = 0 <= ix && ix < size
