{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
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
    fromList,
    -- * Mutators
    write,
    resize,
    resizeSeed,
    -- * Accessors
    read,
    length,
    toList,
  )
where

import Data.Unrestricted.Linear
import GHC.Exts hiding (toList, fromList)
import GHC.Stack
import qualified Unsafe.Linear as Unsafe
import qualified Unsafe.MutableArray as Unsafe
import Prelude hiding (length, read)
import qualified Prelude

-- # Data types
-------------------------------------------------------------------------------

data Array a where
  Array :: Int -> (MutableArray# RealWorld a) -> Array a

-- # Creation
-------------------------------------------------------------------------------

-- | Allocate a constant array given a size and an initial value
-- The size must be greater than zero, otherwise this errors.
alloc :: HasCallStack =>
  Int -> a -> (Array a #-> Unrestricted b) -> Unrestricted b
alloc size x f
  | size > 0 = f (Array size (Unsafe.newMutArr size x))
  | otherwise = error $ "Trying to allocate an array of size " ++ show size

-- | Allocate an array from a non-empty list (and error on empty lists)
fromList :: HasCallStack =>
  [a] -> (Array a #-> Unrestricted b) -> Unrestricted b
fromList [] _ = error $ "Trying to allocate from an empty list."
fromList list@(x:_) (f :: Array a #-> Unrestricted b) =
  alloc (Prelude.length list) x insertThenf
  where
    insertThenf :: Array a #-> Unrestricted b
    insertThenf arr = f (doWrites (zip list [0..]) arr)

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

read :: HasCallStack => Array a #-> Int -> (Array a, a)
read = Unsafe.toLinear readUnsafe
  where
    readUnsafe :: Array a -> Int -> (Array a, a)
    readUnsafe arr@(Array size mutArr) ix
      | indexInRange size ix = (arr, Unsafe.readMutArr mutArr ix)
      | otherwise = error "Read index out of bounds."

-- | Using first element as seed, resize to a constant array
resize :: HasCallStack => Int -> Array a #-> Array a
resize newSize (Array _ mutArr) =
  Array newSize (Unsafe.newMutArr newSize (Unsafe.readMutArr mutArr 0))

-- | Resize to a new constant array given a seed value
resizeSeed :: HasCallStack => Int -> a -> Array a #-> Array a
resizeSeed newSize seed (Array _ _) =
  Array newSize (Unsafe.newMutArr newSize seed)

-- XXX: Replace with toVec
toList :: Array a #-> (Array a, [a])
toList = Unsafe.toLinear unsafetoList
  where
    unsafetoList :: Array a -> (Array a, [a])
    unsafetoList arr@(Array size _) = toListWalk 0 size arr []
      where
        toListWalk :: Int -> Int -> Array a -> [a] -> (Array a, [a])
        toListWalk ix size arr accum
          | ix == size = (arr, accum)
          | otherwise =
            toListWalk (ix + 1) size arr ((snd $ read arr ix) : accum)

-- # Instances
-------------------------------------------------------------------------------

instance Show a => Show (Array a) where
  show = show . snd . (\x -> toList x)

instance Consumable (Array a) where
  consume :: Array a #-> ()
  consume (Array _ _) = ()

-- # Internal library
-------------------------------------------------------------------------------
type Size = Int

indexInRange :: Size -> Int -> Bool
indexInRange size ix = 0 <= ix && ix < size
