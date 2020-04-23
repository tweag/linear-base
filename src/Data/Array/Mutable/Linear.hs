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
--   Module: Array
--   Description: A fast fixed-size mutable array.
--
--   - TODO:
--   - Add more of the API from Data.Vector
--   - Remove the use of error on indicies out of bound.
--   - Use toVec instead of toList
--   - Read should return an (Unrestricted a)
module Data.Array.Mutable.Linear
  ( Array,
    alloc,
    write,
    read,
    length,
    resize,
    resizeSeed,
    toList,
    fromList
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
