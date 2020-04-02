{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
--   Module: Array
--   Description: A fast fixed-size mutable array.
--
--   - TODO:
--   - Remove the use of error on indicies out of bound.
module Data.Linear.Array
  ( Array,
    alloc,
    write,
    read,
    length,
  )
where

import Data.Unrestricted.Linear (Unrestricted (..))
import GHC.Exts
import qualified Unsafe.Linear as Linear
import Prelude hiding (length, read)
import Data.Linear.Internal.MutableArray

-- # Data types
----------------------------

data Array a where
  Array :: Int -> (MutableArray# RealWorld a) -> Array a

-- # Creation
----------------------------

alloc :: Int -> a -> (Array a #-> Unrestricted b) -> Unrestricted b
alloc size init f = f (Array size (unsafeNewMutArr size init))

-- # Mutations and Reads
----------------------------

length :: Array a #-> (Int, Array a)
length = Linear.toLinear unsafeLength
  where
    unsafeLength :: Array a -> (Int, Array a)
    unsafeLength v@(Array size _) = (size, v)

write :: Array a #-> Int -> a -> Array a
write = Linear.toLinear writeUnsafe
  where
    writeUnsafe :: Array a -> Int -> a -> Array a
    writeUnsafe arr@(Array size mutArr) ix val
      | indexInRange size ix =
        case writeMutArr mutArr ix val of
          () -> arr
      | otherwise = error "Index not in range"

read :: Array a #-> Int -> (Array a, Maybe a)
read = Linear.toLinear readUnsafe
  where
    readUnsafe :: Array a -> Int -> (Array a, Maybe a)
    readUnsafe arr@(Array size mutArr) i@(I# ix)
      | indexInRange size i = (arr, Just $ readMutArr mutArr i)
      | otherwise = (arr, Nothing)

-- # Internal library
------------------------
type Size = Int

indexInRange :: Size -> Int -> Bool
indexInRange size ix = 0 <= ix && ix < size
