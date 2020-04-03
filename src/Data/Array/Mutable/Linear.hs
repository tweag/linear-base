{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
--   Module: Array
--   Description: A fast fixed-size mutable array.
--
--   - TODO:
--   - Add more of the API from Data.Vector
--   - Remove the use of error on indicies out of bound.
module Data.Array.Mutable.Linear
  ( Array,
    alloc,
    write,
    read,
    length,
    resize
  )
where

import Data.Unrestricted.Linear (Unrestricted (..))
import GHC.Exts
import GHC.Stack
import qualified Unsafe.Linear as Linear
import Prelude hiding (length, read)
import Unsafe.MutableArray

-- # Data types
----------------------------

data Array a where
  Array :: Int -> (MutableArray# RealWorld a) -> Array a

-- # Creation
----------------------------

-- | Allocate a constant array given a size and an initial value
alloc :: Int -> a -> (Array a #-> Unrestricted b) -> Unrestricted b
alloc size x f = f (Array size (newMutArr size x))

-- # Mutations and Reads
----------------------------

length :: Array a #-> (Int, Array a)
length = Linear.toLinear unsafeLength
  where
    unsafeLength :: Array a -> (Int, Array a)
    unsafeLength v@(Array size _) = (size, v)

write :: HasCallStack => Array a #-> Int -> a #-> Array a
write = Linear.coerce writeUnsafe
  where
    writeUnsafe :: Array a -> Int -> a -> Array a
    writeUnsafe arr@(Array size mutArr) ix val
      | indexInRange size ix =
        case writeMutArr mutArr ix val of
          () -> arr
      | otherwise = error "Write index out of bounds."

read :: HasCallStack => Array a #-> Int #-> (Array a, a)
read = Linear.coerce readUnsafe
  where
    readUnsafe :: Array a -> Int -> (Array a, a)
    readUnsafe arr@(Array size mutArr) ix
      | indexInRange size ix = (arr, readMutArr mutArr ix)
      | otherwise = error "Read index out of bounds."

resize :: HasCallStack => Int -> Array a #-> Array a
resize newSize (Array _ mutArr) =
  Array newSize (resizeMutArr mutArr newSize)

-- # Internal library
------------------------
type Size = Int

indexInRange :: Size -> Int -> Bool
indexInRange size ix = 0 <= ix && ix < size
