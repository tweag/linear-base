{-# OPTIONS_GHC -Wno-unbanged-strict-patterns #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
--   Module: Vector
--   Description: A fast dynamic, mutable vector.
--
--   TODO:
--
--   * Is this fast?
--   * Add doc.
--   * Add more of the core API from Data.Vector
--   * Add control instances
--   * Dupable instance
--   * See if you can index by length nicely
module Data.Vector.Mutable.Linear
  ( Vector,
    singleton,
    singletonWithSize,
    read,
    write,
    snoc,
    length,
    resize
  )
where

import GHC.Exts
import GHC.Stack
import qualified Unsafe.Linear as Linear
import Unsafe.MutableArray
import Prelude.Linear hiding (read, length)
import Prelude ()

-- # Core data types
-------------------------------------------------------

type MutArr a = MutableArray# RealWorld a

-- | A dynamic mutable vector.
data Vector a where
  -- | Vec (memory allocated, current length) marray
  Vec :: (Int, Int) -> MutArr a -> Vector a

-- # API: Construction, Mutation, Queries
-------------------------------------------------------

-- | Singleton vector
singleton :: a -> (Vector a #-> Unrestricted b) -> Unrestricted b
singleton x f = f $ Vec (1,defaultSize) (newMutArr defaultSize x)

-- | Singleton with intial vector size
singletonWithSize :: a -> Int -> (Vector a #-> Unrestricted b) -> Unrestricted b
singletonWithSize x size f = f $ Vec (1,size) (newMutArr size x)

-- | Resize the vector with larger non-negative size
resize :: HasCallStack => Int -> Vector a -> Vector a
resize newSize (Vec (len,_) ma) = Vec (len,newSize) (resizeMutArr ma newSize)

-- | Length of the vector
length :: Vector a #-> (Int, Vector a)
length = Linear.toLinear unsafeLength
    where
      unsafeLength :: Vector a -> (Int, Vector a)
      unsafeLength v@(Vec (len, _) _) = (len, v)

-- | Insert at the end of the vector
snoc :: Vector a #-> a -> Vector a
snoc (Vec (len, size) ma) x
  | len < size = write (Vec (len + 1, size) ma) len x
  | otherwise = write (resize (size*2) (Vec (len,size) ma)) len x

-- | Write to an element already written to before.
-- Note: this will not write to elements beyond the current
-- length of the array.
write :: HasCallStack => Vector a #-> Int -> a -> Vector a
write = Linear.toLinear writeUnsafe
  where
    writeUnsafe :: Vector a -> Int -> a -> Vector a
    writeUnsafe v@(Vec (len, _) mutArr) ix val
      | indexInRange len ix = case writeMutArr mutArr ix val of () -> v
      | otherwise = error "Write index not in range."

-- | Read from a vector, with an in-range index.
read :: HasCallStack => Vector a #-> Int -> (Vector a, a)
read = Linear.toLinear readUnsafe
  where
    readUnsafe :: Vector a -> Int -> (Vector a, a)
    readUnsafe v@(Vec (len, _) mutArr) ix
      | indexInRange len ix = (v, readMutArr mutArr ix)
      | otherwise = error "Read index not in range."

-- # Internal library
-------------------------------------------------------
defaultSize :: Int
defaultSize = 16

-- | Argument order: indexInRange len ix
indexInRange :: Int -> Int -> Bool
indexInRange size ix = 0 <= ix && ix < size
