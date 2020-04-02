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
--   - TODO:
--
--   - Add doc.
--   - XXX: is doubleVec functional???
--
--   - Functor Instance
--   - Dupable instance
module Data.Vector.Mutable.Linear
  ( Vector,
    singleton,
    read,
    write,
    snoc,
    length,
  )
where

import GHC.Exts
import GHC.Stack
import qualified Unsafe.Linear as Linear
import Prelude hiding (length, read)
import Unsafe.MutableArray

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
singleton :: a -> Vector a
singleton x = Vec (1,defaultSize) (newMutArr defaultSize x)

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
  | otherwise = write (doubleVec (Vec (len + 1, size) ma)) len x

-- | Write to an element already written to before.
-- Note: this will not write to elements beyond the current
-- length of the array.
write :: HasCallStack => Vector a #-> Int -> a -> Vector a
write = Linear.toLinear writeUnsafe
  where
    writeUnsafe :: Vector a -> Int -> a -> Vector a
    writeUnsafe v@(Vec (len, _) mutArr) ix val
      | indexInRange len ix = case writeMutArr mutArr ix val of () -> v
      | otherwise = error "Index not in range"

-- | Read from a vector, with an in-range index.
read :: HasCallStack => Vector a #-> Int -> (Vector a, Maybe a)
read = Linear.toLinear readUnsafe
  where
    readUnsafe :: Vector a -> Int -> (Vector a, Maybe a)
    readUnsafe v@(Vec (len, _) mutArr) ix
      | indexInRange len ix = (v, Just $ readMutArr mutArr ix)
      | otherwise = (v, Nothing)

-- # Internal library
-------------------------------------------------------
defaultSize :: Int
defaultSize = 16

doubleVec :: Vector a -> Vector a
doubleVec v@(Vec (_, size) _) =
  case newArray of
    (# _, ma' #) -> copyIntoDoubled v ma'
  where
    (_, Just firstElem) = read v 0
    (I# sizeX2) = 2 * size
    newArray = runRW# $ \stateRW ->
      newArray# sizeX2 firstElem stateRW

-- | Copy a vector into a mutable array of twice the size.
copyIntoDoubled :: Vector a -> MutArr a -> Vector a
copyIntoDoubled (Vec (len, size) ma) dest =
  case copiedArray of
    _ -> Vec (len, size * 2) dest
  where
    (I# len') = len
    (I# zero) = 0 :: Int
    copiedArray = runRW# $ \stateRW ->
      copyMutableArray# ma zero dest zero len' stateRW

-- | Argument order: indexInRange len ix
indexInRange :: Int -> Int -> Bool
indexInRange size ix = 0 <= ix && ix < size
