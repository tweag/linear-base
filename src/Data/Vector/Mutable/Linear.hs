{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unbanged-strict-patterns #-}

-- | Mutable vectors with a linear API.
--
-- Vectors are arrays that grow automatically, that you can append to with
-- 'snoc'.
--
-- To use mutable vectors, create a linear computation of type
-- @Vector a #-> Unrestricted b@ and feed it to 'constant' or 'fromList'.
--
-- == Example
--
-- > {-# LANGUAGE LinearTypes #-}
-- > import Prelude.Linear
-- > import Data.Unrestricted.Linear
-- > import qualified Unsafe.Linear as Unsafe
-- > import qualified Data.Vector.Mutable.Linear as Vector
-- >
-- > isTrue :: Bool
-- > isTrue = unUnrestricted $ Vector.fromList [0..10] isFirstZero
-- >
-- > isFalse :: Bool
-- > isFalse = unUnrestricted $ Vector.fromList [1,2,3] isFirstZero
-- >
-- > isFirstZero :: Vector.Vector Int #-> Unrestricted Bool
-- > isFirstZero arr = withReadingFirst (Vector.read arr 0)
-- >   where
-- >     withReadingFirst :: (Vector.Vector Int, Int) #-> Unrestricted Bool
-- >     withReadingFirst (arr, i) = lseq arr $ move (i === 0)
-- >
-- > (===) :: (Num a, Eq a) => a #-> a #-> Bool
-- > (===) = Unsafe.toLinear2 (==)
--
--
module Data.Vector.Mutable.Linear
  ( -- * A mutable vector
    Vector,
    -- * Run a computation with a vector
    constant,
    fromList,
    -- * Mutators
    write,
    snoc,
    -- * Accessors
    read,
    length,
  )
where

import GHC.Exts hiding (fromList)
import GHC.Stack
import qualified Prelude.Linear as Linear
import Data.Unrestricted.Linear
import Prelude hiding (read, length)
import qualified Prelude as Prelude
import qualified Unsafe.Linear as Unsafe
import qualified Unsafe.MutableArray as Unsafe

-- # Core data types
-------------------------------------------------------------------------------

type MutArr a = MutableArray# RealWorld a

-- | A dynamic mutable vector.
data Vector a where
  -- | Vec (current length, memory allocated) marray
  Vec :: (Int, Int) -> MutArr a -> Vector a

-- # API: Construction, Mutation, Queries
-------------------------------------------------------------------------------

-- | Allocate a constant vector of a given non-zero size (and error on a bad
-- size)
constant :: HasCallStack =>
  Int -> a -> (Vector a #-> Unrestricted b) -> Unrestricted b
constant size x f
  | size <= 0 = error ("Trying to construct a vector of size " ++ show size)
  | otherwise = f Linear.$ Vec (size, size) (Unsafe.newMutArr size x)

-- XXX: long line below
-- | Allocator from a non-empty list (and error on empty lists)
fromList :: HasCallStack => [a] -> (Vector a #-> Unrestricted b) -> Unrestricted b
fromList [] _ = error "Trying to allocate a vector from an empty list"
fromList xs@(x:_) (f :: Vector a #-> Unrestricted b) =
  constant (Prelude.length xs) x buildThenRun
  where
    buildThenRun :: Vector a #-> Unrestricted b
    buildThenRun vec = f Linear.$ doWrites (zip xs [0..]) vec

    doWrites :: [(a,Int)] -> Vector a #-> Vector a
    doWrites [] vec = vec
    doWrites ((a,ix):ws) vec = doWrites ws (write vec ix a)

-- | Length of the vector
length :: Vector a #-> (Vector a, Int)
length = Unsafe.toLinear unsafeLength
  where
    unsafeLength :: Vector a -> (Vector a, Int)
    unsafeLength v@(Vec (len, _) _) = (v, len)

-- | Insert at the end of the vector
snoc :: HasCallStack => Vector a #-> a -> Vector a
snoc (Vec (len, size) ma) x
  | len < size = write (Vec (len + 1, size) ma) len x
  | otherwise = write (unsafeResize (size * 2) (Vec (len + 1, size) ma)) len x

-- | Write to an element already written to before.  Note: this will not write
-- to elements beyond the current length of the array and will error instead.
write :: HasCallStack => Vector a #-> Int -> a -> Vector a
write = Unsafe.toLinear writeUnsafe
  where
    writeUnsafe :: Vector a -> Int -> a -> Vector a
    writeUnsafe v@(Vec (len, _) mutArr) ix val
      | indexInRange len ix = case Unsafe.writeMutArr mutArr ix val of () -> v
      | otherwise = error "Write index not in range."

-- | Read from a vector, with an in-range index and error for an index that is
-- out of range (with the usual range @0..length-1@).
read :: HasCallStack => Vector a #-> Int -> (Vector a, a)
read = Unsafe.toLinear readUnsafe
  where
    readUnsafe :: Vector a -> Int -> (Vector a, a)
    readUnsafe v@(Vec (len, _) mutArr) ix
      | indexInRange len ix = (v, Unsafe.readMutArr mutArr ix)
      | otherwise = error "Read index not in range."

-- # Instances
-------------------------------------------------------------------------------

instance Consumable (Vector a) where
  consume (Vec _ _) = ()

-- # Internal library
-------------------------------------------------------------------------------

-- | Resize the vector with larger non-negative size
unsafeResize :: HasCallStack => Int -> Vector a -> Vector a
unsafeResize newSize (Vec (len, _) ma) =
  Vec (len, newSize) (Unsafe.resizeMutArr ma newSize)

-- | Argument order: indexInRange len ix
indexInRange :: Int -> Int -> Bool
indexInRange size ix = 0 <= ix && ix < size
