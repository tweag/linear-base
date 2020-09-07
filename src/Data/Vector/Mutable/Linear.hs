{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
    empty,
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
import Prelude.Linear hiding (length, read)
import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array

-- # Core data types
-------------------------------------------------------------------------------

-- | A dynamic mutable vector.
data Vector a where
  -- | Vec current length, memory allocated) marray
  Vec ::
    -- ^ Current size
    Int ->
    -- ^ Underlying array (has size equal to or larger than the vectors)
    Array a #->
    Vector a

-- # API: Construction, Mutation, Queries
-------------------------------------------------------------------------------

-- | Allocator from an array
fromArray :: HasCallStack => Array a #-> Vector a
fromArray arr =
  Array.length arr
    & \(arr', size') -> move size'
    & \(Unrestricted size) -> Vec size arr'

-- Allocate an empty vector
empty :: (Vector a #-> Unrestricted b) #-> Unrestricted b
empty f = Array.fromList [] (f . fromArray)

-- | Allocate a constant vector of a given non-negative size (and error on a
-- bad size)
constant :: HasCallStack =>
  Int -> a -> (Vector a #-> Unrestricted b) #-> Unrestricted b
constant size x f
  | size < 0 =
      (error ("Trying to construct a vector of size " ++ show size) :: x #-> x)
      (f undefined)
  | otherwise = Array.alloc size x (f . fromArray)

-- | Allocator from a list
fromList :: HasCallStack => [a] -> (Vector a #-> Unrestricted b) #-> Unrestricted b
fromList xs f = Array.fromList xs (f . fromArray)

-- | Length of the vector
length :: Vector a #-> (Vector a, Int)
length (Vec len arr) = (Vec len arr, len)

-- | Insert at the end of the vector
snoc :: HasCallStack => Vector a #-> a -> Vector a
snoc (Vec size arr) x =
  Array.length arr & \(arr', cap') ->
    move cap' & \(Unrestricted cap) ->
      if size < cap
      then write (Vec (size + 1) arr') size x
      else write (unsafeResize (max size 1 * 2) (Vec (size + 1) arr')) size x

-- | Write to an element already written to before.  Note: this will not write
-- to elements beyond the current length of the array and will error instead.
write :: HasCallStack => Vector a #-> Int -> a -> Vector a
write (Vec size arr) ix val
  | indexInRange size ix = Vec size (Array.unsafeWrite arr ix val)
  | otherwise = arr `lseq` error "Write index not in range."

-- | Read from a vector, with an in-range index and error for an index that is
-- out of range (with the usual range @0..length-1@).
read :: HasCallStack => Vector a #-> Int -> (Vector a, Unrestricted a)
read (Vec size arr) ix
  | indexInRange size ix =
      Array.unsafeRead arr ix
        & \(arr', val) -> (Vec size arr', val)
  | otherwise = arr `lseq` error "Read index not in range."

-- # Instances
-------------------------------------------------------------------------------

instance Consumable (Vector a) where
  consume (Vec _ arr) = consume arr

-- # Internal library
-------------------------------------------------------------------------------

-- | Resize the vector to a non-negative size
unsafeResize :: HasCallStack => Int -> Vector a #-> Vector a
unsafeResize newSize (Vec len ma) =
  Vec
    (min len newSize)
    (Array.resize
      newSize
      (error "access to uninitialized vector index")
      ma
    )

-- | Argument order: indexInRange len ix
indexInRange :: Int -> Int -> Bool
indexInRange size ix = 0 <= ix && ix < size
