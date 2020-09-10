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
-- @Vector a #-> Ur b@ and feed it to 'constant' or 'fromList'.
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
-- > isTrue = unur $ Vector.fromList [0..10] isFirstZero
-- >
-- > isFalse :: Bool
-- > isFalse = unur $ Vector.fromList [1,2,3] isFirstZero
-- >
-- > isFirstZero :: Vector.Vector Int #-> Ur Bool
-- > isFirstZero arr = withReadingFirst (Vector.read arr 0)
-- >   where
-- >     withReadingFirst :: (Vector.Vector Int, Int) #-> Ur Bool
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
    unsafeWrite,
    snoc,
    -- * Accessors
    read,
    unsafeRead,
    size,
    toList,
  )
where

import GHC.Stack
import Prelude.Linear hiding (read)
import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array

-- # Core data types
-------------------------------------------------------------------------------

-- | A dynamic mutable vector.
data Vector a where
  Vec ::
    -- ^ Current size
    Int ->
    -- ^ Underlying array (has size equal to or larger than the vectors)
    Array a #->
    Vector a

-- # API: Construction, Mutation, Queries
-------------------------------------------------------------------------------

-- | Create a 'Vector' from an 'Array'. Result will have the size and capacity
-- equal to the size of the given array.
fromArray :: HasCallStack => Array a #-> Vector a
fromArray arr =
  Array.size arr
    & \(arr', Ur size') -> Vec size' arr'

-- Allocate an empty vector
empty :: (Vector a #-> Ur b) #-> Ur b
empty f = Array.fromList [] (f . fromArray)

-- | Allocate a constant vector of a given non-negative size (and error on a
-- bad size)
constant :: HasCallStack =>
  Int -> a -> (Vector a #-> Ur b) #-> Ur b
constant size' x f
  | size' < 0 =
      (error ("Trying to construct a vector of size " ++ show size') :: x #-> x)
      (f undefined)
  | otherwise = Array.alloc size' x (f . fromArray)

-- | Allocator from a list
fromList :: HasCallStack => [a] -> (Vector a #-> Ur b) #-> Ur b
fromList xs f = Array.fromList xs (f . fromArray)

-- | Number of elements inside the vector
size :: Vector a #-> (Vector a, Ur Int)
size (Vec size' arr) = (Vec size' arr, Ur size')

-- | Insert at the end of the vector
snoc :: HasCallStack => Vector a #-> a -> Vector a
snoc (Vec size' arr) x =
  Array.size arr & \(arr', Ur cap) ->
    if size' < cap
    then write (Vec (size' + 1) arr') size' x
    else write (unsafeResize ((max size' 1) * 2) (Vec (size' + 1) arr')) size' x

-- | Write to an element . Note: this will not write to elements beyond the
-- current size of the vector and will error instead.
write :: HasCallStack => Vector a #-> Int -> a -> Vector a
write (Vec size' arr) ix val
  | indexInRange size' ix = Vec size' (Array.unsafeWrite arr ix val)
  | otherwise = arr `lseq` error "Write index not in range."

-- | Same as 'write', but does not do bounds-checking. The behaviour is undefined
-- when passed an invalid index.
unsafeWrite :: HasCallStack => Vector a #-> Int -> a -> Vector a
unsafeWrite (Vec size' arr) ix val =
  Vec size' (Array.unsafeWrite arr ix val)

-- | Read from a vector, with an in-range index and error for an index that is
-- out of range (with the usual range @0..size-1@).
read :: HasCallStack => Vector a #-> Int -> (Vector a, Ur a)
read (Vec size' arr) ix
  | indexInRange size' ix =
      Array.unsafeRead arr ix
        & \(arr', val) -> (Vec size' arr', val)
  | otherwise = arr `lseq` error "Read index not in range."

-- | Same as 'read', but does not do bounds-checking. The behaviour is undefined
-- when passed an invalid index.
unsafeRead :: HasCallStack => Vector a #-> Int -> (Vector a, Ur a)
unsafeRead (Vec size' arr) ix =
  Array.unsafeRead arr ix
    & \(arr', val) -> (Vec size' arr', val)

-- See the note at Array.toListLazy

-- | Return the vector elements as a lazy list.
toList :: Vector a #-> Ur [a]
toList (Vec s arr) =
  Array.toList arr & \(Ur xs) ->
    Ur (take s xs)

-- # Instances
-------------------------------------------------------------------------------

instance Consumable (Vector a) where
  consume (Vec _ arr) = consume arr

-- # Internal library
-------------------------------------------------------------------------------

-- | Resize the vector to a non-negative size. In-range elements are preserved,
-- the possible new elements are bottoms.
unsafeResize :: HasCallStack => Int -> Vector a #-> Vector a
unsafeResize newSize (Vec size' ma) =
  Vec
    (min size' newSize)
    (Array.resize
      newSize
      (error "access to uninitialized vector index")
      ma
    )

-- | Argument order: indexInRange size ix
indexInRange :: Int -> Int -> Bool
indexInRange size' ix = 0 <= ix && ix < size'
