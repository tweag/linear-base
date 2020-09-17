{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unbanged-strict-patterns #-}

-- | Mutable vectors with a linear API.
--
-- Vectors are arrays that grow automatically, that you can append to with
-- 'push'. They never shrink automatically to reduce unnecessary copying,
-- use 'shrinkToFit' to get rid of the wasted space.
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
    push,
    pop,
    slice,
    shrinkToFit,
    -- * Accessors
    read,
    unsafeRead,
    size,
    capacity,
    toList,
  )
where

import GHC.Stack
import Prelude.Linear hiding (read)
import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array

-- # Constants
-------------------------------------------------------------------------------

-- | When growing the vector, capacity will be multiplied by this number.
--
-- This is usually chosen between 1.5 and 2; 2 being the most common.
constGrowthFactor :: Int
constGrowthFactor = 2

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
--
-- This is a constant time operation.
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

-- | Number of elements inside the vector.
--
-- This might be different than how much actual memory the vector is using.
-- For that, see: 'capacity'.
size :: Vector a #-> (Vector a, Ur Int)
size (Vec size' arr) = (Vec size' arr, Ur size')

-- | Capacity of a vector. In other words, the number of elements
-- the vector can contain before it is copied to a bigger array.
capacity :: Vector a #-> (Vector a, Ur Int)
capacity (Vec s arr) =
  Array.size arr & \(arr', cap) -> (Vec s arr', cap)

-- | Insert at the end of the vector. This will grow the vector if there
-- is no empty space.
push :: Vector a #-> a -> Vector a
push vec x =
  growToFit 1 vec & \(Vec s arr) ->
    write (Vec (s + 1) arr) s x

-- | Pop from the end of the vector. This will never shrink the vector, use
-- 'shrinkToFit' to remove the wasted space.
pop :: Vector a #-> (Vector a, Ur (Maybe a))
pop vec =
  size vec & \case
    (vec', Ur 0) ->
      (vec', Ur Nothing)
    (vec', Ur s) ->
      read vec' (s-1) & \(Vec _ arr, Ur a) ->
        ( Vec (s-1) arr
        , Ur (Just a)
        )

-- | Write to an element . Note: this will not write to elements beyond the
-- current size of the vector and will error instead.
write :: HasCallStack => Vector a #-> Int -> a -> Vector a
write vec ix val =
  unsafeWrite (assertIndexInRange ix vec) ix val

-- | Same as 'write', but does not do bounds-checking. The behaviour is undefined
-- when passed an invalid index.
unsafeWrite :: HasCallStack => Vector a #-> Int -> a -> Vector a
unsafeWrite (Vec size' arr) ix val =
  Vec size' (Array.unsafeWrite arr ix val)

-- | Read from a vector, with an in-range index and error for an index that is
-- out of range (with the usual range @0..size-1@).
read :: HasCallStack => Vector a #-> Int -> (Vector a, Ur a)
read vec ix =
  unsafeRead (assertIndexInRange ix vec) ix

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

-- | Resize the vector to not have any wasted memory (size == capacity). This
-- returns a semantically identical vector.
shrinkToFit :: Vector a #-> Vector a
shrinkToFit vec =
  capacity vec & \(vec', Ur cap) ->
    size vec' & \(vec'', Ur s') ->
      if cap > s'
      then unsafeResize s' vec''
      else vec''

-- | Return a slice of the vector with given size, starting from an offset.
--
-- Start offset + target size should be within the input vector, and both should
-- be non-negative.
--
-- This is a constant time operation if the start offset is 0. Use 'shrinkToFit'
-- to remove the possible wasted space if necessary.
slice :: Int -> Int -> Vector a #-> Vector a
slice from newSize (Vec oldSize arr) =
  if oldSize < from + newSize
  then arr `lseq` error "Slice index out of bounds"
  else if from == 0
       then Vec newSize arr
       else Array.slice from newSize arr & \(oldArr, newArr) ->
              oldArr `lseq` fromArray newArr

-- # Instances
-------------------------------------------------------------------------------

instance Consumable (Vector a) where
  consume (Vec _ arr) = consume arr

-- # Internal library
-------------------------------------------------------------------------------

-- | Grows the vector to the closest power of growthFactor to
-- fit at least n more elements.
growToFit :: HasCallStack => Int -> Vector a #-> Vector a
growToFit n vec =
  capacity vec & \(vec', Ur cap) ->
    size vec' & \(vec'', Ur s') ->
      if s' + n <= cap
      then vec''
      else
        let -- Calculate the closest power of growth factor
            -- larger than required size.
            newSize =
              constGrowthFactor -- This constant is defined above.
                ^ (ceiling :: Double -> Int)
                    (logBase
                      (fromIntegral constGrowthFactor)
                      (fromIntegral (s' + n))) -- this is always
                                               -- > 0 because of
                                               -- the if condition
        in  unsafeResize
              newSize
              vec''

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

-- | Check if given index is within the Vector, otherwise panic.
assertIndexInRange :: HasCallStack => Int -> Vector a #-> Vector a
assertIndexInRange i vec =
  size vec & \(vec', Ur s) ->
    if 0 <= i && i < s
    then vec'
    else vec' `lseq` error "Vector: index out of bounds"
