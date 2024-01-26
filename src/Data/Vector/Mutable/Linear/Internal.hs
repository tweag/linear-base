{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unbanged-strict-patterns #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Vector.Mutable.Linear.Internal where

import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
import qualified Data.Functor.Linear as Data
import Data.Monoid.Linear
import qualified Data.Vector as Vector
import GHC.Stack
import Prelude.Linear hiding (filter, mapMaybe, read)
import qualified Unsafe.Linear as Unsafe
import qualified Prelude

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
  -- | Current size
  Vec ::
    -- | Underlying array (has size equal to or larger than the vectors)
    Int ->
    Array a %1 ->
    Vector a

-- # API: Construction, Mutation, Queries
-------------------------------------------------------------------------------

-- | Create a 'Vector' from an 'Array'. Result will have the size and capacity
-- equal to the size of the given array.
--
-- This is a constant time operation.
fromArray :: (HasCallStack) => Array a %1 -> Vector a
fromArray arr =
  Array.size arr
    & \(Ur size', arr') -> Vec size' arr'

-- Allocate an empty vector
empty :: (Movable b) => (Vector a %1 -> b) %1 -> b
empty f = Array.fromList [] (f . fromArray)

-- | Allocate a constant vector of a given non-negative size (and error on a
-- bad size)
constant ::
  (HasCallStack, Movable b) =>
  Int ->
  a ->
  (Vector a %1 -> b) %1 ->
  b
constant size' x f
  | size' < 0 =
      (error ("Trying to construct a vector of size " ++ show size') :: x %1 -> x)
        (f undefined)
  | otherwise = Array.alloc size' x (f . fromArray)

-- | Allocator from a list
fromList :: (HasCallStack, Movable b) => [a] -> (Vector a %1 -> b) %1 -> b
fromList xs f = Array.fromList xs (f . fromArray)

-- | Number of elements inside the vector.
--
-- This might be different than how much actual memory the vector is using.
-- For that, see: 'capacity'.
size :: Vector a %1 -> (Ur Int, Vector a)
size (Vec size' arr) = (Ur size', Vec size' arr)

-- | Capacity of a vector. In other words, the number of elements
-- the vector can contain before it is copied to a bigger array.
capacity :: Vector a %1 -> (Ur Int, Vector a)
capacity (Vec s arr) =
  Array.size arr & \(cap, arr') -> (cap, Vec s arr')

-- | Insert at the end of the vector. This will grow the vector if there
-- is no empty space.
push :: a -> Vector a %1 -> Vector a
push x vec =
  growToFit 1 vec & \(Vec s arr) ->
    unsafeSet s x (Vec (s + 1) arr)

-- | Pop from the end of the vector. This will never shrink the vector, use
-- 'shrinkToFit' to remove the wasted space.
pop :: Vector a %1 -> (Ur (Maybe a), Vector a)
pop vec =
  case size vec of
    (Ur 0, vec') ->
      (Ur Nothing, vec')
    (Ur s, vec') ->
      get (s - 1) vec' & \(Ur a, Vec _ arr) ->
        ( Ur (Just a),
          Vec (s - 1) arr
        )

-- | Write to an element . Note: this will not write to elements beyond the
-- current size of the vector and will error instead.
set :: (HasCallStack) => Int -> a -> Vector a %1 -> Vector a
set ix val vec =
  unsafeSet ix val (assertIndexInRange ix vec)

-- | Same as 'write', but does not do bounds-checking. The behaviour is undefined
-- when passed an invalid index.
unsafeSet :: (HasCallStack) => Int -> a -> Vector a %1 -> Vector a
unsafeSet ix val (Vec size' arr) =
  Vec size' (Array.unsafeSet ix val arr)

-- | Read from a vector, with an in-range index and error for an index that is
-- out of range (with the usual range @0..size-1@).
get :: (HasCallStack) => Int -> Vector a %1 -> (Ur a, Vector a)
get ix vec =
  unsafeGet ix (assertIndexInRange ix vec)

-- | Same as 'read', but does not do bounds-checking. The behaviour is undefined
-- when passed an invalid index.
unsafeGet :: (HasCallStack) => Int -> Vector a %1 -> (Ur a, Vector a)
unsafeGet ix (Vec size' arr) =
  Array.unsafeGet ix arr
    & \(val, arr') -> (val, Vec size' arr')

-- | Same as 'modify', but does not do bounds-checking.
unsafeModify ::
  (HasCallStack) =>
  (a -> (a, b)) ->
  Int ->
  Vector a %1 ->
  (Ur b, Vector a)
unsafeModify f ix (Vec size' arr) =
  Array.unsafeGet ix arr & \(Ur old, arr') ->
    case f old of
      (a, b) ->
        Array.unsafeSet ix a arr' & \arr'' ->
          (Ur b, Vec size' arr'')

-- | Modify a value inside a vector, with an ability to return an extra
-- information. Errors if the index is out of bounds.
modify ::
  (HasCallStack) =>
  (a -> (a, b)) ->
  Int ->
  Vector a %1 ->
  (Ur b, Vector a)
modify f ix vec = unsafeModify f ix (assertIndexInRange ix vec)

-- | Same as 'modify', but without the ability to return extra information.
modify_ :: (HasCallStack) => (a -> a) -> Int -> Vector a %1 -> Vector a
modify_ f ix vec =
  modify (\a -> (f a, ())) ix vec
    & \(Ur (), vec') -> vec'

-- | Return the vector elements as a lazy list.
toList :: Vector a %1 -> Ur [a]
toList (Vec s arr) =
  Array.toList arr & \(Ur xs) ->
    Ur (Prelude.take s xs)

-- | Filters the vector in-place. It does not deallocate unused capacity,
-- use 'shrinkToFit' for that if necessary.
filter :: Vector a %1 -> (a -> Bool) -> Vector a
filter v f = mapMaybe v (\a -> if f a then Just a else Nothing)

-- TODO A slightly more efficient version exists, where we skip the writes
-- until the first time the predicate fails. However that requires duplicating
-- most of the logic at `mapMaybe`, so lets not until we have benchmarks to
-- see the advantage.

-- | A version of 'fmap' which can throw out elements.
mapMaybe :: Vector a %1 -> (a -> Maybe b) -> Vector b
mapMaybe vec (f :: a -> Maybe b) =
  size vec & \(Ur s, vec') -> go 0 0 s vec'
  where
    go ::
      Int -> -- read cursor
      Int -> -- write cursor
      Int -> -- input size
      Vector a %1 ->
      Vector b
    go r w s vec'
      -- If we processed all elements, set the capacity after the last written
      -- index and coerce the result to the correct type.
      | r == s =
          vec' & \(Vec _ arr) ->
            Vec w (Unsafe.coerce arr)
      -- Otherwise, read an element, write if the predicate is true and advance
      -- the write cursor; otherwise keep the write cursor skipping the element.
      | otherwise =
          case unsafeGet r vec' of
            (Ur a, vec'')
              | Just b <- f a ->
                  go (r + 1) (w + 1) s (unsafeSet w (Unsafe.coerce b) vec'')
              | otherwise ->
                  go (r + 1) w s vec''

-- | Resize the vector to not have any wasted memory (size == capacity). This
-- returns a semantically identical vector.
shrinkToFit :: Vector a %1 -> Vector a
shrinkToFit vec =
  capacity vec & \(Ur cap, vec') ->
    size vec' & \(Ur s', vec'') ->
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
slice :: Int -> Int -> Vector a %1 -> Vector a
slice from newSize (Vec oldSize arr) =
  if oldSize < from + newSize
    then arr `lseq` error "Slice index out of bounds"
    else
      if from == 0
        then Vec newSize arr
        else
          Array.slice from newSize arr & \(oldArr, newArr) ->
            oldArr `lseq` fromArray newArr

-- | /O(1)/ Convert a 'Vector' to an immutable 'Vector.Vector' (from
-- 'vector' package).
freeze :: Vector a %1 -> Ur (Vector.Vector a)
freeze (Vec sz arr) =
  Array.freeze arr
    & \(Ur vec) -> Ur (Vector.take sz vec)

-- | Same as 'set', but takes the 'Vector' as the first parameter.
write :: (HasCallStack) => Vector a %1 -> Int -> a -> Vector a
write arr i a = set i a arr

-- | Same as 'unsafeSafe', but takes the 'Vector' as the first parameter.
unsafeWrite :: Vector a %1 -> Int -> a -> Vector a
unsafeWrite arr i a = unsafeSet i a arr

-- | Same as 'get', but takes the 'Vector' as the first parameter.
read :: (HasCallStack) => Vector a %1 -> Int -> (Ur a, Vector a)
read arr i = get i arr

-- | Same as 'unsafeGet', but takes the 'Vector' as the first parameter.
unsafeRead :: Vector a %1 -> Int -> (Ur a, Vector a)
unsafeRead arr i = unsafeGet i arr

-- # Instances
-------------------------------------------------------------------------------

instance Consumable (Vector a) where
  consume (Vec _ arr) = consume arr

instance Dupable (Vector a) where
  dup2 (Vec i arr) =
    dup2 arr & \(a1, a2) ->
      (Vec i a1, Vec i a2)

-- There is no way to get an unrestricted vector. So the below instance
-- is just to satisfy the linear Semigroup's constraint.
instance Prelude.Semigroup (Vector a) where
  v1 <> v2 = v1 Data.Monoid.Linear.<> v2

instance Semigroup (Vector a) where
  -- This operation tries to use the existing capacity of v1 when possible.
  v1 <> v2 =
    size v2 & \(Ur s2, v2') ->
      growToFit s2 v1 & \v1' ->
        toList v2' & \(Ur xs) ->
          go xs v1'
    where
      go :: [a] -> Vector a %1 -> Vector a
      go [] vec = vec
      go (x : xs) (Vec sz arr) =
        unsafeSet sz x (Vec (sz + 1) arr)
          & go xs

instance Data.Functor Vector where
  fmap f vec = mapMaybe vec (\a -> Just (f a))

-- # Internal library
-------------------------------------------------------------------------------

-- | Grows the vector to the closest power of growthFactor to
-- fit at least n more elements.
growToFit :: (HasCallStack) => Int -> Vector a %1 -> Vector a
growToFit n vec =
  capacity vec & \(Ur cap, vec') ->
    size vec' & \(Ur s', vec'') ->
      if s' + n <= cap
        then vec''
        else
          let -- Calculate the closest power of growth factor
              -- larger than required size.
              newSize =
                constGrowthFactor -- This constant is defined above.
                  ^ (ceiling :: Double -> Int)
                    ( logBase
                        (fromIntegral constGrowthFactor)
                        (fromIntegral (s' + n)) -- this is always
                        -- > 0 because of
                        -- the if condition
                    )
           in unsafeResize
                newSize
                vec''

-- | Resize the vector to a non-negative size. In-range elements are preserved,
-- the possible new elements are bottoms.
unsafeResize :: (HasCallStack) => Int -> Vector a %1 -> Vector a
unsafeResize newSize (Vec size' ma) =
  Vec
    (min size' newSize)
    ( Array.resize
        newSize
        (error "access to uninitialized vector index")
        ma
    )

-- | Check if given index is within the Vector, otherwise panic.
assertIndexInRange :: (HasCallStack) => Int -> Vector a %1 -> Vector a
assertIndexInRange i vec =
  size vec & \(Ur s, vec') ->
    if 0 <= i && i < s
      then vec'
      else vec' `lseq` error "Vector: index out of bounds"
