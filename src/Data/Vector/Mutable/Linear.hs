{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unbanged-strict-patterns #-}

-- | Mutable vectors with a linear API.
--
-- Vectors are arrays that grow and shrink automatically with 'push' and
-- 'pop' methods.
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
    fromArray,
    -- * Mutators
    write,
    unsafeWrite,
    modify,
    modify_,
    push,
    pop,
    shrinkToFit,
    -- * Accessors
    read,
    unsafeRead,
    size,
    toList,
    toListLazy
  )
where

import GHC.Exts hiding (fromList, toList)
import GHC.Stack
import qualified Prelude as Prelude
import Data.Monoid.Linear
import Prelude.Linear hiding (read)
import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array

-- # Constants
-------------------------------------------------------------------------------

-- | The vector will shrink when size / capacity is below this threshold.
--
-- This value should be smaller than 1/growthFactor to create a stable area
-- avoiding frequent resizes.
constShrinkThreshold :: Float
constShrinkThreshold = 0.3

-- | When growing or shrinking, capacity will be multiplied or divided
-- by this number.
--
-- This is usually chosen between 1.5 and 2; 2 being the most common.
constGrowthFactor :: Int
constGrowthFactor = 2

-- # Core data types
-------------------------------------------------------------------------------

-- | A dynamic mutable vector.
--
-- Should contain at most @maxBound :: Int@ values. This invariant is not
-- checked.
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
    & \(arr',  Unrestricted s) -> Vec s arr'

-- | Run a computation with an empty vector.
empty :: (Vector a #-> Unrestricted b) #-> Unrestricted b
empty f = Array.fromList [] (f . fromArray)

-- | Run a computation with a constant vector with given non-negative size.
constant :: HasCallStack =>
  Int -> a -> (Vector a #-> Unrestricted b) #-> Unrestricted b
constant size' x f
  | size' < 0 =
      (error ("Trying to construct a vector of size " ++ show size') :: x #-> x)
      (f undefined)
  | otherwise = Array.alloc size' x (f . fromArray)

-- | Run a computation with a vector with elements from the given list.
fromList :: [a] -> (Vector a #-> Unrestricted b) #-> Unrestricted b
fromList xs f = Array.fromList xs (f . fromArray)

-- | Number of elements inside the vector.
size :: Vector a #-> (Vector a, Unrestricted Int)
size (Vec size' arr) = (Vec size' arr, Unrestricted size')

-- | Capacity of a vector. In other words, the number of elements
-- the vector can contain before it is copied to a bigger array.
capacity :: Vector a #-> (Vector a, Unrestricted Int)
capacity (Vec s arr) =
  Array.size arr & \(arr', cap) -> (Vec s arr', cap)

-- | Insert at the end of the vector. This will grow the vector if there
-- is no empty space.
push :: Vector a #-> a -> Vector a
push vec x =
  growToFit 1 vec & \(Vec s arr) ->
    write (Vec (s + 1) arr) s x

-- | Pop from the end of the vector. This will shrink the vector if there
-- is too much wasted space.
pop :: Vector a #-> (Vector a, Unrestricted (Maybe a))
pop vec =
  size vec & \case
    (vec', Unrestricted 0) ->
      (vec', Unrestricted Nothing)
    (vec', Unrestricted s) ->
      read vec' (s-1) & \(Vec _ arr, Unrestricted a) ->
        ( shrinkIfNecessary (Vec (s-1) arr)
        , Unrestricted (Just a)
        )

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
read :: HasCallStack => Vector a #-> Int -> (Vector a, Unrestricted a)
read (Vec size' arr) ix
  | indexInRange size' ix =
      Array.unsafeRead arr ix
        & \(arr', val) -> (Vec size' arr', val)
  | otherwise = arr `lseq` error "Read index not in range."

-- | Same as 'read', but does not do bounds-checking. The behaviour is undefined
-- when passed an invalid index.
unsafeRead :: HasCallStack => Vector a #-> Int -> (Vector a, Unrestricted a)
unsafeRead (Vec size' arr) ix =
  Array.unsafeRead arr ix
    & \(arr', val) -> (Vec size' arr', val)

-- | Modify a value inside a vector, with an ability to return an extra
-- information. Errors if the index is out of bounds.
modify :: HasCallStack => Vector a #-> Int -> (a -> (a, b))
       -> (Vector a, Unrestricted b)
modify (Vec size' arr) ix f
  | indexInRange size' ix =
      Array.unsafeRead arr ix & \(arr', Unrestricted old) ->
        case f old of
          (a, b) -> Array.unsafeWrite arr' ix a & \arr'' ->
            (Vec size' arr'', Unrestricted b)

  | otherwise = arr `lseq` error "Modify index not in range."

-- | Same as 'modify', but without the ability to return extra information.
modify_ :: HasCallStack => Vector a #-> Int -> (a -> a) -> Vector a
modify_ vec ix f =
  modify vec ix (\a -> (f a, ()))
    & \(vec', Unrestricted ()) -> vec'

-- | Resize the vector to not have any wasted memory (size == capacity). This
-- returns a semantically identical vector.
shrinkToFit :: Vector a #-> Vector a
shrinkToFit vec =
  capacity vec & \(vec', Unrestricted cap) ->
    size vec' & \(vec'', Unrestricted s') ->
      if cap > s'
      then unsafeResize s' vec''
      else vec''

-- | Return the vector elements as a spine-strict list. This
-- function is expensive since it has to traverse the entire
-- vector before returning.
toList :: Vector a #-> (Vector a, Unrestricted [a])
toList v =
  size v & \(vec, Unrestricted s) ->
    go (s - 1) [] vec
 where
  go :: Int -> [a] -> Vector a #-> (Vector a, Unrestricted [a])
  go i acc vec
    | i < 0 = (vec, Unrestricted acc)
    | otherwise =
        unsafeRead vec i & \(vec', Unrestricted x) ->
          go (i - 1) (x:acc) vec'

-- See the note at Array.toListLazy

-- | Return the vector elements as a lazy list. The vector
-- lookups will happen lazily, so this function also consumes
-- the vector.
toListLazy :: Vector a #-> Unrestricted [a]
toListLazy (Vec s arr) =
  Array.toListLazy arr & \(Unrestricted xs) ->
    Unrestricted (take s xs)

-- # Instances
-------------------------------------------------------------------------------

instance Consumable (Vector a) where
  consume (Vec _ arr) = consume arr

-- There is no way to get a non-linear vector. So the below instance is just to
-- satisfy the linear Semigroup's constraint.
instance Prelude.Semigroup (Vector a) where
  v1 <> v2 = v1 Data.Monoid.Linear.<> v2


instance Semigroup (Vector a) where
  -- This operation tries to use the existing capacity of v1 when possible.
  v1 <> v2 =
    size v2 & \(v2', Unrestricted s2) ->
      growToFit s2 v1 & \v1' ->
        toListLazy v2' & \(Unrestricted xs) ->
          go xs v1'
   where
     go :: [a] -> Vector a #-> Vector a
     go [] vec = vec
     go (x:xs) (Vec cap arr) =
       unsafeWrite (Vec (cap+1) arr) cap x
         & go xs

-- # Internal library
-------------------------------------------------------------------------------

-- | Grows the vector to the closest power of growthFactor to
-- fit at least n more elements.
growToFit :: HasCallStack => Int -> Vector a #-> Vector a
growToFit n vec =
  capacity vec & \(vec', Unrestricted cap) ->
    size vec' & \(vec'', Unrestricted s') ->
      if s' + n <= cap
      then vec''
      else
        let -- Calculate the closest power of growth factor
            -- larger than required size.
            newSize =
              constGrowthFactor
                ^ (ceiling :: Double -> Int)
                    (logBase
                      (fromIntegral constGrowthFactor)
                      (fromIntegral (s' + n))) -- this is always
                                               -- > 0 because of
                                               -- the if condition
        in  unsafeResize
              newSize
              vec''

-- | Shrinks the vector by 1/growthFactor when the shrink threshold
-- is met.
shrinkIfNecessary :: HasCallStack => Vector a #-> Vector a
shrinkIfNecessary vec =
  capacity vec & \(vec', Unrestricted cap) ->
    size vec' & \(vec'', Unrestricted s') ->
      let cap' = fromIntegral cap
      in  if fromIntegral s' > cap' Prelude.* constShrinkThreshold
          then vec''
          else
            unsafeResize
              (truncate (cap' / fromIntegral constGrowthFactor))
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

-- | Argument order: indexInRange size ix
indexInRange :: Int -> Int -> Bool
indexInRange size' ix = 0 <= ix && ix < size'
