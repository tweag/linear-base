{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
    unsafeWrite,
    snoc,
    -- * Accessors
    read,
    unsafeRead,
    length,
  )
where

import GHC.Exts hiding (fromList)
import GHC.Stack
import Prelude.Linear hiding (length, read)
import qualified Unsafe.Linear as Unsafe
import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
import qualified Prelude

-- # Core data types
-------------------------------------------------------------------------------

-- | A dynamic mutable vector.
data Vector a where
  Vec :: Int -- ^ Current length
      -> Array a -- ^ Underlying array
      #-> Vector a

-- # API: Construction, Mutation, Queries
-------------------------------------------------------------------------------

empty :: (Vector a #-> Unrestricted b) -> Unrestricted b
empty f = Array.empty (\arr -> f (Vec 0 arr))

-- | Allocate a constant vector of a given non-negative size (and error on a bad
-- size)
constant :: Int -> a -> (Vector a #-> Unrestricted b) -> Unrestricted b
constant size x f
  | size < 0 = error ("Trying to construct a vector of size " ++ show size)
  | otherwise = fromList (replicate size x) f

-- | Creates a vector from a list.
fromList :: HasCallStack => [a] -> (Vector a #-> Unrestricted b) -> Unrestricted b
fromList xs f =
  Array.fromList xs Prelude.$ \arr ->
    Array.size arr & \(arr', Unrestricted i) ->
      f (Vec i arr')

-- | Length of the vector
length :: Vector a #-> (Vector a, Unrestricted Int)
length (Vec len arr) =
  (Vec len arr, Unrestricted len)

-- | Capacity of the vector
capacity :: Vector a #-> (Vector a, Unrestricted Int)
capacity (Vec len arr) =
  Array.size arr & \(arr', cap) ->
    (Vec len arr', cap)

-- | Insert at the end of the vector
snoc :: HasCallStack => a -> Vector a #-> Vector a
snoc val vec =
  length vec
    & \(vec, Unrestricted len) -> ensureCapacity (len+1) vec
    & \(Vec len arr) -> Vec (len+1) arr
    & write len val

-- | Ensure that the vector has capacity for at least given number of items.
-- Grows the underlying array if necessary.
ensureCapacity :: Int -> Vector a #-> Vector a
ensureCapacity target vec =
  capacity vec & \(Vec len arr, Unrestricted cap) ->
    Vec
      len
      (if target <= cap
       then arr
       else Array.growBy cap (error "index out of bounds") arr
      )

-- | Indexes an vector. Fails if the index is out-of-bounds.
read :: HasCallStack => Int -> Vector a #-> (Vector a, Unrestricted a)
read ix vec =
  assertIndexInRange ix vec
    & unsafeRead ix

-- | Same as 'read', but does not do bounds-checking.
unsafeRead :: Int -> Vector a #-> (Vector a, Unrestricted a)
unsafeRead ix (Vec len arr) =
  Array.unsafeRead ix arr & \(arr', val) ->
    (Vec len arr', val)

-- | Writes to the given array index. Fails if the index is out-of-bounds.
write :: Int -> a -> Vector a #-> Vector a
write ix val vec =
  assertIndexInRange ix vec
    & unsafeWrite ix val

-- | Same as 'write', but does not do bounds-checking.
unsafeWrite :: HasCallStack => Int -> a -> Vector a #-> Vector a
unsafeWrite ix val (Vec len arr) =
  Vec len (Array.unsafeWrite ix val arr)

-- # Instances
-------------------------------------------------------------------------------

instance Consumable (Vector a) where
  consume (Vec _ arr) = consume arr

-- # Internal library
-------------------------------------------------------------------------------

assertIndexInRange :: Int -> Vector a #-> Vector a
assertIndexInRange ix vec =
  length vec & \(vec', Unrestricted len) ->
    if 0 <= ix && ix < len
    then vec'
    else vec' `lseq` error ("index out of bounds:" ++ show len)
