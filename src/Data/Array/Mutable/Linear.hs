{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- |
-- This module provides a pure linear interface for arrays with in-place
-- mutation.
--
-- To use these mutable arrays, create a linear computation of type
-- @Array a %1-> Ur b@ and feed it to 'alloc' or 'fromList'.
--
-- == A Tiny Example
--
-- >>> :set -XLinearTypes
-- >>> :set -XNoImplicitPrelude
-- >>> import Prelude.Linear
-- >>> import qualified Data.Array.Mutable.Linear as Array
-- >>> :{
--  isFirstZero :: Array.Array Int %1-> Ur Bool
--  isFirstZero arr =
--    Array.get 0 arr
--      & \(Ur val, arr') -> arr' `lseq` Ur (val == 0)
-- :}
--
-- >>> unur $ Array.fromList [0..10] isFirstZero
-- True
-- >>> unur $ Array.fromList [1,2,3] isFirstZero
-- False
module Data.Array.Mutable.Linear
  ( -- * Mutable Linear Arrays
    Array,
    -- * Performing Computations with Arrays
    alloc,
    allocBeside,
    fromList,
    -- * Modifications
    set,
    unsafeSet,
    resize,
    map,
    -- * Accessors
    get,
    unsafeGet,
    size,
    slice,
    toList,
    freeze,
    -- * Mutable-style interface
    read,
    unsafeRead,
    write,
    unsafeWrite
  )
where

import Data.Unrestricted.Linear
import GHC.Stack
import Data.Array.Mutable.Unlifted.Linear (Array#)
import qualified Data.Array.Mutable.Unlifted.Linear as Unlifted
import qualified Data.Functor.Linear as Data
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Prelude.Linear ((&), forget)
import qualified Data.Primitive.Array as Prim
import System.IO.Unsafe (unsafeDupablePerformIO)
import Prelude hiding (read, map)

-- # Data types
-------------------------------------------------------------------------------

data Array a = Array (Array# a)

-- # Creation
-------------------------------------------------------------------------------

-- | Allocate a constant array given a size and an initial value
-- The size must be non-negative, otherwise this errors.
alloc :: HasCallStack =>
  Int -> a -> (Array a %1-> Ur b) %1-> Ur b
alloc s x f
  | s < 0 =
    (error ("Array.alloc: negative size: " ++ show s) :: x %1-> x)
    (f undefined)
  | otherwise = Unlifted.alloc s x (\arr -> f (Array arr))

-- | Allocate a constant array given a size and an initial value,
-- using another array as a uniqueness proof.
allocBeside :: Int -> a -> Array b %1-> (Array a, Array b)
allocBeside s x (Array orig)
  | s < 0 =
     Unlifted.lseq
       orig
       (error ("Array.allocBeside: negative size: " ++ show s))
  | otherwise =
      wrap (Unlifted.allocBeside s x orig)
     where
      wrap :: (# Array# a, Array# b #) %1-> (Array a, Array b)
      wrap (# orig, new #) = (Array orig, Array new)

-- | Allocate an array from a list
fromList :: HasCallStack =>
  [a] -> (Array a %1-> Ur b) %1-> Ur b
fromList list (f :: Array a %1-> Ur b) =
  alloc
    (Prelude.length list)
    (error "invariant violation: unintialized array position")
    (\arr -> f (insert arr))
 where
  insert :: Array a %1-> Array a
  insert = doWrites (zip list [0..])

  doWrites :: [(a,Int)] -> Array a %1-> Array a
  doWrites [] arr = arr
  doWrites ((a,ix):xs) arr = doWrites xs (unsafeSet ix a arr)

-- # Mutations and Reads
-------------------------------------------------------------------------------

size :: Array a %1-> (Ur Int, Array a)
size (Array arr) = f (Unlifted.size arr)
 where
  f :: (# Ur Int, Array# a #) %1-> (Ur Int, Array a)
  f (# s, arr #) = (s, Array arr)

-- | Sets the value of an index. The index should be less than the arrays
-- size, otherwise this errors.
set :: HasCallStack => Int -> a -> Array a %1-> Array a
set i x arr = unsafeSet i x (assertIndexInRange i arr)

-- | Same as 'set, but does not do bounds-checking. The behaviour is undefined
-- if an out-of-bounds index is provided.
unsafeSet :: Int -> a -> Array a %1-> Array a
unsafeSet ix val (Array arr) =
  Array (Unlifted.set ix val arr)

-- | Get the value of an index. The index should be less than the arrays 'size',
-- otherwise this errors.
get :: HasCallStack => Int -> Array a %1-> (Ur a, Array a)
get i arr = unsafeGet i (assertIndexInRange i arr)

-- | Same as 'get', but does not do bounds-checking. The behaviour is undefined
-- if an out-of-bounds index is provided.
unsafeGet :: Int -> Array a %1-> (Ur a, Array a)
unsafeGet ix (Array arr) = wrap (Unlifted.get ix arr)
 where
  wrap :: (# Ur a, Array# a #) %1-> (Ur a, Array a)
  wrap (# ret, arr #) = (ret, Array arr)

-- | Resize an array. That is, given an array, a target size, and a seed
-- value; resize the array to the given size using the seed value to fill
-- in the new cells when necessary and copying over all the unchanged cells.
--
-- Target size should be non-negative.
--
-- @
-- let b = resize n x a,
--   then size b = n,
--   and b[i] = a[i] for i < size a,
--   and b[i] = x for size a <= i < n.
-- @
resize :: HasCallStack => Int -> a -> Array a %1-> Array a
resize newSize seed (Array arr :: Array a)
  | newSize < 0 =
      Unlifted.lseq
        arr
        (error "Trying to resize to a negative size.")
  | otherwise =
      doCopy (Unlifted.allocBeside newSize seed arr)
     where
      doCopy :: (# Array# a, Array# a #) %1-> Array a
      doCopy (# new, old #) = wrap (Unlifted.copyInto 0 old new)

      wrap :: (# Array# a, Array# a #) %1-> Array a
      wrap (# src, dst #) = src `Unlifted.lseq` Array dst


-- | Return the array elements as a lazy list.
toList :: Array a %1-> Ur [a]
toList (Array arr) = Unlifted.toList arr

-- | Copy a slice of the array, starting from given offset and copying given
-- number of elements. Returns the pair (oldArray, slice).
--
-- Start offset + target size should be within the input array, and both should
-- be non-negative.
--
-- @
-- let b = slice i n a,
--   then size b = n,
--   and b[j] = a[i+j] for 0 <= j < n
-- @
slice
  :: HasCallStack
  => Int -- ^ Start offset
  -> Int -- ^ Target size
  -> Array a %1-> (Array a, Array a)
slice from targetSize arr =
  size arr & \case
    (Ur s, Array old)
      | s < from + targetSize ->
          Unlifted.lseq
            old
            (error "Slice index out of bounds.")
      | otherwise ->
          doCopy
            (Unlifted.allocBeside
               targetSize
               (error "invariant violation: uninitialized array index")
               old)
  where
    doCopy :: (# Array# a, Array# a #) %1-> (Array a, Array a)
    doCopy (# new, old #) = wrap (Unlifted.copyInto from old new)

    wrap :: (# Array# a, Array# a  #) %1-> (Array a, Array a)
    wrap (# old, new #) = (Array old, Array new)

-- | /O(1)/ Convert an 'Array' to an immutable 'Vector.Vector' (from
-- 'vector' package).
freeze :: Array a %1-> Ur (Vector.Vector a)
freeze (Array arr) =
  Unlifted.freeze go arr
 where
   go arr = unsafeDupablePerformIO $ do
     mut <- Prim.unsafeThawArray (Prim.Array arr)
     let mv = MVector.MVector 0 (Prim.sizeofMutableArray mut) mut
     Vector.unsafeFreeze mv
   -- We only need to do above because 'Vector' constructor is hidden.
   -- Once it is exposed, we should be able to replace it with something
   -- safer like: `go arr = Vector 0 (sizeof arr) arr`

map :: (a -> b) -> Array a %1-> Array b
map f (Array arr) = Array (Unlifted.map f arr)

-- # Mutation-style API
-------------------------------------------------------------------------------

-- | Same as 'set', but takes the 'Array' as the first parameter.
write :: HasCallStack => Array a %1-> Int -> a -> Array a
write arr i a = set i a arr

-- | Same as 'unsafeSafe', but takes the 'Array' as the first parameter.
unsafeWrite ::  Array a %1-> Int -> a -> Array a
unsafeWrite arr i a = unsafeSet i a arr

-- | Same as 'get', but takes the 'Array' as the first parameter.
read :: HasCallStack => Array a %1-> Int -> (Ur a, Array a)
read arr i = get i arr

-- | Same as 'unsafeGet', but takes the 'Array' as the first parameter.
unsafeRead :: Array a %1-> Int -> (Ur a, Array a)
unsafeRead arr i = unsafeGet i arr

-- # Instances
-------------------------------------------------------------------------------

instance Consumable (Array a) where
  consume :: Array a %1-> ()
  consume (Array arr) = arr `Unlifted.lseq` ()

instance Dupable (Array a) where
  dup2 :: Array a %1-> (Array a, Array a)
  dup2 (Array arr) = wrap (Unlifted.dup2 arr)
   where
     wrap :: (# Array# a, Array# a #) %1-> (Array a, Array a)
     wrap (# a1, a2 #) = (Array a1, Array a2)

instance Data.Functor Array where
  fmap f arr = map (forget f) arr

-- # Internal library
-------------------------------------------------------------------------------

-- | Check if given index is within the Array, otherwise panic.
assertIndexInRange :: HasCallStack => Int -> Array a %1-> Array a
assertIndexInRange i arr =
  size arr & \(Ur s, arr') ->
    if 0 <= i && i < s
    then arr'
    else arr' `lseq` error "Array: index out of bounds"
