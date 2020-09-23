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
-- @Array a #-> Ur b@ and feed it to 'alloc' or 'fromList'.
--
-- == A Tiny Example
--
-- > {-# LANGUAGE LinearTypes #-}
-- > import Prelude.Linear
-- > import Data.Unrestricted.Linear
-- > import qualified Unsafe.Linear as Unsafe
-- > import qualified Data.Array.Mutable.Linear as Array
-- >
-- > isTrue :: Bool
-- > isTrue = unur $ Array.fromList [0..10] isFirstZero
-- >
-- > isFalse :: Bool
-- > isFalse = unur $ Array.fromList [1,2,3] isFirstZero
-- >
-- > isFirstZero :: Array.Array Int #-> Ur Bool
-- > isFirstZero arr = withReadingFirst (Array.read arr 0)
-- >   where
-- >     withReadingFirst :: (Array.Array Int, Int) #-> Ur Bool
-- >     withReadingFirst (arr, i) = lseq arr $ move (i === 0)
-- >
-- > (===) :: (Num a, Eq a) => a #-> a #-> Bool
-- > (===) = Unsafe.toLinear2 (==)
module Data.Array.Mutable.Linear
  ( -- * Mutable Linear Arrays
    Array,
    -- * Performing Computations with Arrays
    alloc,
    allocBeside,
    fromList,
    -- * Mutators
    write,
    unsafeWrite,
    resize,
    -- * Accessors
    read,
    unsafeRead,
    size,
    slice,
    toList,
  )
where

import Data.Unrestricted.Linear
import GHC.Stack
import Data.Array.Mutable.Unlifted.Linear (Array#)
import qualified Data.Array.Mutable.Unlifted.Linear as Unlifted
import qualified Data.Functor.Linear as Data
import Prelude.Linear ((&))
import Prelude hiding (read)

-- # Data types
-------------------------------------------------------------------------------

data Array a = Array (Array# a)

-- # Creation
-------------------------------------------------------------------------------

-- | Allocate a constant array given a size and an initial value
-- The size must be non-negative, otherwise this errors.
alloc :: HasCallStack =>
  Int -> a -> (Array a #-> Ur b) #-> Ur b
alloc s x f
  | s < 0 =
    (error ("Array.alloc: negative size: " ++ show s) :: x #-> x)
    (f undefined)
  | otherwise = Unlifted.alloc s x (\arr -> f (Array arr))

-- | Allocate a constant array given a size and an initial value,
-- using another array as a uniqueness proof.
allocBeside :: Int -> a -> Array b #-> (Array b, Array a)
allocBeside s x (Array orig)
  | s < 0 =
     Unlifted.lseq
       orig
       (error ("Array.allocBeside: negative size: " ++ show s))
  | otherwise =
      wrap (Unlifted.allocBeside s x orig)
     where
      wrap :: (# Array# b, Array# a #) #-> (Array b, Array a)
      wrap (# orig, new #) = (Array orig, Array new)

-- | Allocate an array from a list
fromList :: HasCallStack =>
  [a] -> (Array a #-> Ur b) #-> Ur b
fromList list (f :: Array a #-> Ur b) =
  alloc
    (Prelude.length list)
    (error "invariant violation: unintialized array position")
    (\arr -> f (insert arr))
 where
  insert :: Array a #-> Array a
  insert = doWrites (zip list [0..])

  doWrites :: [(a,Int)] -> Array a #-> Array a
  doWrites [] arr = arr
  doWrites ((a,ix):xs) arr = doWrites xs (unsafeWrite arr ix a)

-- # Mutations and Reads
-------------------------------------------------------------------------------

size :: Array a #-> (Array a, Ur Int)
size (Array arr) = f (Unlifted.size arr)
 where
  f :: (# Array# a, Ur Int #) #-> (Array a, Ur Int)
  f (# arr, s #) = (Array arr, s)

-- | Writes a value to an index of an Array. The index should be less than the
-- arrays size, otherwise this errors.
write :: HasCallStack => Array a #-> Int -> a -> Array a
write arr i x = unsafeWrite (assertIndexInRange i arr) i x

-- | Same as 'write', but does not do bounds-checking. The behaviour is undefined
-- if an out-of-bounds index is provided.
unsafeWrite :: Array a #-> Int -> a -> Array a
unsafeWrite (Array arr) ix val =
  Array (Unlifted.write ix val arr)

-- | Read an index from an Array. The index should be less than the arrays size,
-- otherwise this errors.
read :: HasCallStack => Array a #-> Int -> (Array a, Ur a)
read arr i = unsafeRead (assertIndexInRange i arr) i

-- | Same as read, but does not do bounds-checking. The behaviour is undefined
-- if an out-of-bounds index is provided.
unsafeRead :: Array a #-> Int -> (Array a, Ur a)
unsafeRead (Array arr) ix = wrap (Unlifted.read ix arr)
 where
  wrap :: (# Array# a, Ur a #) #-> (Array a, Ur a)
  wrap (# arr, ret #) = (Array arr, ret)

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
resize :: HasCallStack => Int -> a -> Array a #-> Array a
resize newSize seed (Array arr :: Array a)
  | newSize < 0 =
      Unlifted.lseq
        arr
        (error "Trying to resize to a negative size.")
  | otherwise =
      doCopy (Unlifted.allocBeside newSize seed arr)
     where
      doCopy :: (# Array# a, Array# a #) #-> Array a
      doCopy (# src, dest #) = wrap (Unlifted.copyInto 0 src dest)

      wrap :: (# Array# a, Array# a #) #-> Array a
      wrap (# old, new #) = old `Unlifted.lseq` Array new


-- | Return the array elements as a lazy list.
toList :: Array a #-> Ur [a]
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
  -> Array a #-> (Array a, Array a)
slice from targetSize arr =
  size arr & \case
    (Array old, Ur s)
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
    doCopy :: (# Array# a, Array# a #) #-> (Array a, Array a)
    doCopy (# old, new #) = wrap (Unlifted.copyInto from old new)

    wrap :: (# Array# a, Array# a  #) #-> (Array a, Array a)
    wrap (# old, new #) = (Array old, Array new)

-- # Instances
-------------------------------------------------------------------------------

instance Consumable (Array a) where
  consume :: Array a #-> ()
  consume (Array arr) = arr `Unlifted.lseq` ()

instance Dupable (Array a) where
  dup2 :: Array a #-> (Array a, Array a)
  dup2 (Array arr) = wrap (Unlifted.dup2 arr)
   where
     wrap :: (# Array# a, Array# a #) #-> (Array a, Array a)
     wrap (# a1, a2 #) = (Array a1, Array a2)

instance Data.Functor Array where
  fmap f (Array arr) = Array (Unlifted.map f arr)

-- # Internal library
-------------------------------------------------------------------------------

-- | Check if given index is within the Array, otherwise panic.
assertIndexInRange :: HasCallStack => Int -> Array a #-> Array a
assertIndexInRange i arr =
  size arr & \(arr', Ur s) ->
    if 0 <= i && i < s
    then arr'
    else arr' `lseq` error "Array: index out of bounds"
