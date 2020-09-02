{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTSyntax #-}
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
-- @Array a #-> Unrestricted b@ and feed it to 'alloc' or 'fromList'.
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
-- > isTrue = unUnrestricted $ Array.fromList [0..10] isFirstZero
-- >
-- > isFalse :: Bool
-- > isFalse = unUnrestricted $ Array.fromList [1,2,3] isFirstZero
-- >
-- > isFirstZero :: Array.Array Int #-> Unrestricted Bool
-- > isFirstZero arr = withReadingFirst (Array.read arr 0)
-- >   where
-- >     withReadingFirst :: (Array.Array Int, Int) #-> Unrestricted Bool
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
    empty,
    fromList,
    -- * Mutators
    write,
    unsafeWrite,
    growBy,
    shrinkTo,
    -- * Accessors
    read,
    unsafeRead,
    size,
    toList,
  )
where

import Data.Unrestricted.Linear
import GHC.Exts hiding (toList, fromList)
import GHC.Stack
import qualified Unsafe.Linear as Unsafe
import qualified Unsafe.MutableArray as Unsafe
import Prelude.Linear ((&), forget)
import qualified Control.Monad as Control
import Prelude hiding (size, read)
import qualified Prelude

-- # Data types
-------------------------------------------------------------------------------

data Array a where
  Array :: MutableArray# RealWorld a -> Array a

-- # Creation
-------------------------------------------------------------------------------

-- | Creates an empty array.
empty :: (Array a #-> Unrestricted b) -> Unrestricted b
empty f = f (Array (Unsafe.emptyMutArray ()))

-- | Allocate a constant array given a size and an initial value
-- The size must be non-negative, otherwise this errors.
alloc :: Int -> a -> (Array a #-> Unrestricted b) -> Unrestricted b
alloc size val f
  | size >= 0 = f (Array (Unsafe.newMutArr size val))
  | otherwise = error $ "Trying to allocate an array of size " ++ show size

-- | Allocate a constant array given a size and an initial value,
-- using another array as a uniqueness proof.
allocBeside :: Int -> a -> Array b #-> (Array b, Array a)
allocBeside size val orig
  | size >= 0 = (orig, Array (Unsafe.newMutArr size val))
  | otherwise = orig `lseq` error ("Trying to allocate an array of size " ++ show size)

-- | Allocate an array from a list
fromList :: HasCallStack =>
  [a] -> (Array a #-> Unrestricted b) -> Unrestricted b
fromList [] f = empty f
fromList list@(x:_) (f :: Array a #-> Unrestricted b) =
  alloc (Prelude.length list) x insertThenf
  where
    insertThenf :: Array a #-> Unrestricted b
    insertThenf arr = f (doWrites (zip list [0..]) arr)

    doWrites :: [(a,Int)] -> Array a #-> Array a
    doWrites [] arr = arr
    doWrites ((a,ix):xs) arr = doWrites xs (write ix a arr)

-- # Mutations and Reads
-------------------------------------------------------------------------------

size :: Array a #-> (Array a, Unrestricted Int)
size = Unsafe.toLinear unsafeLength
  where
    unsafeLength :: Array a -> (Array a, (Unrestricted Int))
    unsafeLength v@(Array a) = (v, Unrestricted (Unsafe.sizeMutArr a))

-- | Indexes an array. Fails if the index is out-of-bounds.
read :: HasCallStack => Int -> Array a #-> (Array a, Unrestricted a)
read ix arr =
  assertIndexInRange ix arr & unsafeRead ix

-- | Writes to the given array index. Fails if the index is out-of-bounds.
write :: HasCallStack => Int -> a -> Array a #-> Array a
write ix val arr =
  assertIndexInRange ix arr & unsafeWrite ix val

-- | Same as 'read', but does not do bounds-checking.
unsafeRead :: Int -> Array a #-> (Array a, Unrestricted a)
unsafeRead ix (Array mut) =
  let !(# a #) = Unsafe.readMutArr mut ix
  in  (Array mut, Unrestricted a)

-- | Same as 'write', but does not do bounds-checking.
unsafeWrite :: Int -> a -> Array a #-> Array a
unsafeWrite ix val (Array mut) =
  case Unsafe.writeMutArr mut ix val of
    () -> Array mut

-- XXX: Replace with toVec
-- | Reads all elements of an array.
toList :: Array a #-> (Array a, Unrestricted [a])
toList arr = size arr & \case
  (arr', Unrestricted len) ->
    toListWalk (len - 1) [] arr'
  where
  toListWalk :: Int -> [a] -> Array a #-> (Array a, Unrestricted [a])
  toListWalk ix accum arr
    | ix < 0 = (arr, Unrestricted accum)
    | otherwise = read ix arr & \case
        (arr', Unrestricted x) ->
          toListWalk (ix - 1) (x:accum) arr'

-- # Resizing
-------------------------------------------------------------------------------

-- | Grows the array by given number of elements, using a default value.
growBy :: HasCallStack => Int -> a -> Array a #-> Array a
growBy count _ arr | count < 0 =
  arr `lseq` error "growBy: negative input"
growBy count def (Array arr) =
  Array (Unsafe.growByMutArr arr count def)

-- | Shrinks the array to contain given number of elements.
shrinkTo :: HasCallStack => Int -> Array a #-> Array a
shrinkTo length arr | length < 0 =
  arr `lseq` error "shrinkTo: negative input"
shrinkTo length arr =
  size arr & \(Array mut, Unrestricted s) ->
    if length >= s
    then error "shrinkTo: array is already smaller"
    else Array (Unsafe.shrinkToMutArr mut length)

-- # Instances
-------------------------------------------------------------------------------

instance Show a => Show (Array a) where
  show = show . forget unUnrestricted . snd . (\x -> toList x)

instance Consumable (Array a) where
  consume :: Array a #-> ()
  consume (Array _) = ()

-- # Internal library
-------------------------------------------------------------------------------
type Size = Int

assertIndexInRange :: Int -> Array a #-> Array a
assertIndexInRange ix arr =
  size arr & \(arr', Unrestricted len) ->
    if 0 <= ix && ix < len
    then arr'
    else arr' `lseq` error ("index out of bounds:" ++ show len)
