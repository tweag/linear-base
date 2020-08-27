{-# OPTIONS_GHC -Wno-unbanged-strict-patterns #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- This module defines convenient and unsafe wrappers around the MutableArray#
-- API in GHC.Exts.
--
-- Please import this module qualified as Unsafe to signal that all
-- functions from this module are unsafe and crash on inputs that
-- fail their respective predcondition.
module Unsafe.MutableArray
  ( -- * Unsafe Wrappers around @MutableArray#@
    MutArr#,
    -- * Mutators and Constructors
    newMutArr,
    writeMutArr,
    resizeMutArr,
    resizeMutArrSeed,
    copyIntoMutArr,
    -- * Accessors
    readMutArr,
    sizeMutArr
  )
where

import GHC.Exts

-- # Unsafe wrappers
----------------------------

-- | A mutable array holding @a@s
type MutArr# a = MutableArray# RealWorld a

-- | Get the size of a mutable array
sizeMutArr :: MutArr# a -> Int
sizeMutArr arr  = I# (sizeofMutableArray# arr)

-- | Given a size, try to allocate a mutable array
-- of this size. The size should be greater than zero.
newMutArr :: Int -> a -> MutArr# a
newMutArr (I# size) x =
  case newArray of
    (# _, array #) -> array
  where
    newArray = runRW# $ \stateRW -> newArray# size x stateRW

-- | Write to a given mutable array arr, given an
-- index in [0, size(arr)-1] , and a value
writeMutArr :: MutArr# a -> Int -> a -> ()
writeMutArr mutArr (I# ix) val =
  case doWrite of _ -> ()
  where
    doWrite = runRW# $ \stateRW -> writeArray# mutArr ix val stateRW

-- | Read from a given mutable array arr, given an index
-- in [0, size(arr)-1]
--
-- This returns an unboxed tuple to give the callee the
-- ability to evaluate the function call without evaluating
-- the final result.
readMutArr :: MutArr# a -> Int -> (# a #)
readMutArr mutArr (I# ix) =
  case doRead of (# _, a #) -> (# a #)
  where
    doRead = runRW# $ \stateRW -> readArray# mutArr ix stateRW

-- | Grow a mutable array. This is given an array, a given larger size,
-- and it returns a larger array of the given size using the first value
-- to fill in the new cells and copying over all the unchanged cells. This
-- fails for a size smaller than the size of the given array.
resizeMutArr :: MutArr# a -> Int -> MutArr# a
resizeMutArr mutArr newSize =
  let (# a #) = readMutArr mutArr 0
  in  resizeMutArrSeed mutArr a newSize

-- | Grow a mutable array. This is given an array, a given larger size,
-- and it returns a larger array of the given size using the seed value
-- to fill in the new cells and copying over all the unchanged cells. This
-- fails for a size smaller than the size of the given array.
resizeMutArrSeed :: MutArr# a -> a -> Int -> MutArr# a
resizeMutArrSeed mutArr x newSize = case newMutArr newSize x of
  newArr -> case copyIntoMutArr mutArr newArr of
    () -> newArr

-- | Copy the first mutable array into the second, larger mutable array.
-- This fails if the second array is smaller than the first.
copyIntoMutArr :: MutArr# a -> MutArr# a -> ()
copyIntoMutArr src dest = case doCopy of _ -> ()
  where
    doCopy = runRW# $ \stateRW -> copyMutableArray# src z dest z src_len stateRW
    (I# z) = 0 :: Int
    src_len = sizeofMutableArray# src
