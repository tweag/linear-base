{-# OPTIONS_GHC -Wno-unbanged-strict-patterns #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

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
    newMutArr,
    emptyMutArray,
    -- * Basic operations
    readMutArr,
    writeMutArr,
    sizeMutArr,
    -- * Resizing an array
    growByMutArr,
    shrinkToMutArr,
    sliceMutArr
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
-- of this size. The size should be non-negative.
newMutArr :: Int -> a -> MutArr# a
newMutArr (I# size) x =
  case newArray of
    (# _, array #) -> array
  where
    newArray = runRW# $ \stateRW -> newArray# size x stateRW

emptyMutArray :: () -> MutArr# a
emptyMutArray () = newMutArr 0 undefined
{-# NOINLINE emptyMutArray #-}

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

-- | Return a shrunk mutable array to given size.
--
-- Precondition: Given length should be less than or equal to the original
-- arrays length.
--
-- @
--   shrinkMutArr arr len = sliceMutArr arr 0 len
-- @
shrinkToMutArr :: MutArr# a -> Int -> MutArr# a
shrinkToMutArr arr len = sliceMutArr arr 0 len

-- | Given an array, a starting index and a number of elements to copy,
-- return a new array containing the elements from the original.
--
-- Preconditions:
--   * start >= 0, count >= 0
--   * start + count <= original_length
sliceMutArr
  :: MutArr# a
  -> Int -- ^ First index to include
  -> Int -- ^ Number of elements to copy
  -> MutArr# a
sliceMutArr src (I# start#) (I# count#) =
  let I# z# = 0
      res = runRW# $ \s ->
        let (# s', newArr #) = newArray# count# undefined s
            !_ = copyMutableArray# src start# newArr z# count# s'
         in newArr
   in res

-- | Grow the array by given number of elements using a default value.
--
-- Precondition: Number should be positive.
growByMutArr
  :: MutArr# a
  -> Int -- ^ Number of elements to grow
  -> a   -- ^ Default value
  -> MutArr# a
growByMutArr src count def =
  let I# z# = 0
      srcSize# = sizeofMutableArray# src
      I# targetSize# = I# srcSize# + count
      res = runRW# $ \s ->
        let (# s', newArr #) = newArray# targetSize# def s
            !_ = copyMutableArray# src z# newArr z# srcSize# s'
         in newArr
   in res

