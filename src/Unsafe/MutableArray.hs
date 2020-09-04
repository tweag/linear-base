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
    -- * Mutators and Constructors
    newMutArr,
    writeMutArr,
    resizeMutArr,
    copyIntoMutArr,
    -- * Accessors
    readMutArr,
    sizeMutArr
  )
where

import GHC.Exts
import GHC.Stack

-- # Unsafe wrappers
----------------------------

-- | A mutable array holding @a@s
type MutArr# a = MutableArray# RealWorld a

-- | Get the size of a mutable array
sizeMutArr :: MutArr# a -> Int
sizeMutArr arr  = I# (sizeofMutableArray# arr)

-- | Given a size, try to allocate a mutable array
-- of this size. The size should be non-negative.
newMutArr :: HasCallStack => Int -> a -> MutArr# a
newMutArr (I# size) x =
  case newArray of
    (# _, array #) -> array
  where
    newArray = runRW# $ \stateRW -> newArray# size x stateRW

-- | Write to a given mutable array arr; given an
-- index in [0, size(arr)-1], and a value.
writeMutArr :: HasCallStack => MutArr# a -> Int -> a -> ()
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
readMutArr :: HasCallStack => MutArr# a -> Int -> (# a #)
readMutArr mutArr (I# ix) =
  case doRead of (# _, a #) -> (# a #)
  where
    doRead = runRW# $ \stateRW -> readArray# mutArr ix stateRW

-- | Resize a mutable array. That is given an array, a size, and it returns
-- a new array of the given size using the seed value to fill in the new
-- cells when necessary and copying over all the unchanged cells.
--
-- The size should be non-negative.
--
-- @
-- let b = resize a x n,
--   then length b = n,
--   and b[i] = a[i] for i < length a,
--   and b[i] = x for length a <= i < n.
-- @
resizeMutArr :: HasCallStack => MutArr# a -> a -> Int -> MutArr# a
resizeMutArr mutArr x newSize = case newMutArr newSize x of
  newArr -> case copyIntoMutArr mutArr newArr of
    () -> newArr

-- | Copy the first mutable array into the second mutable array.
-- Copies fewer elements if the second array is smaller than the first.
copyIntoMutArr :: MutArr# a -> MutArr# a -> ()
copyIntoMutArr src dest = case doCopy of _ -> ()
  where
    doCopy = runRW# $ \stateRW ->
      copyMutableArray# src z dest z min_len stateRW
    (I# z) = 0 :: Int
    I# min_len =
      min
        (I# (sizeofMutableArray# src))
        (I# (sizeofMutableArray# dest))
