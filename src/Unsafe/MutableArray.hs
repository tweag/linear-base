{-# OPTIONS_GHC -Wno-unbanged-strict-patterns #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module: MutableArray
-- Description: Unsafe wrappers around `MutableArray#`s
module Unsafe.MutableArray
  ( newMutArr,
    readMutArr,
    writeMutArr,
    resizeMutArr,
    resizeMutArrSeed,
    copyIntoMutArr,
    sizeMutArr
  )
where

import GHC.Exts

-- # Unsafe wrappers
----------------------------

type MutArr# a = MutableArray# RealWorld a

sizeMutArr :: MutArr a -> Int
sizeMutArr arr  = I# (sizeofMutableArray# arr)

newMutArr :: Int -> a -> MutArr a
newMutArr (I# size) x =
  case newArray of
    (# _, array #) -> array
  where
    newArray = runRW# $ \stateRW -> newArray# size x stateRW

writeMutArr :: MutArr# a -> Int -> a -> ()
writeMutArr mutArr (I# ix) val =
  case doWrite of _ -> ()
  where
    doWrite = runRW# $ \stateRW -> writeArray# mutArr ix val stateRW

readMutArr :: MutArr# a -> Int -> a
readMutArr mutArr (I# ix) =
  case doRead of (# _, a #) -> a
  where
    doRead = runRW# $ \stateRW -> readArray# mutArr ix stateRW

-- | Resize a mutable array, using the first value to fill in the new cells
resizeMutArr :: MutArr a -> Int -> MutArr a
resizeMutArr mutArr newSize =
  resizeMutArrSeed mutArr (readMutArr mutArr 0) newSize

-- | Resize a mutable array, using a seed value
resizeMutArrSeed :: MutArr a -> a -> Int -> MutArr a
resizeMutArrSeed mutArr x newSize = case newMutArr newSize x of
  newArr -> case copyIntoMutArr mutArr newArr of
    () -> newArr

-- | Copy the first mutable array into the second, larger mutable array
copyIntoMutArr :: MutArr# a -> MutArr# a -> ()
copyIntoMutArr src dest = case doCopy of _ -> ()
  where
    doCopy = runRW# $ \stateRW -> copyMutableArray# src z dest z src_len stateRW
    (I# z) = 0 :: Int
    src_len = sizeofMutableArray# src
