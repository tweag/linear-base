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
  )
where

import GHC.Exts

-- # Unsafe wrappers
----------------------------

newMutArr :: Int -> a -> MutableArray# RealWorld a
newMutArr (I# size) x =
  case newArray of
    (# _, array #) -> array
  where
    newArray = runRW# $ \stateRW -> newArray# size x stateRW

-- | writeMutArr should have evaluation forced
writeMutArr :: MutableArray# RealWorld a -> Int -> a -> ()
writeMutArr mutArr (I# ix) val =
  case doWrite of _ -> ()
  where
    doWrite = runRW# $ \stateRW -> writeArray# mutArr ix val stateRW

readMutArr :: MutableArray# RealWorld a -> Int -> a
readMutArr mutArr (I# ix) =
  case doRead of (# _, a #) -> a
  where
    doRead = runRW# $ \stateRW -> readArray# mutArr ix stateRW
