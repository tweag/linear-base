{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module: MutableArray
-- Description: Unsafe wrappers around `MutableArray#`s

module Data.Linear.Internal.MutableArray
  ( unsafeNewMutArr
  , readMutArr
  , writeMutArr
  )
where


import GHC.Exts



-- # Unsafe wrappers
----------------------------

unsafeNewMutArr :: Int -> a -> MutableArray# RealWorld a
unsafeNewMutArr (I# size) init =
  case newArray of
    (# _, array #) -> array
  where
    newArray = runRW# $ \stateRW -> newArray# size init stateRW 


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


