{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

-- |
-- This module provides an unlifted mutable array with a pure
-- interface. Though the array itself is unlifted, it's elements are
-- lifted types. This is made possible by using linear types to make
-- sure array references are single threaded through reads and writes.
--
-- Accessing out-of-bounds indices causes undefined behaviour.
--
-- This module is meant to be imported qualified.
module Data.Array.Mutable.Unlifted.Linear
  ( Array#
  , unArray#
  , alloc
  , allocBeside
  , lseq
  , size
  , read
  , write
  , copyInto
  ) where

import Data.Unrestricted.Linear hiding (lseq)
import Prelude (Int)
import qualified Prelude as Prelude
import qualified Unsafe.Linear as Unsafe
import qualified GHC.Exts as GHC

-- | A mutable array holding @a@s
newtype Array# a = Array# (GHC.MutableArray# GHC.RealWorld a)

-- | Extract the underlying 'GHC.MutableArray#', consuming the 'Array#'
-- in process.
unArray# :: (GHC.MutableArray# GHC.RealWorld a -> b) -> Array# a #-> Ur b
unArray# f = Unsafe.toLinear (\(Array# a) -> Ur (f a))

-- | Consume an 'Array#'.
--
-- Note that we can not implement a 'Consumable' instance because 'Array#'
-- is unlifted.
lseq :: Array# a #-> b #-> b
lseq = Unsafe.toLinear2 (\_ b -> b)

-- | Allocate a mutable array of given size using a default value.
--
-- The size should be non-negative.
alloc :: Int -> a -> (Array# a #-> Ur b) #-> Ur b
alloc (GHC.I# s) a f =
  let new = GHC.runRW# Prelude.$ \st ->
        case GHC.newArray# s a st of
          (# _, arr #) -> Array# arr
   in f new

-- | Allocate a mutable array of given size using a default value,
-- using another 'Array#' as a uniqueness proof.
--
-- The size should be non-negative.
allocBeside :: Int -> a -> Array# b #-> (# Array# b, Array# a #)
allocBeside (GHC.I# s) a orig =
  let new = GHC.runRW# Prelude.$ \st ->
        case GHC.newArray# s a st of
          (# _, arr #) -> Array# arr
   in (# orig, new #)

size :: Array# a #-> (# Array# a, Ur Int #)
size = Unsafe.toLinear go
  where
    go :: Array# a -> (# Array# a, Ur Int #)
    go (Array# arr) =
      let !s = GHC.sizeofMutableArray# arr
      in  (# Array# arr, Ur (GHC.I# s) #)

read ::  Int -> Array# a #-> (# Array# a, Ur a #)
read (GHC.I# i) = Unsafe.toLinear go
  where
    go :: Array# a -> (# Array# a, Ur a #)
    go (Array# arr) =
      case GHC.runRW# (GHC.readArray# arr i) of
        (# _, ret #) -> (# Array# arr, Ur ret #)

write :: Int -> a -> Array# a #-> Array# a
write (GHC.I# i) (a :: a) = Unsafe.toLinear go
  where
    go :: Array# a -> Array# a
    go (Array# arr) =
      case GHC.runRW# (GHC.writeArray# arr i a) of
        _ -> Array# arr

-- | Copy the first mutable array into the second mutable array.
-- This function is safe, it copies fewer elements if the second
-- array is smaller than the first.
copyInto :: Array# a #-> Array# a #-> (# Array# a, Array# a #)
copyInto = Unsafe.toLinear2 go
  where
    go :: Array# a -> Array# a -> (# Array# a, Array# a #)
    go (Array# src) (Array# dst) =
      let !(GHC.I# len#) = Prelude.min
            (GHC.I# (GHC.sizeofMutableArray# src))
            (GHC.I# (GHC.sizeofMutableArray# dst))
      in  case GHC.runRW# (GHC.copyMutableArray# src 0# dst 0# len#) of
            _ -> (# Array# src, Array# dst #)
