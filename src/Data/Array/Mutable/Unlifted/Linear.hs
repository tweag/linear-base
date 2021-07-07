{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE KindSignatures #-}
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
  , get
  , set
  , copyInto
  , map
  , toList
  , freeze
  , dup2
  ) where

import Data.Unrestricted.Linear hiding (lseq, dup2)
import Prelude (Int)
import qualified Prelude as Prelude
import qualified Unsafe.Linear as Unsafe
import qualified GHC.Exts as GHC

-- | A mutable array holding @a@s
newtype Array# a = Array# (GHC.MutableArray# GHC.RealWorld a)

-- | Extract the underlying 'GHC.MutableArray#', consuming the 'Array#'
-- in process.
unArray# :: (GHC.MutableArray# GHC.RealWorld a -> b) -> Array# a %1-> Ur b
unArray# f = Unsafe.toLinear (\(Array# a) -> Ur (f a))

-- | Consume an 'Array#'.
--
-- Note that we can not implement a 'Consumable' instance because 'Array#'
-- is unlifted.
lseq :: Array# a %1-> b %1-> b
lseq = Unsafe.toLinear2 (\_ b -> b)

-- | Allocate a mutable array of given size using a default value.
--
-- The size should be non-negative.
alloc :: Int -> a -> (Array# a %1-> Ur b) %1-> Ur b
alloc (GHC.I# s) a f =
  let new = GHC.runRW# Prelude.$ \st ->
        case GHC.newArray# s a st of
          (# _, arr #) -> Array# arr
   in f new
{-# NOINLINE alloc #-}  -- prevents runRW# from floating outwards

-- For the reasoning behind these NOINLINE pragmas, see the discussion at:
-- https://github.com/tweag/linear-base/pull/187#pullrequestreview-489183531

-- | Allocate a mutable array of given size using a default value,
-- using another 'Array#' as a uniqueness proof.
--
-- The size should be non-negative.
allocBeside :: Int -> a -> Array# b %1-> (# Array# a, Array# b #)
allocBeside (GHC.I# s) a orig =
  let new = GHC.runRW# Prelude.$ \st ->
        case GHC.newArray# s a st of
          (# _, arr #) -> Array# arr
   in (# new, orig #)
{-# NOINLINE allocBeside #-}  -- prevents runRW# from floating outwards

size :: Array# a %1-> (# Ur Int, Array# a #)
size = Unsafe.toLinear go
  where
    go :: Array# a -> (# Ur Int, Array# a #)
    go (Array# arr) =
      let !s = GHC.sizeofMutableArray# arr
      in  (# Ur (GHC.I# s), Array# arr  #)

get ::  Int -> Array# a %1-> (# Ur a, Array# a #)
get (GHC.I# i) = Unsafe.toLinear go
  where
    go :: Array# a -> (# Ur a, Array# a #)
    go (Array# arr) =
      case GHC.runRW# (GHC.readArray# arr i) of
        (# _, ret #) -> (# Ur ret, Array# arr #)
{-# NOINLINE get #-}  -- prevents the runRW# effect from being reordered

set :: Int -> a -> Array# a %1-> Array# a
set (GHC.I# i) (a :: a) = Unsafe.toLinear go
  where
    go :: Array# a -> Array# a
    go (Array# arr) =
      case GHC.runRW# (GHC.writeArray# arr i a) of
        _ -> Array# arr
{-# NOINLINE set #-}  -- prevents the runRW# effect from being reordered

-- | Copy the first mutable array into the second mutable array, starting
-- from the given index of the source array.
--
-- It copies fewer elements if the second array is smaller than the
-- first. 'n' should be within [0..size src).
--
-- @
--  copyInto n src dest:
--   dest[i] = src[n+i] for i < size dest, i < size src + n
-- @
copyInto :: Int -> Array# a %1-> Array# a %1-> (# Array# a, Array# a #)
copyInto start@(GHC.I# start#) = Unsafe.toLinear2 go
  where
    go :: Array# a -> Array# a -> (# Array# a, Array# a #)
    go (Array# src) (Array# dst) =
      let !(GHC.I# len#) = Prelude.min
            (GHC.I# (GHC.sizeofMutableArray# src) Prelude.- start)
            (GHC.I# (GHC.sizeofMutableArray# dst))
      in  case GHC.runRW# (GHC.copyMutableArray# src start# dst 0# len#) of
            _ -> (# Array# src, Array# dst #)
{-# NOINLINE copyInto #-}  -- prevents the runRW# effect from being reordered

map :: (a -> b) -> Array# a %1-> Array# b
map (f :: a -> b) = Unsafe.toLinear (\(Array# as) ->
  let -- We alias the input array to write the resulting -- 'b's to,
      -- just to make the typechecker happy. Care must be taken to
      -- only read indices from 'as' that is not yet written to 'bs'.
      bs :: GHC.MutableArray# GHC.RealWorld b
      bs = GHC.unsafeCoerce# as
      len :: GHC.Int#
      len = GHC.sizeofMutableArray# as

      -- For each index ([0..len]), we read the element on 'as', pass
      -- it through 'f' and write to the same location on 'bs'.
      go :: GHC.Int# -> GHC.State# GHC.RealWorld -> ()
      go i st
        | GHC.I# i Prelude.== GHC.I# len = ()
        | Prelude.otherwise =
          case GHC.readArray# as i st of
            (# st', a #) ->
              case GHC.writeArray# bs i (f a) st' of
                !st'' -> go (i GHC.+# 1#) st''
   in GHC.runRW# (go 0#) `GHC.seq` Array# bs
  )
{-# NOINLINE map #-}

-- | Return the array elements as a lazy list.
toList :: Array# a %1-> Ur [a]
toList = unArray# Prelude.$ \arr ->
  go
    0
    (GHC.I# (GHC.sizeofMutableArray# arr))
    arr
 where
  go i len arr
    | i Prelude.== len = []
    | GHC.I# i# <- i =
        case GHC.runRW# (GHC.readArray# arr i#) of
          (# _, ret #) -> ret : go (i Prelude.+ 1) len arr

-- | /O(1)/ Convert an 'Array#' to an immutable 'GHC.Array#'.
freeze :: (GHC.Array# a -> b) -> Array# a %1-> Ur b
freeze f = unArray# go
 where
  go mut =
    case GHC.runRW# (GHC.unsafeFreezeArray# mut) of
      (# _, ret #) -> f ret

-- | Clone an array.
dup2 :: Array# a %1-> (# Array# a, Array# a #)
dup2 = Unsafe.toLinear go
 where
  go :: Array# a -> (# Array# a, Array# a #)
  go (Array# arr) =
    case GHC.runRW#
           (GHC.cloneMutableArray# arr 0# (GHC.sizeofMutableArray# arr)) of
      (# _, new #) -> (# Array# arr, Array# new #)
{-# NOINLINE dup2 #-}
