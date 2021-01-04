{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module documents polarized arrays and top-level conversions
--
-- == What are polarized arrays and what are they good for?
--
-- Polarized arrays aim to offer an API to replace that of @Data.Vector@
-- with mechanisms to explicitly control when memory is allocated for
-- an array. The current status quo is to use some array or vector type
-- and rely on a good implementation and GHC's fusion capabilities
-- to avoid unnecessary allocations (and thus save memory and improve
-- the performance).
--
-- Polarized arrays are arrays with one of two polarities or directions: push
-- or pull. Push and pull arrays are two array types that do not allocate
-- memory with conversions to and from @Data.Vector@. The only API function
-- that allocates space for an array is @Push.alloc@. Nothing else allocates
-- memory and hence we are not relying on GHC to fuse according to a
-- particular implementation of our vector API and program.
--
-- === What is a pull array?
--
-- A pull array is one that it's easy to "pull" from and read. These arrays
-- work nicely as arguments to functions and we can fold, map, zip, and split
-- these easily.
--
-- A typical use of polarized arrays would construct a pull array to begin
-- a computation using arrays.
--
-- === What is a push array?
--
-- A push array is a finished result that we do not want to allocate just yet.
-- We can concatenate two push arrays, convert a pull array into a push array
-- (without any memory allocation), create constant push arrays and when
-- we desire allocate a push array to a @Data.Vector@:
--
-- > Push.alloc :: Push.Array a %1-> Vector a
--
-- A push array is typically created towards the end of a computation that uses
-- arrays and passed along until we are ready to allocate.
--
-- === What does using polarized arrays look like?
--
-- A typical computation would start by constructing a pull array,
-- computing over it, converting it to a push array while other computations
-- occur and then finally finishing the computation by allocating the push
-- array (or arrays).
--
-- A simple example is a one-time allocating filter on vectors:
--
-- @
-- vecfilter :: Vector a -> (a -> Bool) -> Vector a
-- vecfilter vec f = Push.alloc (transfer (loop (Pull.fromVector vec) f))
--   where
--     loop :: Pull.Array a -> (a -> Bool) -> Pull.Array a
--     loop arr f = case Pull.findLength arr of
--       (0,_) -> Pull.fromFunction (error "empty") 0
--       (n,_) -> case Pull.split 1 arr of
--         (head, tail) -> case Pull.index head 0 of
--           (a,_) ->
--             if f a
--             then Pull.append (Pull.singleton a) (loop tail f)
--             else loop tail f
-- @
--
--
-- == Aside: why do we need linear types?
--
-- To correctly represent a push array, we need a way of specifying a
-- computation that writes to and fills an array.
--
-- @
-- data Array a where
--   Array :: (forall b. (a %1-> b) -> DArray b %1-> ()) %1-> Int -> Array a
-- @
--
-- As documented with destination arrays in @Data.Array.Destination@,
-- any computation of type @DArray b %1-> ()@ must fill the array. Now,
-- since the @b@ is completely abstract due to the rank2 type
-- (read about -XRankNTypes for more) this computation must fill the array
-- by wrapping writes of values of type @a@ with the given linear conversion
-- function of type @a %1-> b@. This prevents the computation from being 
-- evaluated until we are sure we want to allocate.
--
-- == Background for the interested
--
-- To understand how polarized arrays work in greater depth, these links
-- may be of some help:
--
-- * http://www.cse.chalmers.se/~josefs/talks/LinArrays.pdf
-- * http://jyp.github.io/posts/controlled-fusion.html
-- * https://www.sciencedirect.com/science/article/pii/030439759090147A
--
module Data.Array.Polarized
  ( transfer
  , walk
  )
  where

import qualified Data.Array.Polarized.Pull.Internal as Pull
import qualified Data.Array.Polarized.Pull as Pull
import qualified Data.Array.Polarized.Push as Push
import Prelude.Linear
import qualified Prelude
import Data.Vector (Vector)

-- DEVELOPER NOTE:
--
-- The general spirit is: `Push.Array` are those arrays which are friendly in
-- returned-value position. And `Pull.Array` are those arrays which are friendly
-- in argument position. If you have more than one array in an unfriendly
-- position, you need to allocate (allocated arrays are friendly in all
-- positions).
--
-- There are three types of array which are involved, with conversion
-- functions available between them, the third being an allocated Vector.
-- The primary conversion functions are:
-- > Polarized.transfer :: Pull.Array a %1-> Push.Array a
-- > Push.alloc :: Push.Array a %1-> Vector a
-- > Pull.fromVector :: Vector a %1-> Pull.Array a
--
-- In this way, we gain further control over exactly when allocation may occur
-- in a fusion pipeline.
-- In such a pipeline converting one allocated array to another, it would be
-- common to begin with Pull.fromVector, and end with Push.alloc.

-- | Convert a pull array into a push array.
-- NOTE: this does NOT require allocation and can be in-lined.
transfer :: Pull.Array a %1-> Push.Array a
transfer (Pull.Array f n) =
  Push.Array
    (\k -> Prelude.foldl (\m i -> m <> (k (f i))) mempty [0..(n-1)])

-- | This is a shortcut convenience function
-- for @transfer . Pull.fromVector@.
walk :: Vector a %1-> Push.Array a
walk = transfer . Pull.fromVector
