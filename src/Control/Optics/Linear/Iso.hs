-- | This module provides linear isomorphisms
--
-- An @Iso a b s t@ is equivalent to a @(s \#-> a, b \#-> t)@.  In the simple
-- case of an @Iso' a s@, this is equivalent to inverse functions
-- @(s \#-> a, a \#-> s)@.  In the general case an @Iso a b s t@ means if you
-- have the isomorphisms @(a \#-> b, b \#-> a)@ and @(s \#-> t, t \#-> s)@, then
-- you can form isomorphisms between @s@, @t@, @a@ and @b@.
--
-- = Example
--
-- @
-- {-# LANGUAGE LinearTypes #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GADTs #-}
-- import qualified Data.Array.Mutable.Linear as Array
-- import Data.Array.Mutable.Linear (Array)
-- import Prelude.Linear (($), (&), Int, Bool(..), (.))
-- import qualified Prelude as Ur
-- import Prelude ((+), (-), Ord(..))
-- import qualified Data.Vector as Vector
-- import Data.Vector (Vector)
-- import Data.Unrestricted.Linear
--
-- isoListArray :: Iso' (Vector a -> Ur b) (Array a %1-> Ur b)
-- isoListArray = iso byVecs byArrays where
--   byVecs :: (Vector a -> Ur b) %1-> Array a %1-> Ur b
--   byVecs onVecs arr = Array.freeze arr & \(Ur vec) -> onVecs vec
--
--   byArrays :: (Array a %1-> Ur b) %1-> Vector a -> Ur b
--   byArrays onArrs vec = Array.fromList (Vector.toList vec) onArrs
--
-- -- | Apply a mutable quicksort implementation on immutable vectors
-- quickSort :: Vector Int -> Vector Int
-- quickSort v = unur $
--   withIso isoListArray (\_ onVecs -> onVecs (Array.freeze . arrQuicksort)) v
--
-- -- Not important to know the details for this example of optics ...
-- -- but here is an imperative representation of quicksort:
--
-- arrQuicksort :: Array Int %1-> Array Int
-- arrQuicksort arr = Array.size arr & \case
--   (Ur len, arr') -> go 0 (len-1) arr' where
--     go :: Int -> Int -> Array Int %1-> Array Int
--     go lo hi arr = case lo >= hi of
--       True -> arr
--       False -> Array.read arr lo & \case
--         (Ur pivot, arr0) -> partition arr0 pivot lo hi & \case
--           (arr1, Ur ix) -> swapix arr1 lo ix & \case
--             arr2 -> go lo ix arr2 & \case
--               arr3 -> go (ix+1) hi arr3
--
-- partition :: Array Int %1-> Int -> Int -> Int -> (Array Int, Ur Int)
-- partition arr pivot lx rx
--   | (rx < lx) = (arr, Ur rx)
--   | True = Array.read arr lx & \case
--       (Ur lVal, arr1) -> Array.read arr1 rx & \case
--         (Ur rVal, arr2) -> case (lVal <= pivot, pivot < rVal) of
--           (True, True) -> partition arr2 pivot (lx+1) (rx-1)
--           (True, False) -> partition arr2 pivot (lx+1) rx
--           (False, True) -> partition arr2 pivot (lx-1) (rx-1)
--           (False, False) -> swapix arr2 lx rx & \case
--             arr3 -> partition arr3 pivot (lx+1) (rx-1)
--
-- swapix :: Array Int %1-> Int -> Int -> Array Int
-- swapix arr i j = Array.read arr i & \case
--   (Ur ival, arr1) -> Array.read arr1 j & \case
--     (Ur jval, arr2) -> Array.write (Array.write arr2 i jval) j ival
-- @
--
module Control.Optics.Linear.Iso
  ( -- * Types
    Iso, Iso'
    -- * Composing optics
  , (.>)
    -- * Common optics
  , swap, assoc
    -- * Using optics
  , withIso
    -- * Constructing optics
  , iso
  )
  where

import Control.Optics.Linear.Internal
