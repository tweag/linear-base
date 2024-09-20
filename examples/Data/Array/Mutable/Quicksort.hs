{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Uncomment the line below to observe the generated (optimised) Core. It will
-- land in a file named “Quicksort.dump-simpl”
-- {-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dsuppress-uniques #-}

-- | This module implements quicksort with mutable arrays from linear-base
module Data.Array.Mutable.Quicksort where

import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
import Data.Unrestricted.Linear
import GHC.Stack
import Prelude.Linear hiding (partition)

-- # Quicksort
-------------------------------------------------------------------------------

qsortUsingList :: (Ord a) => [a] -> [a]
qsortUsingList [] = []
qsortUsingList (x : xs) = qsortUsingList ltx ++ x : qsortUsingList gex
  where
    ltx = [y | y <- xs, y < x]
    gex = [y | y <- xs, y >= x]

qsortUsingArray :: (Ord a) => [a] -> [a]
qsortUsingArray xs = unur $ Array.fromList xs $ Array.toList . qsortArray

qsortArray :: (Ord a) => Array a %1 -> Array a
qsortArray arr =
  Array.size arr
    & \(Ur len, arr1) -> go 0 (len - 1) arr1

go :: (Ord a) => Int -> Int -> Array a %1 -> Array a
go lo hi arr
  | lo >= hi = arr
  | otherwise =
      Array.read arr lo
        & \(Ur pivot, arr1) ->
          partition arr1 pivot lo hi
            & \(arr2, Ur ix) ->
              swap arr2 lo ix
                & \arr3 ->
                  go lo (ix - 1) arr3
                    & \arr4 -> go (ix + 1) hi arr4

-- | @partition arr pivot lo hi = (arr', Ur ix)@ such that
-- @arr'[i] <= pivot@ for @lo <= i <= ix@,
-- @arr'[j] > pivot@ for @ix < j <= hi@,
-- @arr'[k] = arr[k]@ for @k < lo@ and @k > hi@, and
-- @arr'@ is a permutation of @arr@.
partition :: (Ord a) => Array a %1 -> a -> Int -> Int -> (Array a, Ur Int)
partition arr pivot lo hi
  | (hi < lo) = (arr, Ur (lo - 1))
  | otherwise =
      Array.read arr lo
        & \(Ur lVal, arr1) ->
          Array.read arr1 hi
            & \(Ur rVal, arr2) -> case (lVal <= pivot, pivot < rVal) of
              (True, True) -> partition arr2 pivot (lo + 1) (hi - 1)
              (True, False) -> partition arr2 pivot (lo + 1) hi
              (False, True) -> partition arr2 pivot lo (hi - 1)
              (False, False) ->
                swap arr2 lo hi
                  & \arr3 -> partition arr3 pivot (lo + 1) (hi - 1)

-- | @swap a i j@ exchanges the positions of values at @i@ and @j@ of @a@.
swap :: (HasCallStack) => Array a %1 -> Int -> Int -> Array a
swap arr i j =
  Array.read arr i
    & \(Ur ival, arr1) ->
      Array.read arr1 j
        & \(Ur jval, arr2) -> (Array.set i jval . Array.set j ival) arr2
