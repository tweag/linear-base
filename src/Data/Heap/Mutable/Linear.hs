{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Linear mutable heaps
--
-- Construction takes an order function which is used by default to construct
-- a min heap. For a max heap, simply construct with a flipped order function.
--
-- Designed to be imported thusly:
--
-- > import qualified Data.Heap.Mutable.Linear as Heap
--
module Data.Heap.Mutable.Linear
  (
  -- * Construction
    Heap
  , Order
  , alloc
  , fromList
  -- * Modification
  , push
  , pop
  -- * Query
  , top
  , size
  -- * Consumption
  , toSortedList
  , toListRaw
  -- * Sorting
  , heapSortAsc
  , heapSortDesc
  , heapSortBy
  )
where

import Data.Array.Mutable.Linear (Array)
import qualified Data.Array.Mutable.Linear as Array
import Data.Unrestricted.Linear
import Prelude.Linear
import qualified Prelude
import Data.List (minimumBy)


-- # API
-------------------------------------------------------------------------------

-- | An ordering which should satisfy the usual properties
-- of a total linear order
type Order a = a -> a -> Ordering

data Heap a where
  Heap :: Int -> Order a -> Array a %1-> Heap a
  -- We represent a heap with the size (number of elements), an order function,
  -- and an array. The array has a size at least the size of the heap.
  --
  -- We store a min heap given the order function. So each node is less than
  -- or equal to its children according to the given function.
  --
  -- The representation is the typical one discussed in section 6 of
  -- "Introduction to Algorithms" by CLRS (3rd edition)

-- | Allocate a min heap given the order function with some inital
-- allocated space
alloc :: Int -> Order a -> (Heap a %1-> Ur b) %1-> Ur b
alloc k ord f = Array.alloc k err $ \arr -> f (Heap 0 ord arr) where
  err = Prelude.error "Error heap element"

-- | Allocate a min heap given the order function, pre-loaded with the list
-- elements
fromList :: forall a b. [a] -> Order a -> (Heap a %1-> Ur b) %1-> Ur b
fromList xs ord' f | l <- Prelude.length xs = Array.fromList xs $ \arr ->
  f $ Heap l ord' (loop ord' ((l-2) `div` 2) l arr)
  where
    loop :: Order a -> Int -> Int -> Array a %1-> Array a
    loop _ 0 _  arr = arr
    loop ord ix len arr = trickleDown ord len ix arr & \arr0 ->
      loop ord (ix-1) len arr0

push :: a -> Heap a %1-> Heap a
push x (Heap sz ord arr) = Array.size arr & \(Ur arrSz, arr0) ->
  case (sz == arrSz) of
    True -> push x (Heap sz ord (doubleSize arr0))
    False -> Array.write arr0 sz x & \arr1 ->
      Heap (sz+1) ord (trickleUp ord sz arr1)

pop :: Heap a %1-> (Ur (Maybe a), Heap a)
pop (Heap 0 ord arr) = (Ur Nothing, Heap 0 ord arr)
pop (Heap sz ord arr) = Array.read arr 0 & \(Ur a, arr0) ->
  Array.read arr0 (sz-1) & \(Ur end, arr1) ->
    Array.write arr1 0 end & \arr2 ->
      (Ur (Just a), Heap (sz-1) ord $ trickleDown ord (sz-1) 0 arr2)

top :: Heap a %1-> (Ur (Maybe a), Heap a)
top (Heap 0 ord arr) = (Ur Nothing, Heap 0 ord arr)
top (Heap k ord arr) = Array.read arr 0 & \(Ur a, arr0) ->
  (Ur (Just a), Heap k ord arr0)

size :: Heap a %1-> (Ur Int, Heap a)
size (Heap sz ord arr) = (Ur sz, Heap sz ord arr)

toSortedList :: forall a. Heap a %1-> Ur [a]
toSortedList = loop [] where
  loop :: [a] -> Heap a %1-> Ur [a]
  loop xs h = pop h & \case
    (Ur Nothing, h0) -> lseq h0 (Ur xs)
    (Ur (Just x), h0) -> loop (x:xs) h0

-- | Give the list representation of the heap from chapter 6 of
-- "Introduction to algorithms" by CLRS (3rd Ed).
--
-- This just lays out a binary tree by going left to right
-- across levels starting with the root level going down.
toListRaw :: Heap a %1-> Ur [a]
toListRaw (Heap sz _ arr) = loop (sz-1) [] arr where
  loop :: Int -> [a] -> Array a %1-> Ur [a]
  loop 0 xs arr' = Array.read arr' 0 & \(Ur a, arr0) -> lseq arr0 (Ur (a:xs))
  loop k xs arr' = Array.read arr' k & \(Ur a, arr0) ->
    loop (k-1) (a:xs) arr0

heapSortDesc :: Prelude.Ord a => [a] -> [a]
heapSortDesc = unur' Prelude.. \xs -> fromList xs Prelude.compare toSortedList

heapSortAsc :: Prelude.Ord a => [a] -> [a]
heapSortAsc =
  unur' Prelude.. \xs -> fromList xs (Prelude.flip Prelude.compare) toSortedList

-- Sort in descending order
heapSortBy :: Order a -> [a] -> [a]
heapSortBy ord = unur' Prelude.. \xs -> fromList xs ord toSortedList

instance Consumable (Heap a) where
  consume (Heap _ _ arr) = consume arr

-- # Internal Helpers
-------------------------------------------------------------------------------
unur' :: Ur a -> a
unur' (Ur x) = x

-- Keep swaping the value at the current index with its parent
-- until either it is the root (index 0) or it is greater than or
-- equal to its parent
trickleUp :: Order a -> Int -> Array a %1-> Array a
trickleUp _ 0 arr = arr
trickleUp ord ix arr | pix <- parent ix = Array.read arr ix & \(Ur a, arr0) ->
  Array.read arr0 pix & \(Ur p, arr1) -> case ord a p of
    EQ -> arr1
    GT -> arr1
    LT -> trickleUp ord pix (swap ix pix arr1)

-- Given: an order, the heap size, an index and the array.
-- Keep swapping a certain index if either child is smaller until
-- we have no smaller children.
-- Note: the heap size is just one past the right most index that is
-- considered a valid element.
trickleDown :: Order a -> Int -> Int -> Array a %1-> Array a
trickleDown ord sz ix arr = findSmallerChild ord sz ix arr & \case
  (Ur Nothing, arr0) -> arr0
  (Ur (Just (ixChild, aChild)), arr0) -> Array.read arr0 ix & \(Ur a, arr1) ->
    case ord a aChild of
      EQ -> arr1
      LT -> arr1
      GT -> Array.write arr1 ix aChild & \arr2 ->
        Array.write arr2 ixChild a & \arr3 ->
          trickleDown ord sz ixChild arr3

-- Find index and value of smaller child if there is one
findSmallerChild :: forall a. Order a -> Int -> Int -> Array a %1->
  (Ur (Maybe (Int, a)), Array a)
findSmallerChild ord sz ix arr =
  loop arr [k | k <- [left ix, right ix], k < sz] [] & \case
    (Ur [], arr0) -> (Ur Nothing, arr0)
    (Ur vals, arr0) -> (Ur (Just $ minimumBy comp vals), arr0)
  where
    comp (_,a) (_,b) = ord a b
    loop :: Array a %1-> [Int] -> [(Int,a)] -> (Ur [(Int,a)], Array a)
    loop arr' [] ys = (Ur ys, arr')
    loop arr' (x:xs) ys = Array.read arr' x & \(Ur y, arr0) ->
      loop arr0 xs ((x,y):ys)

doubleSize :: Array a %1-> Array a
doubleSize arr = Array.size arr & \(Ur sz, arr0) ->
  Array.resize ((sz*2) + 1) err arr0 where
    err = Prelude.error "Error heap element"

swap :: Int -> Int -> Array a %1-> Array a
swap i j arr =
  Array.read arr i & \(Ur ai, arr0) -> Array.read arr0 j & \(Ur aj, arr1) ->
      Array.write arr1 i aj & \arr2 -> Array.write arr2 j ai

parent :: Int -> Int
parent ix = (ix - 1) `div` 2

left :: Int -> Int
left ix = 2*ix + 1

right :: Int -> Int
right ix = 2*ix + 2

