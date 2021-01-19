{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Mutable Linear Deque
--
-- This module provides a pure interface to a mutable deque.
--
-- It is designed to be imported qualfied:
--
-- > import qualfied Data.Deque.Mutable.Linear as Deque
module Data.Deque.Mutable.Linear
  (
  -- * Allocation
    Deque
  , alloc
  , fromList
  -- * Querying
  , size
  , length
  , peekFront
  , peekBack
  -- * Modification
  , pushFront
  , pushBack
  , popFront
  , popBack
  , map
  -- * Consumption
  , toList
  )
where

import qualified Data.Array.Mutable.Linear as Array
import Data.Unrestricted.Linear
import Prelude.Linear hiding (length, map)
import qualified Prelude


-- # Types
-------------------------------------------------------------------------------

data Deque a where
  Deque :: !Int -> !Ptr -> !(Array.Array a) %1-> Deque a
  -- This is: Deque length ptr array
  --
  -- The length is the number of elements stored.
  -- The ptr is the starting pointer to the front end, and the deque
  -- continues forward, wrapping the end if needed. Example:
  --
  -- [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  --  ....^                 ^..........
  --      |                 |
  --   ptr+len             ptr
  --
  --
  -- So the deque is: 7--8--9--10--0--1

newtype Ptr = Ptr Int deriving Prelude.Num

-- | The two faces of a deque
data Face = Front | Back


-- # Internal Helpers
-------------------------------------------------------------------------------

-- @backPtr offset len size ptr = ptr'@ where @ptr'@ is the pointer
-- to the back of the deque + the offset
-- Must have: len >= 1
backPtr :: Int -> Int -> Int -> Ptr -> Int
backPtr off len sz (Ptr p) = (off + p + len - 1) `mod` sz

-- @prevPtr size ptr@ will be the previous pointer
prevPtr :: Int -> Ptr -> Int
prevPtr sz (Ptr p) = (p - 1 + sz) `mod` sz

-- @nextPtr size ptr@ will be the next pointer
nextPtr :: Int -> Ptr -> Int
nextPtr sz (Ptr p) = (p + 1) `mod` sz


-- # Allocation
-------------------------------------------------------------------------------

-- | Run a computation of an empty Deque with a given size
alloc :: Int -> (Deque a %1-> Ur b) %1-> Ur b
alloc k f = Array.alloc k err $ \arr -> f (Deque 0 0 arr) where
  err = Prelude.error "Accessing error element of a collection!"

-- | Run a computation on a Deque that is deterimined by the given the list
-- where we treat the start and end of the list as the left and right pointers,
-- with the total capacity as the length of the list.
fromList :: [a] -> (Deque a %1-> Ur b) %1-> Ur b
fromList xs f =
  Array.fromList xs $ \arr -> f (Deque (Prelude.length xs) 0 arr)


-- # Querying
-------------------------------------------------------------------------------

-- | The total capacity of the Deque
size :: Deque a %1-> (Ur Int, Deque a)
size (Deque len ptr arr) = Array.size arr &
  \(sz, arr0) -> (sz, Deque len ptr arr0)

-- | The number of elements currently stored
length :: Deque a %1-> (Ur Int, Deque a)
length (Deque len ptr arr) = (Ur len, Deque len ptr arr)

-- | We are full if the length equals the size
isFull :: Deque a %1-> (Ur Bool, Deque a)
isFull d =
  size d & \(Ur sz, Deque len ptr arr) -> (Ur (len == sz), Deque len ptr arr)

peek :: Face -> Deque a %1-> (Ur (Maybe a), Deque a)
peek _ (Deque 0 p arr) = (Ur Nothing, Deque 0 p arr)
peek face (Deque len ptr@(Ptr p) arr) = case face of
  Front ->
    Array.read arr p & \(Ur a, arr0) -> (Ur (Just a), Deque len ptr arr0)
  Back -> Array.size arr & \(Ur sz, arr0) ->
    Array.read arr0 (backPtr 0 len sz ptr) & \(Ur a, arr1) ->
      (Ur (Just a), Deque len ptr arr1)

-- | View the top of the left queue
peekFront :: Deque a %1-> (Ur (Maybe a), Deque a)
peekFront = peek Front

-- | View the top of the right queue
peekBack :: Deque a %1-> (Ur (Maybe a), Deque a)
peekBack = peek Back


-- # Modification
-------------------------------------------------------------------------------

push :: Face -> a -> Deque a %1-> Deque a
push face x deq = isFull deq & \case
  (Ur True, deq0) -> push face x (doubleSize deq0)
  (Ur False, Deque 0 _ arr) -> Array.write arr 0 x & \arr0 -> Deque 1 0 arr0
  (Ur False, Deque len (Ptr p) arr) -> case face of
    Front -> Array.size arr & \(Ur sz, arr0) ->
      Array.write arr0 (prevPtr sz (Ptr p)) x & \arr1 ->
        Deque (len+1) (Ptr $ prevPtr sz (Ptr p)) arr1
    Back -> Array.size arr & \(Ur sz, arr0) ->
      Array.write arr0 (backPtr 1 len sz (Ptr p)) x & \arr1 ->
        Deque (len+1) (Ptr p) arr1

doubleSize :: Deque a %1-> Deque a
doubleSize (Deque len ptr@(Ptr start) arr) =
  Array.size arr & \(Ur sz, arr0) ->
    Array.resize (sz*2) err arr0 & \arr1 ->
      Deque len ptr (movePrefix 0 start arr1)
  where
    err = Prelude.error "Accessing error element of a collection!"
    movePrefix :: Int -> Int -> Array.Array a %1-> Array.Array a
    movePrefix ix p arr'
      | ix == p = arr'
      | otherwise = Array.read arr' ix & \(Ur a, arr0) ->
          Array.write arr0 (p+ix+1) a & \arr1 -> movePrefix (ix+1) p arr1

-- | Push to the front end
pushFront :: a -> Deque a %1-> Deque a
pushFront = push Front

-- | Push to the back end
pushBack :: a -> Deque a %1-> Deque a
pushBack = push Back

pop :: Face -> Deque a %1-> (Ur (Maybe a), Deque a)
pop _ (Deque 0 p arr) = (Ur Nothing, Deque 0 p arr)
pop face (Deque len ptr@(Ptr p) arr) = case face of
  Front -> Array.size arr & \(Ur sz, arr0) ->
    Array.read arr0 p & \(Ur a, arr1) ->
      (Ur (Just a), Deque (len-1) (Ptr $ nextPtr sz ptr) arr1)
  Back -> Array.size arr & \(Ur sz, arr0) ->
    Array.read arr0 (backPtr 0 len sz ptr) & \(Ur a, arr1) ->
      (Ur (Just a), Deque (len-1) ptr arr1)

-- | Remove the last added element from the left queue
popFront :: Deque a %1-> (Ur (Maybe a), Deque a)
popFront = pop Front

-- | Remove the last added element from the right queue
popBack :: Deque a %1-> (Ur (Maybe a), Deque a)
popBack = pop Back

-- Note: We can't use a Prelude.Functor nor a Data.Functor
-- because the mapped function need not be linear but we must
-- consume the Deque linearly. The types don't align.
-- Note: This could be more efficient if we only mapped the
-- elements we care about and coerced the rest
map :: (a -> b) -> Deque a %1-> Deque b
map f (Deque len p arr) = Deque len p (Array.map f arr)


-- # Consumption
-------------------------------------------------------------------------------

-- | Convert the Deque to a list where the first element is the left
-- top and the last element is the right top
toList :: Deque a %1-> Ur [a]
toList (Deque len (Ptr p) arr) = Array.size arr & \(Ur sz, arr0) ->
  loop len (backPtr 0 len sz (Ptr p)) [] arr0
  where
    loop :: Int -> Int -> [a] -> Array.Array a %1-> Ur [a]
    loop 0 _ xs arr' = lseq arr' (Ur xs)
    loop l ptr xs arr' = Array.read arr' ptr & \(Ur a, arr0) ->
      Array.size arr0 & \(Ur sz, arr1) ->
        loop (l-1) (prevPtr sz (Ptr ptr)) (a:xs) arr1

instance Consumable (Deque a) where
  consume (Deque _ _ arr) = consume arr

