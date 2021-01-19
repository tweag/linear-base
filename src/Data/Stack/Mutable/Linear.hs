{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Mutable linear stacks
--
-- This module provides mutable stacks with a pure API. Import thusly:
--
-- > import qualified Data.Stack.Mutable.Linear as Linear
--
module Data.Stack.Mutable.Linear
  (
  -- * Construction
    Stack
  , alloc
  , fromList
  -- * Modification
  , push
  , pop
  , map
  -- * Querying
  , top
  , length
  -- * Consumption
  , toList
  )
where

import Data.Deque.Mutable.Linear (Deque)
import qualified Data.Deque.Mutable.Linear as Deque
import qualified Data.Functor.Linear as Data
import Data.Unrestricted.Linear
import Prelude.Linear hiding (length, map)


-- # API
-------------------------------------------------------------------------------

data Stack a where
  Stack :: {-# UNPACK #-} !(Deque a) %1-> Stack a
  -- We represent a stack as a Deque where we grow and
  -- shrink from the **front** end

unstack :: Stack a %1-> Deque a
unstack (Stack deq) = deq

-- | Allocate a stack with a given initial allocated size
alloc :: Int -> (Stack a %1-> Ur b) %1-> Ur b
alloc k f = Deque.alloc k $ \deq -> f (Stack deq)

-- | Given a list, make a stack where we treat the head of the list
-- as the top of the stack
fromList :: [a] -> (Stack a %1-> Ur b) %1-> Ur b
fromList xs f = Deque.fromList xs $ \deq -> f (Stack deq)

push :: a -> Stack a %1-> Stack a
push x = Stack . Deque.pushFront x . unstack

pop :: Stack a %1-> (Ur (Maybe a), Stack a)
pop = Data.fmap Stack . Deque.popFront . unstack

map :: (a -> b) -> Stack a %1-> Stack b
map f = Stack . Deque.map f . unstack

top :: Stack a %1-> (Ur (Maybe a), Stack a)
top = Data.fmap Stack . Deque.peekFront . unstack

length :: Stack a %1-> (Ur Int, Stack a)
length = Data.fmap Stack . Deque.length . unstack

-- | Convert to a list where the head of the
-- list is the top of the stack
toList :: Stack a %1-> Ur [a]
toList = Deque.toList . unstack

instance Consumable (Stack a) where
  consume (Stack deq) = consume deq

