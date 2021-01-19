{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Mutable linear queues
--
-- This module provides mutable queues with a pure API. Import thusly:
--
-- > import qualified Data.Queue.Mutable.Linear as Linear
--
module Data.Queue.Mutable.Linear
  (
  -- * Construction
    Queue
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

data Queue a where
  Queue :: {-# UNPACK #-} !(Deque a) %1-> Queue a
  -- We represent a queue as a Deque where we add to the front end
  -- and take from the back end

unqueue :: Queue a %1-> Deque a
unqueue (Queue deq) = deq

-- | Allocate a queue with a given initial allocated size
alloc :: Int -> (Queue a %1-> Ur b) %1-> Ur b
alloc k f = Deque.alloc k $ \deq -> f (Queue deq)

-- | Given a list, make a queue where we treat the end of the list
-- as the top of the queue, the first-in-line element
fromList :: [a] -> (Queue a %1-> Ur b) %1-> Ur b
fromList xs f = Deque.fromList xs $ \deq -> f (Queue deq)

push :: a -> Queue a %1-> Queue a
push x = Queue . Deque.pushFront x . unqueue

pop :: Queue a %1-> (Ur (Maybe a), Queue a)
pop = Data.fmap Queue . Deque.popBack . unqueue

map :: (a -> b) -> Queue a %1-> Queue b
map f = Queue . Deque.map f . unqueue

top :: Queue a %1-> (Ur (Maybe a), Queue a)
top = Data.fmap Queue . Deque.peekBack . unqueue

length :: Queue a %1-> (Ur Int, Queue a)
length = Data.fmap Queue . Deque.length . unqueue

-- | Convert to a list where the head of the
-- list is the top of the stack
toList :: Queue a %1-> Ur [a]
toList = Deque.toList . unqueue

instance Consumable (Queue a) where
  consume (Queue deq) = consume deq

