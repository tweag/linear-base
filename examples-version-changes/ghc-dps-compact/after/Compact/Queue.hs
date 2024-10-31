{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-type-defaults #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}

module Compact.Queue where

import Compact.DList
import qualified Compact.DList as DList
import Compact.Destination
import Data.Word
import Prelude.Linear hiding ((*), (+), (<))
import Prelude ((*), (+), (<))

data NaiveQueue a = NaiveQueue [a] [a]

newN :: NaiveQueue a
newN = NaiveQueue [] []

singletonN :: a %1 -> NaiveQueue a
singletonN x = NaiveQueue [x] []

toListN :: NaiveQueue a %1 -> [a]
toListN (NaiveQueue l r) = l ++ reverse r

enqueueN :: NaiveQueue a %1 -> a %1 -> NaiveQueue a
enqueueN (NaiveQueue l r) x = NaiveQueue l (x : r)

dequeueN :: NaiveQueue a %1 -> Maybe (a, NaiveQueue a)
dequeueN (NaiveQueue l r) = case l of
  [] -> case reverse r of
    [] -> Nothing
    (x : xs) -> Just (x, NaiveQueue xs [])
  (x : xs) -> Just (x, NaiveQueue xs r)

data QueueF a = QueueF [a] (DListN a)

newF :: forall a. QueueF a
newF = QueueF [] DList.newN

singletonF :: forall a. a %1 -> QueueF a
singletonF x = QueueF [x] DList.newN

toListF :: forall a. QueueF a %1 -> [a]
toListF (QueueF l dl) = l ++ DList.toListN dl

enqueueF :: forall a. QueueF a %1 -> a %1 -> QueueF a
enqueueF (QueueF l dl) x = QueueF l (DList.appendN dl x)

dequeueF :: forall a. QueueF a %1 -> Maybe (a, QueueF a)
dequeueF (QueueF l dl) = case l of
  [] -> case DList.toListN dl of
    [] -> Nothing
    (x : xs) -> Just (x, QueueF xs DList.newN)
  (x : xs) -> Just (x, QueueF xs dl)

data Queue r a = Queue [a] (DList r a)

new :: forall r a. (Region r) => Token %1 -> Queue r a
new token = Queue [] (DList.new @r token)

singleton :: forall r a. (Region r) => Token %1 -> a -> Queue r a
singleton token x = Queue [x] (DList.new @r token)

toList :: forall r a. (Region r) => Queue r a %1 -> [a]
toList (Queue l dl) = l ++ DList.toList dl

enqueue :: forall r a. (Region r) => Queue r a %1 -> a -> Queue r a
enqueue (Queue l dl) x = Queue l (DList.append dl x)

dequeue :: forall r a. (Region r) => Queue r a %1 -> Maybe (a, Queue r a)
dequeue (Queue l (DList i)) = case l of
  [] ->
    let !(i', token) = piggyback i
     in case DList.toList (DList i') of
          [] -> consume token `lseq` Nothing
          (x : xs) -> Just (x, Queue xs (DList.new @r token))
  (x : xs) -> Just (x, Queue xs (DList i))

-------------------------------------------------------------------------------

naiveImpl :: Word64 -> Word64
naiveImpl limit = go 0 (singletonN 1)
  where
    go sum q = case dequeueN q of
      Nothing -> sum
      Just (x, q') -> go (sum + x) q''
        where
          q'' =
            if x < limit
              then q' `enqueueN` (2 * x) `enqueueN` (2 * x + 1)
              else q'

funcImpl :: Word64 -> Word64
funcImpl limit = go 0 (singletonF 1)
  where
    go sum q = case dequeueF q of
      Nothing -> sum
      Just (x, q') -> go (sum + x) q''
        where
          q'' =
            if x < limit
              then q' `enqueueF` (2 * x) `enqueueF` (2 * x + 1)
              else q'

destImpl :: Word64 -> Word64
destImpl limit = unur (withRegion (\ @r t -> let r = go 0 (singleton @r t (Ur 1)) in move r))
  where
    go :: (Region r) => Word64 -> Queue r (Ur Word64) %1 -> Word64
    go sum q = case dequeue q of
      Nothing -> sum
      Just (Ur x, q') -> go (sum + x) q''
        where
          q'' =
            if x < limit
              then q' `enqueue` Ur (2 * x) `enqueue` Ur (2 * x + 1)
              else q'

impls :: [(Word64 -> Word64, String, Bool)]
impls =
  [ (naiveImpl, "naiveImpl", False),
    (funcImpl, "funcImpl", False),
    (destImpl, "destImpl", False)
  ]
