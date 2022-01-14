{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Implementation of pairing heaps stored off-heap
module Foreign.Heap where

import qualified Data.List as List
import Foreign.List (List)
import qualified Foreign.List as List
import Foreign.Marshal.Pure (Box, Pool)
import qualified Foreign.Marshal.Pure as Manual
import Prelude.Linear hiding (foldl)

data Heap k a
  = Empty
  | NonEmpty (Box (NEHeap k a))

data NEHeap k a
  = Heap k a (Box (List (NEHeap k a)))

instance
  (Manual.Representable k, Manual.Representable a) =>
  Manual.MkRepresentable (NEHeap k a) (k, a, Box (List (NEHeap k a)))
  where
  toRepr (Heap k a l) = (k, a, l)
  ofRepr (k, a, l) = Heap k a l

instance (Manual.Representable k, Manual.Representable a) => Manual.Representable (NEHeap k a) where
  type AsKnown (NEHeap k a) = Manual.AsKnown (k, a, (Box (List (NEHeap k a))))

-- * Non-empty heap primitives

singletonN :: (Manual.Representable k, Manual.Representable a) => k %1 -> a %1 -> Pool %1 -> NEHeap k a
singletonN k a pool = Heap k a (Manual.alloc List.Nil pool)

-- XXX: (Movable k, Ord k) is a bit stronger than strictly required. We could
-- give a linear version of `Ord` instead.
mergeN :: forall k a. (Manual.Representable k, Manual.Representable a, Movable k, Ord k) => NEHeap k a %1 -> NEHeap k a %1 -> Pool %1 -> NEHeap k a
mergeN (Heap k1 a1 h1) (Heap k2 a2 h2) pool =
  testAndRebuild (move k1) a1 h1 (move k2) a2 h2 pool
  where
    --- XXX: this is a good example of why we need a working `case` and/or
    --- `let`.
    testAndRebuild :: Ur k %1 -> a %1 -> Box (List (NEHeap k a)) %1 -> Ur k %1 -> a %1 -> Box (List (NEHeap k a)) %1 -> Pool %1 -> NEHeap k a
    testAndRebuild (Ur k1') a1' h1' (Ur k2') a2' h2' =
      if k1' <= k2'
        then helper k1' a1' k2' a2' h1' h2'
        else helper k2' a2' k1' a1' h2' h1'

    helper :: k -> a %1 -> k -> a %1 -> Box (List (NEHeap k a)) %1 -> Box (List (NEHeap k a)) %1 -> Pool %1 -> NEHeap k a
    helper k1'' a1'' k2'' a2'' h1'' h2'' pool'' = Heap k1'' a1'' (Manual.alloc ((List.Cons :: b %1 -> Box (List b) %1 -> List b) ((Heap :: c %1 -> b %1 -> Box (List (NEHeap c b)) %1 -> NEHeap c b) k2'' a2'' h2'') h1'') pool'')

-- XXX: the type signatures for List.Cons and Heap are necessary for certain
-- older versions of the compiler, and as such are temporary. See PR #38
-- and PR #380 in tweag/ghc/linear-types.

mergeN' :: forall k a. (Manual.Representable k, Manual.Representable a, Movable k, Ord k) => NEHeap k a %1 -> Heap k a %1 -> Pool %1 -> NEHeap k a
mergeN' h Empty pool = pool `lseq` h
mergeN' h (NonEmpty h') pool = mergeN h (Manual.deconstruct h') pool

extractMinN :: (Manual.Representable k, Manual.Representable a, Movable k, Ord k) => NEHeap k a %1 -> Pool %1 -> (k, a, Heap k a)
extractMinN (Heap k a h) pool = (k, a, pairUp (Manual.deconstruct h) pool)

pairUp :: forall k a. (Manual.Representable k, Manual.Representable a, Movable k, Ord k) => List (NEHeap k a) %1 -> Pool %1 -> Heap k a
pairUp List.Nil pool = pool `lseq` Empty
pairUp (List.Cons h r) pool = pairOne h (Manual.deconstruct r) (dup pool)
  where
    pairOne :: NEHeap k a %1 -> List (NEHeap k a) %1 -> (Pool, Pool) %1 -> Heap k a
    pairOne h' r' (pool1, pool2) =
      NonEmpty $ Manual.alloc (pairOne' h' r' (dup3 pool1)) pool2

    pairOne' :: NEHeap k a %1 -> List (NEHeap k a) %1 -> (Pool, Pool, Pool) %1 -> NEHeap k a
    pairOne' h1 List.Nil pools =
      pools `lseq` h1
    pairOne' h1 (List.Cons h2 r') (pool1, pool2, pool3) =
      mergeN' (mergeN h1 h2 pool1) (pairUp (Manual.deconstruct r') pool2) pool3

-- * Heap primitives

empty :: Heap k a
empty = Empty

singleton :: forall k a. (Manual.Representable k, Manual.Representable a) => k %1 -> a %1 -> Pool %1 -> Heap k a
singleton k a pool = NonEmpty $ singletonAlloc k a (dup pool)
  where
    singletonAlloc :: k %1 -> a %1 -> (Pool, Pool) %1 -> Box (NEHeap k a)
    singletonAlloc k' a' (pool1, pool2) =
      Manual.alloc (singletonN k' a' pool1) pool2

extractMin :: (Manual.Representable k, Manual.Representable a, Movable k, Ord k) => Heap k a %1 -> Pool %1 -> Maybe (k, a, Heap k a)
extractMin Empty pool = pool `lseq` Nothing
extractMin (NonEmpty h) pool = Just $ extractMinN (Manual.deconstruct h) pool

merge :: forall k a. (Manual.Representable k, Manual.Representable a, Movable k, Ord k) => Heap k a %1 -> Heap k a %1 -> Pool %1 -> Heap k a
merge Empty h' pool = pool `lseq` h'
merge (NonEmpty h) h' pool = NonEmpty $ neMerge (Manual.deconstruct h) h' (dup pool)
  where
    neMerge :: NEHeap k a %1 -> Heap k a %1 -> (Pool, Pool) %1 -> Box (NEHeap k a)
    neMerge h1 h2 (pool1, pool2) =
      Manual.alloc (mergeN' h1 h2 pool1) pool2

-- * Heap sort

-- | Guaranteed to yield pairs in ascending key order
foldl :: forall k a b. (Manual.Representable k, Manual.Representable a, Movable k, Ord k) => (b %1 -> k %1 -> a %1 -> b) -> b %1 -> Heap k a %1 -> Pool %1 -> b
foldl f acc h pool = go acc h (dup pool)
  where
    go :: b %1 -> Heap k a %1 -> (Pool, Pool) %1 -> b
    go acc' h' (pool1, pool2) = dispatch acc' (extractMin h' pool1) pool2

    dispatch :: b %1 -> Maybe (k, a, Heap k a) %1 -> Pool %1 -> b
    dispatch acc' Nothing pool' = pool' `lseq` acc'
    dispatch acc' (Just (k, a, h')) pool' =
      foldl f (f acc' k a) h' pool'

-- | Strict: stream must terminate.
unfold :: forall k a s. (Manual.Representable k, Manual.Representable a, Movable k, Ord k) => (s -> Maybe ((k, a), s)) -> s -> Pool %1 -> Heap k a
unfold step seed pool = dispatch (step seed) pool
  where
    dispatch :: (Maybe ((k, a), s)) -> Pool %1 -> Heap k a
    dispatch Nothing pool' = pool' `lseq` Empty
    dispatch (Just ((k, a), next)) pool' = mkStep k a next (dup3 pool')

    mkStep :: k -> a -> s -> (Pool, Pool, Pool) %1 -> Heap k a
    mkStep k a next (pool1, pool2, pool3) =
      merge (singleton k a pool1) (unfold step next pool2) pool3

-- TODO: linear unfold: could apply to off-heap lists!

ofList :: (Manual.Representable k, Manual.Representable a, Movable k, Ord k) => [(k, a)] -> Pool %1 -> Heap k a
ofList l pool = unfold List.uncons l pool

-- XXX: sorts in reverse
toList :: (Manual.Representable k, Manual.Representable a, Movable k, Ord k) => Heap k a %1 -> Pool %1 -> [(k, a)]
toList h pool = foldl (\l k a -> (k, a) : l) [] h pool

sort :: forall k a. (Manual.Representable k, Manual.Representable a, Movable k, Ord k, Movable a) => [(k, a)] -> [(k, a)]
sort l = unur $ Manual.withPool (\pool -> move $ sort' l (dup pool))
  where
    -- XXX: can we avoid this call to `move`?

    sort' :: [(k, a)] -> (Pool, Pool) %1 -> [(k, a)]
    sort' l' (pool1, pool2) = toList (ofList l' pool1) pool2
