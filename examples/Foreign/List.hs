{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foreign.List where

import qualified Data.List as List
import Foreign.Marshal.Pure (Box, Pool)
import qualified Foreign.Marshal.Pure as Manual
import Prelude.Linear hiding (foldl, foldr, map)

-- XXX: we keep the last Cons in Memory here. A better approach would be to
-- always keep a Box instead.
data List a
  = Nil
  | Cons !a !(Box (List a))

-- TODO: generating appropriate instances using the Generic framework
instance
  Manual.Representable a =>
  Manual.MkRepresentable (List a) (Maybe (a, Box (List a)))
  where
  toRepr Nil = Nothing
  toRepr (Cons a l) = Just (a, l)

  ofRepr Nothing = Nil
  ofRepr (Just (a, l)) = Cons a l

instance Manual.Representable a => Manual.Representable (List a) where
  type AsKnown (List a) = Manual.AsKnown (Maybe (a, Box (List a)))

-- Remark: this is a bit wasteful, we could implement an allocation-free map by
-- reusing the old pointer with realloc.
--
-- XXX: the mapped function should be of type (a %1-> Pool %1-> b)
--
-- Remark: map could be tail-recursive in destination-passing style
map :: forall a b. (Manual.Representable a, Manual.Representable b) => (a %1 -> b) -> List a %1 -> Pool %1 -> List b
map _f Nil pool = pool `lseq` Nil
map f (Cons a l) pool =
  withPools (dup pool) a (Manual.deconstruct l)
  where
    withPools :: (Pool, Pool) %1 -> a %1 -> List a %1 -> List b
    withPools (pool1, pool2) a' l' =
      Cons (f a') (Manual.alloc (map f l' pool1) pool2)

foldr :: forall a b. Manual.Representable a => (a %1 -> b %1 -> b) -> b %1 -> List a %1 -> b
foldr _f seed Nil = seed
foldr f seed (Cons a l) = f a (foldr f seed (Manual.deconstruct l))

foldl :: forall a b. Manual.Representable a => (b %1 -> a %1 -> b) -> b %1 -> List a %1 -> b
foldl _f seed Nil = seed
foldl f seed (Cons a l) = foldl f (f seed a) (Manual.deconstruct l)

-- Remark: could be tail-recursive with destination-passing style

-- | Make a 'List' from a stream. 'List' is a type of strict lists, therefore
-- the stream must terminate otherwise 'unfold' will loop. Not tail-recursive.
unfold :: forall a s. Manual.Representable a => (s -> Maybe (a, s)) -> s -> Pool %1 -> List a
unfold step state pool = dispatch (step state) (dup pool)
  where
    -- XXX: ^ The reason why we need to `dup` the pool before we know whether the
    -- next step is a `Nothing` (in which case we don't need the pool at all) or a
    -- `Just`, is because of the limitation of `case` to the unrestricted
    -- case. Will be fixed.

    dispatch :: Maybe (a, s) -> (Pool, Pool) %1 -> List a
    dispatch Nothing pools = pools `lseq` Nil
    dispatch (Just (a, next)) (pool1, pool2) =
      Cons a (Manual.alloc (unfold step next pool1) pool2)

-- | Linear variant of 'unfold'. Note how they are implemented exactly
-- identically. They could be merged if multiplicity polymorphism was supported.
unfoldL :: forall a s. Manual.Representable a => (s %1 -> Maybe (a, s)) -> s %1 -> Pool %1 -> List a
unfoldL step state pool = dispatch (step state) (dup pool)
  where
    dispatch :: Maybe (a, s) %1 -> (Pool, Pool) %1 -> List a
    dispatch Nothing pools = pools `lseq` Nil
    dispatch (Just (a, next)) (pool1, pool2) =
      Cons a (Manual.alloc (unfoldL step next pool1) pool2)

ofList :: Manual.Representable a => [a] -> Pool %1 -> List a
ofList l pool = unfold List.uncons l pool

toList :: Manual.Representable a => List a %1 -> [a]
toList l = foldr (:) [] l

-- | Like unfold but builds the list in reverse, and tail recursive
runfold :: forall a s. Manual.Representable a => (s -> Maybe (a, s)) -> s -> Pool %1 -> List a
runfold step state pool = loop state Nil pool
  where
    loop :: s -> List a %1 -> Pool %1 -> List a
    loop state' acc pool' = dispatch (step state') acc (dup pool')

    dispatch :: Maybe (a, s) -> List a %1 -> (Pool, Pool) %1 -> List a
    dispatch Nothing !acc pools = pools `lseq` acc
    dispatch (Just (a, next)) !acc (pool1, pool2) =
      loop next (Cons a (Manual.alloc acc pool1)) pool2

ofRList :: Manual.Representable a => [a] -> Pool %1 -> List a
ofRList l pool = runfold List.uncons l pool
