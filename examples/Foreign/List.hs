{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foreign.List where

import qualified Data.List as List
import Data.Word
import qualified Foreign.Marshal.Pure as Manual
import Foreign.Marshal.Pure (Pool, Box)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple ()
import Prelude.Linear hiding (map, foldl, foldr)

-- XXX: we keep the last Cons in Memory here. A better approach would be to
-- always keep a Box instead.
data List a
  = Nil
  | Cons !a !(Box (List a))

-- TODO: generating appropriate instances using the Generic framework
instance Storable a => Storable (List a) where
  sizeOf _ = sizeOf (undefined :: (Word8, a, Box (List a)))
  alignment _ = alignment (undefined :: (Word8, a, Box (List a)))

  peek ptr = do
    (tag, a, l) <- peek (castPtr ptr :: Ptr (Word8, a, Box (List a)))
    case tag of
      0 -> return Nil
      1 -> return $ Cons a l
      t -> error ("Storable (List a): peek: unknown tag ( " ++ show t ++ " )")

  poke ptr Nil = poke (castPtr ptr :: Ptr Word8) 0
  poke ptr (Cons a l) = poke (castPtr ptr :: Ptr (Word8, a, Box (List a))) (1, a, l)

-- Remark: this is a bit wasteful, we could implement an allocation-free map by
-- reusing the old pointer with realloc.
--
-- XXX: the mapped function should be of type (a ->. Pool ->. b)
--
-- Remark: map could be tail-recursive in destination-passing style
map :: forall a b. (Storable a, Storable b) => (a ->. b) -> List a ->. Pool ->. List b
map _f Nil pool = pool `lseq` Nil
map f (Cons a l) pool =
    withPools (dup pool) a (Manual.deconstruct l)
  where
    withPools :: (Pool, Pool) ->. a ->. List a ->. List b
    withPools (pool1, pool2) a' l' =
      Cons (f a') (Manual.alloc (map f l' pool1) pool2)

foldr :: forall a b. Storable a => (a ->. b ->. b) -> b ->. List a ->. b
foldr _f seed Nil = seed
foldr f seed (Cons a l) = f a (foldr f seed (Manual.deconstruct l))

foldl :: forall a b. Storable a => (b ->. a ->. b) -> b ->. List a ->. b
foldl _f seed Nil = seed
foldl f seed (Cons a l) = foldl f (f seed a) (Manual.deconstruct l)

-- Remark: could be tail-recursive with destination-passing style
-- | Make a 'List' from a stream. 'List' is a type of strict lists, therefore
-- the stream must terminate otherwise 'unfold' will loop. Not tail-recursive.
unfold :: forall a s. Storable a => (s -> Maybe (a,s)) -> s -> Pool ->. List a
unfold step state pool = dispatch (step state) (dup pool)
  -- XXX: ^ The reason why we need to `dup` the pool before we know whether the
  -- next step is a `Nothing` (in which case we don't need the pool at all) or a
  -- `Just`, is because of the limitation of `case` to the unrestricted
  -- case. Will be fixed.
  where
    dispatch :: Maybe (a, s) -> (Pool, Pool) ->. List a
    dispatch Nothing pools = pools `lseq` Nil
    dispatch (Just (a, next)) (pool1, pool2) =
      Cons a (Manual.alloc (unfold step next pool1) pool2)

-- | Linear variant of 'unfold'. Note how they are implemented exactly
-- identically. They could be merged if multiplicity polymorphism was supported.
unfoldL :: forall a s. Storable a => (s ->. Maybe (a,s)) -> s ->. Pool ->. List a
unfoldL step state pool = dispatch (step state) (dup pool)
  where
    dispatch :: Maybe (a, s) ->. (Pool, Pool) ->. List a
    dispatch Nothing pools = pools `lseq` Nil
    dispatch (Just (a, next)) (pool1, pool2) =
      Cons a (Manual.alloc (unfoldL step next pool1) pool2)

ofList :: Storable a => [a] -> Pool ->. List a
ofList l pool = unfold List.uncons l pool

toList :: Storable a => List a ->. [a]
toList l = foldr (:) [] l

-- | Like unfold but builds the list in reverse, and tail recursive
runfold :: forall a s. Storable a => (s -> Maybe (a,s)) -> s -> Pool ->. List a
runfold step state pool = loop state Nil pool
  where
    loop :: s -> List a ->. Pool ->. List a
    loop state' acc pool' = dispatch (step state') acc (dup pool')

    dispatch :: Maybe (a, s) -> List a ->. (Pool, Pool) ->. List a
    dispatch Nothing !acc pools = pools `lseq` acc
    dispatch (Just (a, next)) !acc (pool1, pool2) =
      loop next (Cons a (Manual.alloc acc pool1)) pool2

ofRList :: Storable a => [a] -> Pool ->. List a
ofRList l pool = runfold List.uncons l pool
