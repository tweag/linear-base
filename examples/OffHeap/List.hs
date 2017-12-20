{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OffHeap.List where

import qualified Data.OffHeap as Manual
import qualified Data.List as List
import Data.OffHeap (Pool, Box)
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple ()
import Prelude.Linear hiding (map, foldl, foldr)

-- XXX: we keep the last Cons in Memory here. A better approach would be to
-- always keep a Box instead.
data List a
  = Nil
  | Cons a (Box (List a))

-- TODO: generating appropriate instances using the Generic framework
instance Storable a => Storable (List a) where
  sizeOf _ = sizeOf (undefined :: (Word8, a, Box (List a)))
  alignment _ = alignment (undefined :: (Word8, a, Box (List a)))

  peek ptr = do
    (tag, a, l) <- peek (castPtr ptr :: Ptr (Word8, a, Box (List a)))
    case tag of
      0 -> return Nil
      1 -> return $ Cons a l
      _ -> error "Storable (List a): peek: unknown tag"

  poke ptr Nil = poke (castPtr ptr :: Ptr Word8) 0
  poke ptr (Cons a l) = poke (castPtr ptr :: Ptr (Word8, a, Box (List a))) (1, a, l)

-- Remark: this is a bit wasteful, we could implement an allocation-free map by
-- reusing the old pointer with realloc.
--
-- XXX: the mapped function should be of type (a ->. Pool ->. (b, Pool))
--
-- XXX: We're really starting to need a linear state monad to manage the pool
--
-- Remark: map could be tail-recursive in destination-passing style
map :: forall a b. (Storable a, Storable b) => (a ->. b) -> List a ->. Pool ->. (List b, Pool)
map _f Nil pool = (Nil, pool)
map f (Cons a l) pool =
    rebuild a (map f (Manual.deconstruct l) pool)
  where
    rebuild :: a ->. (List b, Pool) ->. (List b, Pool)
    rebuild a' (l', pool') = consS (f a') (Manual.alloc l' pool')

    consS :: b ->. (Box (List b), Pool) ->. (List b, Pool)
    consS b (l', pool') = (Cons b l', pool')

foldr :: forall a b. Storable a => (a ->. b ->. b) -> b ->. List a ->. b
foldr _f seed Nil = seed
foldr f seed (Cons a l) = f a (foldr f seed (Manual.deconstruct l))

foldl :: forall a b. Storable a => (b ->. a ->. b) -> b ->. List a ->. b
foldl _f seed Nil = seed
foldl f seed (Cons a l) = foldl f (f seed a) (Manual.deconstruct l)

-- Remark: could be tail-recursive with destination-passing style
-- | Make a 'List' from a stream. 'List' is a type of strict lists, therefore
-- the stream must terminate otherwise 'unfold' will loop. Not tail-recursive.
unfold :: forall a s. Storable a => (s -> Maybe (a,s)) -> s -> Pool ->. (List a, Pool)
unfold step state = dispatch (step state)
  where
    dispatch :: Maybe (a, s) -> Pool ->. (List a, Pool)
    dispatch Nothing pool = (Nil, pool)
    dispatch (Just (a, next)) pool =
      consS a (uncurry Manual.alloc (unfold step next pool))

    consS :: a ->. (Box (List a),  Pool) ->. (List a, Pool)
    consS a (l, pool) = (Cons a l, pool)

-- | Linear variant of 'unfold'. Note how they are implemented exactly
-- identically. They could be merged if multiplicity polymorphism was supported.
unfoldL :: forall a s. Storable a => (s ->. Maybe (a,s)) -> s ->. Pool ->. (List a, Pool)
unfoldL step state = dispatch (step state)
  where
    dispatch :: Maybe (a, s) ->. Pool ->. (List a, Pool)
    dispatch Nothing pool = (Nil, pool)
    dispatch (Just (a, next)) pool =
      consS a (uncurry Manual.alloc (unfoldL step next pool))

    consS :: a ->. (Box (List a),  Pool) ->. (List a, Pool)
    consS a (l, pool) = (Cons a l, pool)

ofList :: Storable a => [a] -> Pool ->. (List a, Pool)
ofList l pool = unfold List.uncons l pool

toList :: Storable a => List a ->. [a]
toList l = foldr (:) [] l
