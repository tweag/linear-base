{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OffHeap.List where

import qualified Data.OffHeap as Manual
import Data.OffHeap (Pool, Box)
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple ()
import Prelude.Linear

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
-- Remark: omap could be tail-recursive in destination-passing style
omap :: forall a b. (Storable a, Storable b) => (a ->. b) -> List a ->. Pool ->. (List b, Pool)
omap _f Nil pool = (Nil, pool)
omap f (Cons a l) pool =
    rebuild a (omap f (Manual.deconstruct l) pool)
  where
    rebuild :: a ->. (List b, Pool) ->. (List b, Pool)
    rebuild a' (l', pool') = consS (f a') (Manual.alloc l' pool')

    consS :: b ->. (Box (List b), Pool) ->. (List b, Pool)
    consS b (l', pool') = (Cons b l', pool')

ofoldr :: forall a b. Storable a => (a ->. b ->. b) -> b ->. List a ->. b
ofoldr _f seed Nil = seed
ofoldr f seed (Cons a l) = f a (ofoldr f seed (Manual.deconstruct l))

ofoldl :: forall a b. Storable a => (b ->. a ->. b) -> b ->. List a ->. b
ofoldl _f seed Nil = seed
ofoldl f seed (Cons a l) = ofoldl f (f seed a) (Manual.deconstruct l)
