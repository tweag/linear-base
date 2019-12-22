{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

module Data.Bifunctor.Linear where

import Prelude.Linear
import Data.Void

-- | Bifunctors on the category of linear types
--
-- Laws:
-- If 'bimap' is supplied, then
-- * @'bimap' 'id' 'id' = 'id'@
-- If 'first' and 'second' are supplied, then
-- * @
-- 'first' 'id' ≡ 'id'
-- 'second' 'id' ≡ 'id'
-- @
-- If all are supplied, then
-- * @'bimap' f g = 'first' f '.' 'second' g
class Bifunctor p where
  {-# MINIMAL bimap | (first , second) #-}
  bimap :: (a #-> b) -> (c #-> d) -> a `p` c #-> b `p` d
  bimap f g x = first f (second g x)
  {-# INLINE bimap #-}

  first :: (a #-> b) -> a `p` c #-> b `p` c
  first f = bimap f id
  {-# INLINE first #-}

  second :: (b #-> c) -> a `p` b #-> a `p` c
  second = bimap id
  {-# INLINE second #-}

instance Bifunctor (,) where
  bimap f g (x,y) = (f x, g y)
  first f (x,y) = (f x, y)
  second g (x,y) = (x, g y)

instance Bifunctor Either where
  bimap f g = either (Left . f) (Right . g)

-- TODO: assoc laws
-- | Symmetric monoidal products on the category of linear types
--
-- Laws
-- * @'swap' . 'swap' = 'id'@
class Bifunctor m => SymmetricMonoidal (m :: * -> * -> *) (u :: *) | m -> u, u -> m where
  {-# MINIMAL swap, (rassoc | lassoc) #-}
  rassoc :: (a `m` b) `m` c #-> a `m` (b `m` c)
  rassoc = swap . lassoc . swap . lassoc . swap
  lassoc :: a `m` (b `m` c) #-> (a `m` b) `m` c
  lassoc = swap . rassoc . swap . rassoc . swap
  swap :: a `m` b #-> b `m` a
-- XXX: should unitors be added?

instance SymmetricMonoidal (,) () where
  swap (x, y) = (y, x)
  rassoc ((x,y),z) = (x,(y,z))

instance SymmetricMonoidal Either Void where
  swap = either Right Left
  rassoc (Left (Left x)) = Left x
  rassoc (Left (Right x)) = (Right :: a #-> Either b a) (Left x)
  rassoc (Right x) = (Right :: a #-> Either b a) (Right x)
-- XXX: the above type signatures are necessary for certain older versions of
-- the compiler, and as such are temporary
