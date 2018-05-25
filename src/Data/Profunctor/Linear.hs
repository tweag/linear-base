{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Profunctor.Linear
  ( Profunctor(..)
  , SymmetricMonoidal(..)
  , MonoidalProfunctor(..)
  , StrongProfunctor(..)
  ) where

import Data.Void
import Prelude.Linear hiding (swap)

-- TODO: write laws

class Profunctor (arr :: * -> * -> *) where
  {-# MINIMAL dimap | lmap, rmap #-}

  dimap :: (s ->. a) -> (b ->. t) -> a `arr` b -> s `arr` t
  dimap f g = lmap f . rmap g
  {-# INLINE dimap #-}

  lmap :: (s ->. a) -> a `arr` t -> s `arr` t
  lmap f = dimap f id
  {-# INLINE lmap #-}

  rmap :: (b ->. t) -> s `arr` b -> s `arr` t
  rmap = dimap id
  {-# INLINE rmap #-}

-- TODO: Move to a dedicated module
-- TODO: Is a bifunctor
-- TODO: associators
-- TODO: remove `swap` from Prelude
-- | Symmetric monoidal products on the category of linear types
--
-- Laws
-- * @swap . swap = id@
class SymmetricMonoidal (m :: * -> * -> *) (u :: *) | m -> u, u -> m where
  swap :: a `m` b ->. b `m` a

instance SymmetricMonoidal (,) () where
  swap (x, y) = (y, x)

instance SymmetricMonoidal Either Void where
  swap (Left x) = Right x
  swap (Right x) = Left x

class (SymmetricMonoidal m u, Profunctor arr) => MonoidalProfunctor m u arr where
  (***) :: a `arr` b -> u `arr` v -> (a `m` u) `arr` (b `m` v)
  unit :: u `arr` u

class (SymmetricMonoidal m u, Profunctor arr) => StrongProfunctor m u arr where
  {-# MINIMAL first | second #-}

  first :: a `arr` b -> (a `m` c) `arr` (b `m` c)
  first arr = dimap swap swap $ second arr
  {-# INLINE first #-}

  second :: b `arr` c -> (a `m` b) `arr` (a `m` c)
  second arr = dimap swap swap $ first arr
  {-# INLINE second #-}
