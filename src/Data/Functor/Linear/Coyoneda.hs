{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language NoImplicitPrelude #-}

-- | Data functors for free. The 'Coyoneda' type offers a free
-- 'liftCoyoneda' function and a 'lowerCoyoneda' function that
-- actually performs the maps, fused. "Control.Functor.Linear.Coyoneda"
-- has a version for control functors. "Control.Functor.Linear.Yoneda"
-- has a somewhat different version.
module Data.Functor.Linear.Coyoneda
  ( Coyoneda (..)
  , liftCoyoneda
  , lowerCoyoneda
  ) where
import Data.Functor.Linear.Internal.Functor
import Data.Functor.Linear.Internal.Applicative
import Prelude.Linear.Internal
import Data.Kind (Type)

-- Hmm.... If the 'Applicative' class were multiplicity
-- polymorphic, I /think/ we could make a multiplicity polymorphic
-- 'Coyoneda' to match.

-- | Turn an arbitrary type into a data functor.
data Coyoneda :: (Type -> Type) -> Type -> Type where
  Coyoneda :: (a %1-> b) -> f a %1-> Coyoneda f b

instance Functor (Coyoneda f) where
  fmap f (Coyoneda g fa) = Coyoneda (f . g) fa

instance Applicative f => Applicative (Coyoneda f) where
  pure a = Coyoneda id (pure a)
  liftA2 f (Coyoneda g p) (Coyoneda h q)
    = Coyoneda id (liftA2 (\pres qres -> f (g pres) (h qres)) p q)

-- |
-- @
-- liftCoyoneda . 'lowerCoyoneda' = id
-- 'lowerCoyoneda' . liftCoyoneda = id
-- @
liftCoyoneda :: f a %1-> Coyoneda f a
liftCoyoneda = Coyoneda id

-- |
-- @
-- 'liftCoyoneda' . lowerCoyoneda = id
-- lowerCoyoneda . 'liftCoyoneda' = id
-- @
lowerCoyoneda :: Functor f => Coyoneda f a %1-> f a
lowerCoyoneda (Coyoneda f fa) = fmap f fa
