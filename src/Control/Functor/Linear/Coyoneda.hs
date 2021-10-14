{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LinearTypes #-}
{-# language NoImplicitPrelude #-}

-- | Control functors for free. The 'Coyoneda' type offers a free
-- 'liftCoyoneda' function and a 'lowerCoyoneda' function that
-- actually performs the maps, fused. "Data.Functor.Linear.Coyoneda"
-- has a version for data functors. "Control.Functor.Linear.Yoneda"
-- offers a somewhat different version.
module Control.Functor.Linear.Coyoneda
  ( Coyoneda (..)
  , liftCoyoneda
  , lowerCoyoneda
  , coyap
  ) where
import Data.Functor.Linear.Internal.Functor
import Data.Functor.Linear.Internal.Applicative
import Prelude.Linear.Internal
import Data.Kind (Type)
import qualified Control.Functor.Linear.Internal.Class as Control

-- | Turn an arbitrary type into a control functor.
data Coyoneda :: (Type -> Type) -> Type -> Type where
  Coyoneda :: (a %1-> b) %1-> f a %1-> Coyoneda f b

instance Functor (Coyoneda f) where
  fmap f (Coyoneda g fa) = Coyoneda (f . g) fa

instance Control.Functor (Coyoneda f) where
  fmap f (Coyoneda g fa) = Coyoneda (f . g) fa

instance Control.Applicative f => Applicative (Coyoneda f) where
  pure a = Coyoneda id (pure a)
  liftA2 f (Coyoneda g fa) (Coyoneda h fb) = Coyoneda id (Control.liftA2 (\a b -> f (g a) (h b)) fa fb)

instance Control.Applicative f => Control.Applicative (Coyoneda f) where
  pure a = Coyoneda id (Control.pure a)
  liftA2 f (Coyoneda g fa) (Coyoneda h fb) = Coyoneda id (Control.liftA2 (\a b -> f (g a) (h b)) fa fb)

-- |
-- @
-- 'liftCoyoneda' . lowerCoyoneda = id
-- lowerCoyoneda . 'liftCoyoneda' = id
-- @
lowerCoyoneda :: Control.Functor f => Coyoneda f a %1-> f a
lowerCoyoneda (Coyoneda f fa) = Control.fmap f fa

-- |
-- @
-- liftCoyoneda . 'lowerCoyoneda' = id
-- 'lowerCoyoneda' . liftCoyoneda = id
-- @
liftCoyoneda :: f a %1-> Coyoneda f a
liftCoyoneda = Coyoneda id

coyap :: Control.Applicative f => Coyoneda f (a %1-> b) %1-> f a %1-> Coyoneda f b
coyap (Coyoneda xab fx) fa = Coyoneda id (Control.liftA2 xab fx fa)
{-# INLINE coyap #-}
