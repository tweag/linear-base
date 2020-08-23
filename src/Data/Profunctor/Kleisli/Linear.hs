{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

-- | This module is intended to be imported qualified

module Data.Profunctor.Kleisli.Linear
  ( Kleisli(..)
  , CoKleisli(..)
  )
  where


import Data.Profunctor.Linear
import Data.Void
import Prelude.Linear (Either(..), either)
import Prelude.Linear.Internal
import qualified Control.Monad.Linear as Control
import qualified Data.Functor.Linear as Data

-- Ideally, there would only be one Kleisli arrow, parametrised by
-- a multiplicity parameter:
-- newtype Kleisli p m a b = Kleisli { runKleisli :: a # p -> m b }
--
-- Some instances would also still work, eg
-- instance Functor p f => Profunctor (Kleisli p f)

-- | Linear Kleisli arrows for the monad `m`. These arrows are still useful
-- in the case where `m` is not a monad however, and some profunctorial
-- properties still hold in this weaker setting.
newtype Kleisli m a b = Kleisli { runKleisli :: a #-> m b }

instance Data.Functor f => Profunctor (Kleisli f) where
  dimap f g (Kleisli h) = Kleisli (Data.fmap g . h . f)

instance Control.Functor f => Strong (,) () (Kleisli f) where
  first  (Kleisli f) = Kleisli (\(a,b) -> (,b) Control.<$> f a)
  second (Kleisli g) = Kleisli (\(a,b) -> (a,) Control.<$> g b)

instance Control.Applicative f => Strong Either Void (Kleisli f) where
  first  (Kleisli f) = Kleisli (either (Data.fmap Left . f) (Control.pure . Right))
  second (Kleisli g) = Kleisli (either (Control.pure . Left) (Data.fmap Right . g))

instance Control.Applicative f => Wandering (Kleisli f) where
  wander (Kleisli f) = Kleisli (Data.traverse f)

-- | Linear co-Kleisli arrows for the comonad `w`. These arrows are still
-- useful in the case where `w` is not a comonad however, and some
-- profunctorial properties still hold in this weaker setting.
-- However stronger requirements on `f` are needed for profunctorial
-- strength, so we have fewer instances.
--
-- Category theoretic remark: duality doesn't work in the obvious way, since
-- (,) isn't the categorical product. Instead, we have a product (&), called
-- "With", defined by
-- > type With a b = forall r. Either (a #-> r) (b #-> r) #-> r
-- which satisfies the universal property of the product of `a` and `b`.
-- CoKleisli arrows are strong with respect to this monoidal structure,
-- although this might not be useful...
newtype CoKleisli w a b = CoKleisli { runCoKleisli :: w a #-> b }

instance Data.Functor f => Profunctor (CoKleisli f) where
  dimap f g (CoKleisli h) = CoKleisli (g . h . Data.fmap f)

-- instance of a more general idea, but this will do for now
instance Strong Either Void (CoKleisli (Data.Const x)) where
  first (CoKleisli f) = CoKleisli (\(Data.Const x) -> (Left :: a #-> Either a b) (f (Data.Const x)))
-- XXX: the above type signature is necessary for certain older versions of
-- the compiler, and as such is temporary
