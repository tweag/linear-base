{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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


import Data.Functor.Compose hiding (getCompose)
import Data.Profunctor.Linear
import qualified Data.Unrestricted.Linear as Ur
import Data.Void
import Prelude.Linear (Either(..), either, Ur(..))
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

instance Data.Applicative f => Monoidal (,) () (Kleisli f) where
  Kleisli f *** Kleisli g = Kleisli $ \(x,y) -> (,) Data.<$> f x Data.<*> g y
  unit = Kleisli $ \() -> Data.pure ()

instance Data.Functor f => Monoidal Either Void (Kleisli f) where
  Kleisli f *** Kleisli g = Kleisli $ \case
    Left a -> Left Data.<$> f a
    Right b -> Right Data.<$> g b
  unit = Kleisli $ \case {}

instance Control.Applicative f => Wandering (Kleisli f) where
  wander traverse (Kleisli f) = Kleisli (traverse f)

instance Data.Functor f => Unrestricting (Kleisli (Compose f Ur)) where
  unrestrict (Kleisli f) = Kleisli $ \(Ur a) -> Compose $  Ur.duplicate Data.<$> getCompose (f a)

getCompose :: Compose f g a #-> f (g a)
getCompose (Compose x) = x

-- | Linear co-Kleisli arrows for the comonad `w`. These arrows are still
-- useful in the case where `w` is not a comonad however, and some
-- profunctorial properties still hold in this weaker setting.
-- However stronger requirements on `f` are needed for profunctorial
-- strength, so we have fewer instances.
newtype CoKleisli w a b = CoKleisli { runCoKleisli :: w a #-> b }

instance Data.Functor f => Profunctor (CoKleisli f) where
  dimap f g (CoKleisli h) = CoKleisli (g . h . Data.fmap f)

instance Strong Either Void (CoKleisli (Data.Const x)) where
  first (CoKleisli f) = CoKleisli (\(Data.Const x) -> Left (f (Data.Const x)))
