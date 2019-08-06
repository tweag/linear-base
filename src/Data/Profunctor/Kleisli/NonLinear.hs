{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Data.Profunctor.Kleisli.NonLinear
  ( Kleisli(..)
  )
  where

import Data.Profunctor.Linear
import Data.Void
import qualified Prelude
import Prelude.Linear (Either(..), forget)
import Prelude.Linear.Internal.Simple (($))

-- Non-linear Kleisli arrows for the monad `m`. As in the linear case,
-- these arrows are still useful if `m` is only a `Functor` or an
-- `Applicative`.
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Prelude.Functor f => Profunctor (Kleisli f) where
  dimap f g (Kleisli h) = Kleisli (\x -> forget g Prelude.<$> h (f x))

instance Prelude.Functor f => Strong (,) () (Kleisli f) where
  first  (Kleisli f) = Kleisli (\(a,b) -> (,b) Prelude.<$> f a)
  second (Kleisli g) = Kleisli (\(a,b) -> (a,) Prelude.<$> g b)

instance Prelude.Applicative f => Strong Either Void (Kleisli f) where
  first  (Kleisli f) = Kleisli $ \case
                                   Left  x -> Prelude.fmap Left (f x)
                                   Right y -> Prelude.pure (Right y)
