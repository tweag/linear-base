{-# language RankNTypes #-}
{-# language LinearTypes #-}
{-# language NoImplicitPrelude #-}

-- | Data functors for free!
module Data.Functor.Linear.Yoneda
  ( Yoneda (..)
  , liftYoneda
  , lowerYoneda
  , yap
  ) where

import Prelude.Linear.Internal
import Data.Functor.Linear.Internal.Functor
import Data.Functor.Linear.Internal.Applicative

newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a %1-> b) -> f b }

instance Functor (Yoneda f) where
  fmap f (Yoneda m) = Yoneda (\k -> m (k . f))
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  {-# INLINE pure #-}
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (f .) <*> n id)
  {-# INLINE (<*>) #-}

liftYoneda :: Functor f => f a %1-> Yoneda f a
liftYoneda fa = Yoneda (<$> fa)
{-# INLINE liftYoneda #-}

lowerYoneda :: Yoneda f a %1-> f a
lowerYoneda (Yoneda m) = m id
{-# INLINE lowerYoneda #-}

yap :: Applicative f => Yoneda f (a %1-> b) %1-> f a %1-> Yoneda f b
yap (Yoneda k) fa = Yoneda (\ab_r -> k (ab_r .) <*> fa)
{-# INLINE yap #-}
