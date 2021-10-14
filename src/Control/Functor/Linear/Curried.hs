{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language LinearTypes #-}
{-# language NoImplicitPrelude #-}
-- This is lifted (pretty much) straight out of `kan-extensions`.
module Control.Functor.Linear.Curried
  ( Curried (..)
  , liftCurried
  , lowerCurried
  ) where

import Prelude.Linear.Internal
import Control.Functor.Linear.Internal.Class
import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Linear.Internal.Applicative as Data

newtype Curried g h a = Curried
  { runCurried :: forall r. g (a %1-> r) %1-> h r }

instance Data.Functor g => Data.Functor (Curried g h) where
  fmap f (Curried g) = Curried (g . Data.fmap (.f))
  {-# INLINE fmap #-}

instance Functor g => Functor (Curried g h) where
  fmap f (Curried g) = Curried (\x -> g (fmap (\y -> y . f) x))
  {-# INLINE fmap #-}

instance (Data.Functor g, g ~ h) => Data.Applicative (Curried g h) where
  pure a = Curried (Data.fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . Data.fmap (.))
  {-# INLINE (<*>) #-}

instance (Functor g, g ~ h) => Applicative (Curried g h) where
  pure a = Curried (fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . fmap (.))
  {-# INLINE (<*>) #-}

liftCurried :: Applicative f => f a %1-> Curried f f a
liftCurried ga = Curried (<*> ga)

lowerCurried :: Applicative f => Curried f g a %1-> g a
lowerCurried (Curried f) = f (pure id)
{-# INLINE lowerCurried #-}
