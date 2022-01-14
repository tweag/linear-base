{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Functor.Linear.Internal.Instances
  ( Data (..),
  )
where

import Control.Functor.Linear.Internal.Class
import Data.Functor.Compose
import Data.Functor.Identity
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import Data.Functor.Sum
import Data.Monoid.Linear hiding (Sum)
import Prelude.Linear.Internal

-- # Deriving Data.XXX in terms of Control.XXX
-------------------------------------------------------------------------------

-- | This is a newtype for deriving Data.XXX classes from
-- Control.XXX classes.
newtype Data f a = Data (f a)

-- # Basic instances
-------------------------------------------------------------------------------

instance Functor f => Data.Functor (Data f) where
  fmap f (Data x) = Data (fmap f x)

instance Applicative f => Data.Applicative (Data f) where
  pure x = Data (pure x)
  Data f <*> Data x = Data (f <*> x)

instance Functor ((,) a) where
  fmap f (a, x) = (a, f x)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (a, f) <*> (b, x) = (a <> b, f x)

instance Monoid a => Monad ((,) a) where
  (a, x) >>= f = go a (f x)
    where
      go :: a %1 -> (a, b) %1 -> (a, b)
      go b1 (b2, y) = (b1 <> b2, y)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  Identity x >>= f = f x

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (InL fa) = InL (fmap f fa)
  fmap f (InR ga) = InR (fmap f ga)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ fmap (fmap f) fga
