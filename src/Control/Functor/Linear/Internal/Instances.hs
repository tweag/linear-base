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
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data

-- # Deriving Data.XXX in terms of Control.XXX
-------------------------------------------------------------------------------

-- | This is a newtype for deriving Data.XXX classes from
-- Control.XXX classes.
newtype Data f a = Data (f a)

-- # Basic instances
-------------------------------------------------------------------------------

instance (Functor f) => Data.Functor (Data f) where
  fmap f (Data x) = Data (fmap f x)

instance (Applicative f) => Data.Applicative (Data f) where
  pure x = Data (pure x)
  Data f <*> Data x = Data (f <*> x)
