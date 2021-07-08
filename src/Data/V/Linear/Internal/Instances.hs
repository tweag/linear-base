{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module contains all instances for V
--
module Data.V.Linear.Internal.Instances where

import Data.V.Linear.Internal.V
import Prelude.Linear.Internal
import qualified Unsafe.Linear.Internal as Unsafe
import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Traversable as Data
import GHC.TypeLits
import qualified Data.Vector as Vector


-- # Instances of V
-------------------------------------------------------------------------------

instance Data.Functor (V n) where
  fmap f (V xs) = V $ Unsafe.toLinear (Vector.map (\x -> f x)) xs

instance KnownNat n => Data.Applicative (V n) where
  pure a = V $ Vector.replicate (theLength @n) a
  (V fs) <*> (V xs) = V $
    Unsafe.toLinear2 (Vector.zipWith (\f x -> f $ x)) fs xs

instance KnownNat n => Data.Traversable (V n) where
  traverse f (V xs) =
    (V . Unsafe.toLinear (Vector.fromListN (theLength @n))) Data.<$>
    Data.traverse f (Unsafe.toLinear Vector.toList xs)
