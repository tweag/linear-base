{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module contains all instances for V
module Data.V.Linear.Internal.Instances where

import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Linear.Internal.Traversable as Data
import Data.V.Linear.Internal.V
import qualified Data.Vector as Vector
import GHC.TypeLits
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe

-- # Instances of V
-------------------------------------------------------------------------------

instance Data.Functor (V n) where
  fmap f (V xs) = V $ Unsafe.toLinear (Vector.map (\x -> f x)) xs

instance KnownNat n => Data.Applicative (V n) where
  pure a = V $ Vector.replicate (theLength @n) a
  (V fs) <*> (V xs) =
    V $
      Unsafe.toLinear2 (Vector.zipWith (\f x -> f $ x)) fs xs

instance KnownNat n => Data.Traversable (V n) where
  traverse f (V xs) =
    (V . Unsafe.toLinear (Vector.fromListN (theLength @n)))
      Data.<$> Data.traverse f (Unsafe.toLinear Vector.toList xs)
