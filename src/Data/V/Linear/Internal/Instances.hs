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
import Data.V.Linear.Internal (V (..))
import qualified Data.V.Linear.Internal as V
import qualified Data.Vector as Vector
import GHC.TypeLits
import qualified Prelude
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe

-- # Instances of V
-------------------------------------------------------------------------------

instance Data.Functor (V n) where
  fmap = V.map

instance KnownNat n => Data.Applicative (V n) where
  pure = V.pure
  a <*> b = a V.<*> b

instance KnownNat n => Prelude.Applicative (V n) where
  pure = V.pure
  V fs <*> V xs = V $ Vector.zipWith ($) fs xs

instance KnownNat n => Data.Traversable (V n) where
  traverse f (V xs) =
    (V . Unsafe.toLinear (Vector.fromListN (V.theLength @n)))
      Data.<$> Data.traverse f (Unsafe.toLinear Vector.toList xs)
