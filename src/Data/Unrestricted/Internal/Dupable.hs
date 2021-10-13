{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Unrestricted.Internal.Dupable
  (
  -- * Dupable
    Dupable(..)
  , dup
  , dup3
  -- * Generic deriving
  , GDupable
  , genericDup2
  ) where

import Data.Coerce
import Data.Unrestricted.Internal.Consumable
import GHC.TypeLits
import Data.Type.Equality
import Data.V.Linear.Internal.V (V)
import qualified Data.V.Linear.Internal.V as V
import Data.V.Linear.Internal.V (V (..))
import Generics.Linear
import Prelude.Linear.Generically
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import GHC.Types (Multiplicity (..))
import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Prelude as Prelude
import qualified Data.Vector as Vector

-- | The laws of @Dupable@ are dual to those of 'Monoid':
--
-- * @first consume (dup2 a) ≃ a ≃ second consume (dup2 a)@ (neutrality)
-- * @first dup2 (dup2 a) ≃ (second dup2 (dup2 a))@ (associativity)
--
-- Where the @(≃)@ sign represents equality up to type isomorphism.
--
-- When implementing 'Dupable' instances for composite types, using 'dupV'
-- should be more convenient since 'V' has a zipping 'Applicative' instance.
class Consumable a => Dupable a where
  {-# MINIMAL dupV | dup2 #-}

  dupV :: forall n. KnownNat n => a %1-> V n a
  dupV a =
    case V.caseNat @n of
      Prelude.Left Refl -> a `lseq` V.make @0 @a
      Prelude.Right Refl -> V.iterate dup2 a

  dup2 :: a %1-> (a, a)
  dup2 a = V.elim (dupV @a @2 a) (,)

dup3 :: Dupable a => a %1-> (a, a, a)
dup3 x = V.elim (dupV @_ @3 x) (,,)

dup :: Dupable a => a %1-> (a, a)
dup = dup2

-- ---------------------
-- Instances

-- XXX: The Consumable and Dupable instances for V will be easier to define (in
-- fact direct, we may consider adding a deriving-via combinator) when we have a
-- traversable-by-a-data-applicative class see #190.
instance (KnownNat n, Dupable a) => Dupable (V n a) where
  dupV (V xs) =
    V . Unsafe.toLinear (Vector.fromListN (V.theLength @n)) Data.<$>
    dupV (Unsafe.toLinear Vector.toList xs)

instance Dupable a => Dupable [a] where
  dupV [] = Data.pure []
  dupV (a:l) = (:) Data.<$> dupV a Data.<*> dupV l

-- ---------------------
-- Generic deriving

instance (Generic a, GDupable (Rep a)) => Dupable (Generically a) where
  dup2 = coerce (genericDup2 @a)

genericDup2 :: (Generic a, GDupable (Rep a)) => a %1-> (a, a)
genericDup2 a = gdup2 (from a) & \case (r1, r2) -> (to r1, to r2)

class GConsumable f => GDupable f where
  gdup2 :: f p %1-> (f p, f p)
instance GDupable V1 where
  gdup2 = \case
instance GDupable U1 where
  gdup2 U1 = (U1, U1)
instance (GDupable f, GDupable g) => GDupable (f :+: g) where
  gdup2 (L1 a) = gdup2 a & \case (x, y) -> (L1 x, L1 y)
  gdup2 (R1 a) = gdup2 a & \case (x, y) -> (R1 x, R1 y)
instance (GDupable f, GDupable g) => GDupable (f :*: g) where
  gdup2 (a :*: b) = gdup2 a & \case
    (a1, a2) -> gdup2 b & \case
      (b1, b2) -> (a1 :*: b1, a2 :*: b2)
instance Dupable c => GDupable (K1 i c) where
  gdup2 (K1 c) = dup2 c & \case (x, y) -> (K1 x, K1 y)
instance GDupable f => GDupable (M1 i t f) where
  gdup2 (M1 a) = gdup2 a & \case (x, y) -> (M1 x, M1 y)

instance GDupable (MP1 'Many f) where
  gdup2 (MP1 x) = (MP1 x, MP1 x)
instance GDupable f => GDupable (MP1 'One f) where
  gdup2 (MP1 a) = gdup2 a & \case (x, y) -> (MP1 x, MP1 y)
