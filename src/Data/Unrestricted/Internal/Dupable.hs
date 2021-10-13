{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Prelude (Bool (..), Maybe (..), Either (..), Ordering (..))
import qualified Data.Vector as Vector
import GHC.Exts (Char (..), Double (..), Float (..), Int (..), Word (..))
import qualified Data.Semigroup as Semigroup
import qualified Data.Monoid as Monoid
import Data.Unrestricted.Internal.Ur (Ur (..))
import Data.List.NonEmpty (NonEmpty (..))

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

-- ---------------
-- Primitive types

instance Dupable Prelude.Char where
  dup2 = genericDup2
  dupV (C# c) = Unsafe.toLinear (\x -> Data.pure (C# x)) c

instance Dupable Prelude.Double where
  dup2 = genericDup2
  dupV (D# i) = Unsafe.toLinear (\j -> Data.pure (D# j)) i

instance Dupable Prelude.Float where
  dup2 = genericDup2
  dupV (F# i) = Unsafe.toLinear (\j -> Data.pure (F# j)) i

instance Dupable Prelude.Int where
  -- /!\ 'Int#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int#' and using it several times. /!\
  dup2 = genericDup2
  dupV (I# i) = Unsafe.toLinear (\j -> Data.pure (I# j)) i

instance Dupable Prelude.Word where
  dup2 = genericDup2
  dupV (W# i) = Unsafe.toLinear (\j -> Data.pure (W# j)) i

-- Prelude types
instance Dupable () where
  dup2 () = ((), ())

  dupV () = Data.pure ()

instance (Dupable a, Dupable b) => Dupable (a, b) where
  dupV (a, b) = (,) Data.<$> dupV a Data.<*> dupV b

instance (Dupable a, Dupable b, Dupable c) => Dupable (a, b, c) where
  dupV (a, b, c) = (,,) Data.<$> dupV a Data.<*> dupV b Data.<*> dupV c

instance Dupable a => Dupable (NonEmpty a) where
  dupV (x :| xs) = (:|) Data.<$> dupV x Data.<*> dupV xs

instance Dupable (Ur a) where
  dupV (Ur a) = Data.pure (Ur a)

instance Dupable Bool where
  dup2 = genericDup2

  dupV True = Data.pure True
  dupV False = Data.pure False

instance Dupable Ordering where
  dup2 = genericDup2

  dupV LT = Data.pure LT
  dupV GT = Data.pure GT
  dupV EQ = Data.pure EQ

instance Dupable a => Dupable (Maybe a) where
  dup2 = genericDup2

  dupV Nothing = Data.pure Nothing
  dupV (Just x) = Data.fmap Just (dupV x)

instance (Dupable e, Dupable a) => Dupable (Either e a) where
  dup2 = genericDup2

  dupV (Left x) = Data.fmap Left (dupV x)
  dupV (Right y) = Data.fmap Right (dupV y)

instance Dupable a => Dupable [a] where
  dup2 = genericDup2

  dupV [] = Data.pure []
  dupV (a:l) = (:) Data.<$> dupV a Data.<*> dupV l

-- Data.Semigroup instances

instance (Dupable a, Dupable b) => Dupable (Semigroup.Arg a b) where
  dup2 = genericDup2

  dupV (Semigroup.Arg a b) = Semigroup.Arg Data.<$> dupV a Data.<*> dupV b

deriving newtype instance _ => Dupable (Semigroup.Min a)
deriving newtype instance _ => Dupable (Semigroup.Max a)
deriving newtype instance _ => Dupable (Semigroup.First a)
deriving newtype instance _ => Dupable (Semigroup.Last a)
deriving newtype instance _ => Dupable (Semigroup.WrappedMonoid a)
deriving newtype instance _ => Dupable (Semigroup.Dual a)
deriving newtype instance Dupable Semigroup.All
deriving newtype instance Dupable Semigroup.Any
deriving newtype instance _ => Dupable (Semigroup.Sum a)
deriving newtype instance _ => Dupable (Semigroup.Product a)

-- Data.Monoid instances

deriving newtype instance _ => Dupable (Monoid.First a)
deriving newtype instance _ => Dupable (Monoid.Last a)
deriving newtype instance _ => Dupable (Monoid.Alt f a)
deriving newtype instance _ => Dupable (Monoid.Ap f a)



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

-- Instances for unlifted generic representations
--
-- /!\ 'Char#', 'Double#', 'Float#', 'Int#', 'Word#' are unboxed data-types,
-- and therefore they cannot have any linear values hidden in a closure
-- anywhere. Therefore it is safe to call non-linear functions linearly on
-- these types. We refrain from including a 'GConsumable' instance for 'UAddr'
-- for the moment, as that seems potentially confusing—pointers usually
-- must be created, duplicated, and destroyed rather carefully. /!\

instance GDupable UChar where
  gdup2 (UChar i) = Unsafe.toLinear (\j -> (UChar j, UChar j)) i

instance GDupable UDouble where
  gdup2 (UDouble i) = Unsafe.toLinear (\j -> (UDouble j, UDouble j)) i

instance GDupable UFloat where
  gdup2 (UFloat i) = Unsafe.toLinear (\j -> (UFloat j, UFloat j)) i

instance GDupable UInt where
  gdup2 (UInt i) = Unsafe.toLinear (\j -> (UInt j, UInt j)) i

instance GDupable UWord where
  gdup2 (UWord i) = Unsafe.toLinear (\j -> (UWord j, UWord j)) i
