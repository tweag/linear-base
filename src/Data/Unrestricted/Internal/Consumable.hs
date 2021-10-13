{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Unrestricted.Internal.Consumable
  (
  -- * Consumable
    Consumable(..)
  , lseq
  , seqUnit

  -- * Generic deriving
  , GConsumable
  , genericConsume
  )
  where

import Prelude.Linear.Generically
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import Generics.Linear
import GHC.Types (Multiplicity (..))
import qualified Prelude as Prelude
import Data.List.NonEmpty (NonEmpty)
import Data.Unrestricted.Internal.Ur
import qualified Data.Semigroup as Semigroup
import qualified Data.Monoid as Monoid
import GHC.Tuple (Solo)
import Data.V.Linear.Internal.V (V (..))
import qualified Data.Vector as Vector

class Consumable a where
  consume :: a %1-> ()

-- | Consume the unit and return the second argument.
-- This is like 'seq' but since the first argument is restricted to be of type
-- @()@ it is consumed, hence @seqUnit@ is linear in its first argument.
seqUnit :: () %1-> b %1-> b
seqUnit () b = b

-- | Consume the first argument and return the second argument.
-- This is like 'seq' but the first argument is restricted to be 'Consumable'.
lseq :: Consumable a => a %1-> b %1-> b
lseq a b = seqUnit (consume a) b

-- ----------------
-- Instances

-- XXX: The Consumable and Dupable instances for V will be easier to define (in
-- fact direct, we may consider adding a deriving-via combinator) when we have a
-- traversable-by-a-data-applicative class see #190.

deriving newtype instance Consumable a => Consumable (V n a)
instance Consumable a => Consumable (Vector.Vector a) where
  consume xs = consume (Unsafe.toLinear Vector.toList xs)

-- Prelude and primitive instances

deriving via Generically Prelude.Char
  instance Consumable Prelude.Char
deriving via Generically Prelude.Double
  instance Consumable Prelude.Double
deriving via Generically Prelude.Float
  instance Consumable Prelude.Float
deriving via Generically Prelude.Int
  instance Consumable Prelude.Int
deriving via Generically Prelude.Word
  instance Consumable Prelude.Word
deriving via Generically Prelude.Ordering
  instance Consumable Prelude.Ordering
deriving via Generically Prelude.Bool
  instance Consumable Prelude.Bool

deriving via Generically ()
  instance Consumable ()
deriving via Generically (Solo a)
  instance _ => Consumable (Solo a)
deriving via Generically (a, b)
  instance _ => Consumable (a, b)
deriving via Generically (a, b, c)
  instance _ => Consumable (a, b, c)
deriving via Generically (a, b, c, d)
  instance _ => Consumable (a, b, c, d)
deriving via Generically (a, b, c, d, e)
  instance _ => Consumable (a, b, c, d, e)
deriving via Generically (a, b, c, d, e, f)
  instance _ => Consumable (a, b, c, d, e, f)

deriving via Generically (Prelude.Maybe a)
  instance _ => Consumable (Prelude.Maybe a)
deriving via Generically (Prelude.Either e a)
  instance _ => Consumable (Prelude.Either e a)
deriving via Generically [a]
  instance _ => Consumable [a]
deriving via Generically (NonEmpty a)
  instance _ => Consumable (NonEmpty a)

deriving via Generically (Ur a)
  instance Consumable (Ur a)

-- Data.Semigroup instances

deriving via Generically (Semigroup.Arg a b)
  instance _ => Consumable (Semigroup.Arg a b)
deriving newtype instance _ => Consumable (Semigroup.Min a)
deriving newtype instance _ => Consumable (Semigroup.Max a)
deriving newtype instance _ => Consumable (Semigroup.First a)
deriving newtype instance _ => Consumable (Semigroup.Last a)
deriving newtype instance _ => Consumable (Semigroup.WrappedMonoid a)
deriving newtype instance _ => Consumable (Semigroup.Dual a)
deriving newtype instance Consumable Semigroup.All
deriving newtype instance Consumable Semigroup.Any
deriving newtype instance _ => Consumable (Semigroup.Sum a)
deriving newtype instance _ => Consumable (Semigroup.Product a)

-- Data.Monoid instances

deriving newtype instance _ => Consumable (Monoid.First a)
deriving newtype instance _ => Consumable (Monoid.Last a)
deriving newtype instance _ => Consumable (Monoid.Alt f a)
deriving newtype instance _ => Consumable (Monoid.Ap f a)

-- ----------------
-- Generic deriving

instance (Generic a, GConsumable (Rep a)) => Consumable (Generically a) where
  consume (Generically x) = genericConsume x

genericConsume :: (Generic a, GConsumable (Rep a)) => a %1-> ()
genericConsume = gconsume . from
{-# INLINABLE genericConsume #-}

-- | A class for generic representations that can be consumed.
class GConsumable f where
  gconsume :: f p %1-> ()
instance GConsumable V1 where
  gconsume = \case
  {-# INLINE gconsume #-}
instance GConsumable U1 where
  gconsume U1 = ()
  {-# INLINE gconsume #-}
instance (GConsumable f, GConsumable g) => GConsumable (f :+: g) where
  gconsume (L1 a) = gconsume a
  gconsume (R1 a) = gconsume a
  {-# INLINE gconsume #-}
instance (GConsumable f, GConsumable g) => GConsumable (f :*: g) where
  gconsume (a :*: b) = gconsume a `seqUnit` gconsume b
  {-# INLINE gconsume #-}
instance Consumable c => GConsumable (K1 i c) where
  gconsume (K1 c) = consume c
  {-# INLINE gconsume #-}
instance GConsumable f => GConsumable (M1 i t f) where
  gconsume (M1 a) = gconsume a
  {-# INLINE gconsume #-}

-- This split is a bit awkward. We'd like to be able to *default*
-- the multiplicity to `Many` when it's polymorphic. We'll be able
-- to do that once the Exportable Named Defaults Proposal
-- (https://github.com/ghc-proposals/ghc-proposals/pull/409#issuecomment-931839874)
-- has been implemented. The same goes for Dupable and Movable.
instance GConsumable (MP1 'Many f) where
  gconsume (MP1 _) = ()
  {-# INLINE gconsume #-}
instance GConsumable f => GConsumable (MP1 'One f) where
  gconsume (MP1 x) = gconsume x
  {-# INLINE gconsume #-}

-- Instances for unlifted generic representations
--
-- /!\ 'Char#', 'Double#', 'Float#', 'Int#', 'Word#' are unboxed data-types,
-- and therefore they cannot have any linear values hidden in a closure
-- anywhere. Therefore it is safe to call non-linear functions linearly on
-- these types. We refrain from including a 'GConsumable' instance for 'UAddr'
-- for the moment, as that seems potentially confusing—pointers usually
-- must be created, duplicated, and destroyed rather carefully. /!\

instance GConsumable UChar where
  gconsume (UChar x) = Unsafe.toLinear (\_ -> ()) x

instance GConsumable UDouble where
  gconsume (UDouble x) = Unsafe.toLinear (\_ -> ()) x

instance GConsumable UFloat where
  gconsume (UFloat x) = Unsafe.toLinear (\_ -> ()) x

instance GConsumable UInt where
  gconsume (UInt x) = Unsafe.toLinear (\_ -> ()) x

instance GConsumable UWord where
  gconsume (UWord x) = Unsafe.toLinear (\_ -> ()) x
