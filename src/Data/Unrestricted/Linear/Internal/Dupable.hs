{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Unrestricted.Linear.Internal.Dupable
  ( Dupable (..),
    genericDupR,
    dup,
    dup3,
    dup4,
    dup5,
    dup6,
    dup7,
    GDupable,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Replicator.Linear.Internal (Replicator (..))
import qualified Data.Replicator.Linear.Internal as Replicator
import Data.Replicator.Linear.Internal.ReplicationStream (ReplicationStream (..))
import qualified Data.Replicator.Linear.Internal.ReplicationStream as ReplicationStream
import qualified Data.Semigroup as Semigroup
import Data.Unrestricted.Linear.Internal.Consumable
import Data.Unrestricted.Linear.Internal.Ur (Ur)
import GHC.Tuple (Solo (..))
import GHC.Types (Multiplicity (..))
import Generics.Linear
import Prelude.Linear.Generically
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import qualified Prelude

-- | The laws of 'Dupable' are dual to those of 'Monoid':
--
-- * 1. @first consume (dup2 a) ≃ a ≃ second consume (dup2 a)@ ('dup2' neutrality)
-- * 2. @first dup2 (dup2 a) ≃ (second dup2 (dup2 a))@ ('dup2' associativity)
--
-- where the @(≃)@ sign represents equality up to type isomorphism.
--
-- * 3. @dup2 = Replicator.elim (,) . dupR@ (coherence between 'dup2' and 'dupR')
-- * 4. @consume = Replicator.elim () . dupR@ (coherence between 'consume' and 'dupR')
--
-- * 5. @Replicator.extract . dupR = id@ ('dupR' identity)
-- * 6. @dupR . dupR = (Replicator.map dupR) . dupR@ ('dupR' interchange)
--
-- (Laws 1-2 and 5-6 are equivalent)
--
-- Implementation of 'Dupable' for 'Data.Unrestricted.Movable' types should
-- be done with @deriving via 'Data.Unrestricted.AsMovable'@.
--
-- Implementation of 'Dupable' for other types can be done with
-- @deriving via 'Generically'@. Note that at present this mechanism
-- can have performance problems for recursive parameterized types.
-- Specifically, the methods will not specialize to underlying 'Dupable'
-- instances. See [this GHC issue](https://gitlab.haskell.org/ghc/ghc/-/issues/21131).
class Consumable a => Dupable a where
  {-# MINIMAL dupR | dup2 #-}

  -- | Creates a 'Replicator' for the given @a@.
  --
  -- You usually want to define this method using 'Replicator'\'s
  -- 'Data.Functor.Linear.Applicative' instance. For instance, here is an
  -- implementation of @'Dupable' [a]@:
  --
  -- > instance Dupable a => Dupable [a] where
  -- >   dupR [] = pure []
  -- >   dupR (a : as) = (:) <$> dupR a <*> dupR as
  dupR :: a %1 -> Replicator a
  dupR x = Streamed $ ReplicationStream x id dup2 consume

  -- | Creates two @a@s from a @'Dupable' a@, in a linear fashion.
  dup2 :: a %1 -> (a, a)
  dup2 x = Replicator.elim (,) (dupR x)

-- | Creates 3 @a@s from a @'Dupable' a@, in a linear fashion.
dup3 :: Dupable a => a %1 -> (a, a, a)
dup3 x = Replicator.elim (,,) (dupR x)

-- | Creates 4 @a@s from a @'Dupable' a@, in a linear fashion.
dup4 :: Dupable a => a %1 -> (a, a, a, a)
dup4 x = Replicator.elim (,,,) (dupR x)

-- | Creates 5 @a@s from a @'Dupable' a@, in a linear fashion.
dup5 :: Dupable a => a %1 -> (a, a, a, a, a)
dup5 x = Replicator.elim (,,,,) (dupR x)

-- | Creates 6 @a@s from a @'Dupable' a@, in a linear fashion.
dup6 :: Dupable a => a %1 -> (a, a, a, a, a, a)
dup6 x = Replicator.elim (,,,,,) (dupR x)

-- | Creates 7 @a@s from a @'Dupable' a@, in a linear fashion.
dup7 :: Dupable a => a %1 -> (a, a, a, a, a, a, a)
dup7 x = Replicator.elim (,,,,,,) (dupR x)

-- | Creates two @a@s from a @'Dupable' a@. Same function as 'dup2'.
dup :: Dupable a => a %1 -> (a, a)
dup = dup2

------------
-- Instances
------------

instance Dupable (ReplicationStream a) where
  dupR = Streamed . ReplicationStream.duplicate

instance Dupable (Replicator a) where
  dupR = Replicator.duplicate

deriving via
  Generically Prelude.Bool
  instance
    Dupable Prelude.Bool

deriving via
  Generically Prelude.Int
  instance
    Dupable Prelude.Int

deriving via
  Generically Prelude.Word
  instance
    Dupable Prelude.Word

deriving via
  Generically Prelude.Ordering
  instance
    Dupable Prelude.Ordering

deriving via
  Generically Prelude.Char
  instance
    Dupable Prelude.Char

deriving via
  Generically Prelude.Double
  instance
    Dupable Prelude.Double

deriving via
  Generically Prelude.Float
  instance
    Dupable Prelude.Float

deriving via
  Generically (Prelude.Maybe a)
  instance
    Dupable a => Dupable (Prelude.Maybe a)

deriving via
  Generically (Prelude.Either a b)
  instance
    (Dupable a, Dupable b) => Dupable (Prelude.Either a b)

-- This instance is written manually because I (David Feuer) haven't
-- been able to find a way to get the generic version to specialize
-- to a particular underlying Dupable. The recursion leads to the
-- whole thing being a loop breaker and I don't know how to fix that.
instance Dupable a => Dupable [a] where
  dupR = go
    where
      go :: [a] %1 -> Replicator [a]
      go [] = Replicator.pure []
      go (x : xs) = Replicator.liftA2 (:) (dupR x) (go xs)

deriving via
  Generically (NonEmpty a)
  instance
    Dupable a => Dupable (NonEmpty a)

deriving via
  Generically (Ur a)
  instance
    Dupable (Ur a)

deriving via
  Generically ()
  instance
    Dupable ()

deriving via
  Generically (Solo a)
  instance
    Dupable a => Dupable (Solo a)

deriving via
  Generically (a, b)
  instance
    (Dupable a, Dupable b) => Dupable (a, b)

deriving via
  Generically (a, b, c)
  instance
    (Dupable a, Dupable b, Dupable c) => Dupable (a, b, c)

deriving via
  Generically (a, b, c, d)
  instance
    (Dupable a, Dupable b, Dupable c, Dupable d) => Dupable (a, b, c, d)

deriving via
  Generically (a, b, c, d, e)
  instance
    (Dupable a, Dupable b, Dupable c, Dupable d, Dupable e) => Dupable (a, b, c, d, e)

deriving newtype instance Dupable a => Dupable (Semigroup.Sum a)

deriving newtype instance Dupable a => Dupable (Semigroup.Product a)

deriving newtype instance Dupable Semigroup.All

deriving newtype instance Dupable Semigroup.Any

-------------------
-- Generic deriving
-------------------

instance (Generic a, GDupable (Rep a)) => Dupable (Generically a) where
  dupR (Generically x) = lcoerce (Replicator.map (to :: Rep a x %1 -> a) (gdupR (from x)))

genericDupR :: (Generic a, GDupable (Rep a)) => a %1 -> Replicator a
genericDupR x = Replicator.map to (gdupR (from x))

class GConsumable f => GDupable f where
  gdupR :: f a %1 -> Replicator (f a)

instance GDupable f => GDupable (M1 i c f) where
  gdupR (M1 x) = lcoerce (gdupR x)
  {-# INLINE gdupR #-}

instance (GDupable f, GDupable g) => GDupable (f :*: g) where
  gdupR (x :*: y) = Replicator.liftA2 (:*:) (gdupR x) (gdupR y)
  {-# INLINE gdupR #-}

instance (GDupable f, GDupable g) => GDupable (f :+: g) where
  gdupR (L1 x) = Replicator.map L1 (gdupR x)
  gdupR (R1 y) = Replicator.map R1 (gdupR y)
  {-# INLINE gdupR #-}

instance Dupable c => GDupable (K1 i c) where
  gdupR = lcoerce (dupR @c)
  {-# INLINE gdupR #-}

instance GDupable U1 where
  gdupR U1 = Replicator.pure U1
  {-# INLINE gdupR #-}

instance GDupable V1 where
  gdupR = \case {}
  {-# INLINE gdupR #-}

instance GDupable (MP1 'Many f) where
  gdupR (MP1 x) = Replicator.pure (MP1 x)
  {-# INLINE gdupR #-}

instance GDupable f => GDupable (MP1 'One f) where
  gdupR (MP1 x) = Replicator.map MP1 (gdupR x)
  {-# INLINE gdupR #-}

instance GDupable UChar where
  gdupR = Unsafe.toLinear Replicator.pure

instance GDupable UDouble where
  gdupR = Unsafe.toLinear Replicator.pure

instance GDupable UFloat where
  gdupR = Unsafe.toLinear Replicator.pure

instance GDupable UInt where
  gdupR = Unsafe.toLinear Replicator.pure

instance GDupable UWord where
  gdupR = Unsafe.toLinear Replicator.pure
