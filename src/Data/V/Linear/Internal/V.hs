{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.V.Linear.Internal.V
  ( V (..),
    consume,
    -- consumeV,
    fmap,
    pure,
    (<*>),
    next#,
    next,
    -- split,
    Elim (..),
    cons,
    -- fromReplicator,
    -- dupV,
  )
where

import Data.Arity.Linear.Internal.Arity
import Data.Kind
-- import Data.Replicator.Linear.Internal.Replicator (Replicator)
-- import qualified Data.Replicator.Linear.Internal.Replicator as Replicator
import Data.V.Linear.Internal.Ambiguous
  ( theLength,
  -- make
  )
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.TypeLits
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import qualified Prelude

-- import Data.Unrestricted.Internal.Dupable (Dupable (dupR))

{- Developers Note

See the "Developers Note" in Data.V.Linear for an explanation of this module
structure.

-}

-- # Type Definitions
-------------------------------------------------------------------------------

newtype V (n :: Nat) (a :: Type) = V (Vector a)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Functor)

-- Using vector rather than, say, 'Array' (or directly 'Array#') because it
-- offers many convenience function. Since all these unsafeCoerces probably
-- kill the fusion rules, it may be worth it going lower level since I
-- probably have to write my own fusion anyway. Therefore, starting from
-- Vectors at the moment.

-- # API
-------------------------------------------------------------------------------

consume :: V 0 a %1 -> ()
consume = Unsafe.toLinear (\_ -> ())

-- Backward compatility
-- consumeV :: V 0 a %1 -> b %1 -> b
-- consumeV v b = consume v & \case
--   () -> b

fmap :: (a %1 -> b) -> V n a %1 -> V n b
fmap f (V xs) = V $ Unsafe.toLinear (Vector.map (\x -> f x)) xs

pure :: forall n a. KnownNat n => a -> V n a
pure a = V $ Vector.replicate (theLength @n) a

(<*>) :: V n (a %1 -> b) %1 -> V n a %1 -> V n b
(V fs) <*> (V xs) =
  V $
    Unsafe.toLinear2 (Vector.zipWith (\f x -> f $ x)) fs xs

next# :: 1 <= n => V n a %1 -> (# a, V (n - 1) a #)
next# = Unsafe.toLinear next'#
  where
    next'# :: 1 <= n => V n a -> (# a, V (n - 1) a #)
    next'# (V xs) = (# Vector.head xs, V (Vector.tail xs) #)

next :: 1 <= n => V n a %1 -> (a, V (n - 1) a)
next = Unsafe.toLinear next'
  where
    next' :: 1 <= n => V n a -> (a, V (n - 1) a)
    next' (V xs) = (Vector.head xs, V (Vector.tail xs))

-- Backward compatility
-- split :: 1 <= n => V n a %1 -> (# a, V (n - 1) a #)
-- split = next#

type Elim :: Nat -> Type -> Type -> Type -> Constraint
class (n ~ Arity b f) => Elim n a b f | n a b -> f, f b -> n where
  elim :: f %1 -> V n a %1 -> b

instance Elim 0 a b b where
  elim b v =
    consume v & \case
      () -> b
  {-# INLINE elim #-}

instance (1 <= n, n ~ Arity b (a %1 -> f), Elim (n - 1) a b f) => Elim n a b (a %1 -> f) where
  elim g v =
    next v & \case
      (a, v') -> elim (g a) v'
  {-# INLINE elim #-}

cons :: forall n a. a %1 -> V (n - 1) a %1 -> V n a
cons = Unsafe.toLinear2 $ \x (V v) -> V (Vector.cons x v)

-- TODO: fails because Elim and make are not dual with the current implementation
-- fromReplicator :: forall n a. KnownNat n => Replicator a %1 -> V n a
-- fromReplicator = Replicator.elim (make @n)

-- dupV :: forall n a. (KnownNat n, Dupable a) => a %1 -> V n a
-- dupV = fromReplicator . dupR
