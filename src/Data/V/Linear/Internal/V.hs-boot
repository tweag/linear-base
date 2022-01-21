{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

import Data.Arity.Linear.Internal.Arity (Arity)
import Data.Kind
import Data.Vector (Vector)
import GHC.TypeLits

newtype V (n :: Nat) (a :: Type) = V (Vector a)

consume :: V 0 a %1 -> ()
-- consumeV :: V 0 a %1 -> b %1 -> b
fmap :: (a %1 -> b) -> V n a %1 -> V n b
pure :: forall n a. KnownNat n => a -> V n a
(<*>) :: V n (a %1 -> b) %1 -> V n a %1 -> V n b
next# :: 1 <= n => V n a %1 -> (# a, V (n - 1) a #)
next :: 1 <= n => V n a %1 -> (a, V (n - 1) a)

-- split :: 1 <= n => V n a %1 -> (# a, V (n - 1) a #)
type Elim :: Nat -> Type -> Type -> Type -> Constraint
class (n ~ Arity b f) => Elim n a b f | n a b -> f, f b -> n where
  elim :: f %1 -> V n a %1 -> b

cons :: forall n a. a %1 -> V (n - 1) a %1 -> V n a

-- fromReplicator :: forall n a. KnownNat n => Replicator a %1 -> V n a
-- dupV :: forall n a. (KnownNat n, Dupable a) => a %1 -> V n a
