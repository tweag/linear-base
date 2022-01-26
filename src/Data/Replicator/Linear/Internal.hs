{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Replicator.Linear.Internal (Replicator (..), consume, map, pure, (<*>), next, next#, extract, Elim (..)) where

import Data.Arity.Linear.Internal.Arity
import Data.Kind (Constraint, Type)
import Data.Replicator.Linear.Internal.ReplicationStream (ReplicationStream (..))
import qualified Data.Replicator.Linear.Internal.ReplicationStream as ReplicationStream
import GHC.TypeLits
import Prelude.Linear.Internal

data Replicator a where
  Moved :: a -> Replicator a
  Streamed :: ReplicationStream a %1 -> Replicator a

consume :: Replicator a %1 -> ()
consume (Moved _) = ()
consume (Streamed stream) = ReplicationStream.consume stream

map :: (a %1 -> b) -> Replicator a %1 -> Replicator b
map f = \case
  Moved x -> Moved (f x)
  Streamed stream -> Streamed $ ReplicationStream.map f stream

pure :: a -> Replicator a
pure = Moved

(<*>) :: Replicator (a %1 -> b) %1 -> Replicator a %1 -> Replicator b
(Moved f) <*> (Moved x) = Moved (f x)
sf <*> sx = Streamed (toStream sf ReplicationStream.<*> toStream sx)
  where
    toStream :: Replicator a %1 -> ReplicationStream a
    toStream = \case
      Moved x -> ReplicationStream.pure x
      Streamed stream -> stream

next :: Replicator a %1 -> (a, Replicator a)
next (Moved x) = (x, Moved x)
next (Streamed (ReplicationStream s give dups consumes)) =
  dups s & \case
    (s1, s2) -> (give s1, Streamed (ReplicationStream s2 give dups consumes))

next# :: Replicator a %1 -> (# a, Replicator a #)
next# (Moved x) = (# x, Moved x #)
next# (Streamed (ReplicationStream s give dups consumes)) =
  dups s & \case
    (s1, s2) -> (# give s1, Streamed (ReplicationStream s2 give dups consumes) #)

extract :: Replicator a %1 -> a
extract (Moved x) = x
extract (Streamed (ReplicationStream s give _ _)) = give s

-- | @Elim n a b f@ asserts that @f@ is a function taking @n@ linear arguments of
-- type @a@ and then returning a value of type @b@.
type Elim :: Nat -> Type -> Type -> Type -> Constraint
class (n ~ Arity b f) => Elim n a b f | n a b -> f, f b -> n where
  elim :: f %1 -> Replicator a %1 -> b

instance Elim 0 a b b where
  elim b r =
    consume r & \case
      () -> b
  {-# INLINE elim #-}

instance (Arity b (a %1 -> b) ~ 1) => Elim 1 a b (a %1 -> b) where
  elim f r = f (extract r)
  {-# INLINE elim #-}

instance {-# OVERLAPPABLE #-} (n ~ Arity b (a %1 -> f), Elim (n - 1) a b f) => Elim n a b (a %1 -> f) where
  elim g r =
    next r & \case
      (a, r') -> elim (g a) r'
  {-# INLINE elim #-}
