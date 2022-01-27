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

-- | @Replicator@ is used to linearly duplicate values in an efficient fashion.
-- @Movable@ types are deep-copied only once (no matter how many copies are
-- required), whereas non-@Movable@ values are duplicated using the internal
-- construct @ReplicationStream@. @Replicator@ also have a linear
-- @Data.Applicative@ instance.
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

-- | Extract one more copy of @a@ from the infinite "stream" @Replicator a@.
next :: Replicator a %1 -> (a, Replicator a)
next (Moved x) = (x, Moved x)
next (Streamed (ReplicationStream s give dups consumes)) =
  dups s & \case
    (s1, s2) -> (give s1, Streamed (ReplicationStream s2 give dups consumes))

-- | Extract one more copy of @a@ from the infinite "stream" @Replicator a@.
-- Same function as @next@, but returning an unboxed tuple.
next# :: Replicator a %1 -> (# a, Replicator a #)
next# (Moved x) = (# x, Moved x #)
next# (Streamed (ReplicationStream s give dups consumes)) =
  dups s & \case
    (s1, s2) -> (# give s1, Streamed (ReplicationStream s2 give dups consumes) #)

-- | Returns one more copy of @a@ while efficiently consuming the replicator
-- at the same time.
extract :: Replicator a %1 -> a
extract (Moved x) = x
extract (Streamed (ReplicationStream s give _ _)) = give s

-- | @Elim n a b f@ asserts that @f@ is a function taking @n@ linear arguments
-- of type @a@ and then returning a value of type @b@.
type Elim :: Nat -> Type -> Type -> Type -> Constraint
class (n ~ Arity b f) => Elim n a b f | n a b -> f, f b -> n where
  -- | Takes a function of type @a %1 -> a %1 -> ... %1 -> a %1 -> b@, and
  -- returns a @b@ . The replicator is used to supply all the copies of @a@
  -- required by the function.
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
