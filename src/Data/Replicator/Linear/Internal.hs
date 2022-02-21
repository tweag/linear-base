{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Replicator.Linear.Internal
  ( Replicator (..),
    consume,
    duplicate,
    map,
    pure,
    (<*>),
    next,
    next#,
    take,
    extract,
    extend,
    Elim,
    elim,
  )
where

import Data.Arity.Linear.Internal
import Data.Kind (Constraint, Type)
import Data.Replicator.Linear.Internal.ReplicationStream (ReplicationStream (..))
import qualified Data.Replicator.Linear.Internal.ReplicationStream as ReplicationStream
import GHC.TypeLits
import Prelude.Linear.Internal
import Prelude ((-))
import qualified Prelude

-- | 'Replicator' is a stream-like data structure used to linearly duplicate
-- values.
data Replicator a where
  Moved :: a -> Replicator a
  Streamed :: ReplicationStream a %1 -> Replicator a

consume :: Replicator a %1 -> ()
consume (Moved _) = ()
consume (Streamed stream) = ReplicationStream.consume stream
{-# INLINEABLE consume #-}

duplicate :: Replicator a %1 -> Replicator (Replicator a)
duplicate = \case
  Moved x -> Moved (Moved x)
  Streamed stream -> Streamed $ ReplicationStream.map Streamed (ReplicationStream.duplicate stream)

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

infixl 4 <*> -- same fixity as base.<*>

-- | Extracts the next item from the \"infinite stream\" @'Replicator' a@.
next :: Replicator a %1 -> (a, Replicator a)
next (Moved x) = (x, Moved x)
next (Streamed (ReplicationStream s give dups consumes)) =
  dups s & \case
    (s1, s2) -> (give s1, Streamed (ReplicationStream s2 give dups consumes))
{-# INLINEABLE next #-}

-- | Extracts the next item from the \"infinite stream\" @'Replicator' a@.
-- Same function as 'next', but returning an unboxed tuple.
next# :: Replicator a %1 -> (# a, Replicator a #)
next# (Moved x) = (# x, Moved x #)
next# (Streamed (ReplicationStream s give dups consumes)) =
  dups s & \case
    (s1, s2) -> (# give s1, Streamed (ReplicationStream s2 give dups consumes) #)
{-# INLINEABLE next# #-}

-- | @'take' n as@ is a list of size @n@, containing @n@ replicas from @as@.
take :: Prelude.Int -> Replicator a %1 -> [a]
take 0 r =
  consume r & \case
    () -> []
take 1 r = [extract r]
take n r =
  next r & \case
    (a, r') -> a : take (n - 1) r'

-- | Returns the next item from @'Replicator' a@ and efficiently consumes
-- the replicator at the same time.
extract :: Replicator a %1 -> a
extract (Moved x) = x
extract (Streamed (ReplicationStream s give _ _)) = give s
{-# INLINEABLE extract #-}

-- | Comonadic 'extend' function.
--
-- > extend f = map f . duplicate
extend :: (Replicator a %1 -> b) -> Replicator a %1 -> Replicator b
extend f = map f . duplicate

-- | Takes a function of type @a %1 -> a %1 -> ... %1 -> a %1 -> b@, and
-- returns a @b@ . The replicator is used to supply all the items of type @a@
-- required by the function.
--
-- For instance:
--
-- > elim @1 :: (a %1 -> b) %1 -> Replicator a %1 -> b
-- > elim @2 :: (a %1 -> a %1 -> b) %1 -> Replicator a %1 -> b
-- > elim @3 :: (a %1 -> a %1 -> a %1 -> b) %1 -> Replicator a %1 -> b
--
-- It is not always necessary to give the arity argument. It can be
-- inferred from the function argument.
--
-- > elim (,) :: Replicator a %1 -> (a, a)
-- > elim (,,) :: Replicator a %1 -> (a, a, a)
elim :: forall (n :: Nat) a b f. (Elim (NatToPeano n) a b, IsFunN a b f, f ~ FunN (NatToPeano n) a b, n ~ Arity b f) => f %1 -> Replicator a %1 -> b
elim f r = elim' @(NatToPeano n) f r

-- | @'Elim' n a b f@ asserts that @f@ is a function taking @n@ linear arguments
-- of type @a@ and then returning a value of type @b@.
--
-- It is solely used to define the type of the 'elim' function.
type Elim :: Peano -> Type -> Type -> Constraint
class Elim n a b where
  -- Note that 'elim' is, in particular, used to force eta-expansion of
  -- 'elim\''.  Otherwise, 'elim\'' might not get inlined (see
  -- https://github.com/tweag/linear-base/issues/369).
  elim' :: FunN n a b %1 -> Replicator a %1 -> b

instance Elim 'Z a b where
  elim' b r =
    consume r & \case
      () -> b
  {-# INLINE elim' #-}

instance Elim ('S 'Z) a b where
  elim' f r = f (extract r)
  {-# INLINE elim' #-}

instance (Elim ('S n) a b) => Elim ('S ('S n)) a b where
  elim' g r =
    next r & \case
      (a, r') -> elim' @('S n) (g a) r'
  {-# INLINE elim' #-}
