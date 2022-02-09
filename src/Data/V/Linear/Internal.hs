{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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

module Data.V.Linear.Internal
  ( V (..),
    empty,
    consume,
    map,
    pure,
    (<*>),
    uncons#,
    uncons,
    Elim,
    elim,
    cons,
    fromReplicator,
    dupV,
    theLength,
    Make,
    make,
  )
where

import Data.Arity.Linear.Internal.Arity
import Data.Kind
import Data.Replicator.Linear.Internal (Replicator)
import qualified Data.Replicator.Linear.Internal as Replicator
import Data.Unrestricted.Internal.Dupable (Dupable (dupR))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts (proxy#)
import GHC.TypeLits
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import qualified Prelude

-- | @'V' n a@ represents an immutable sequence of @n@ elements of type @a@
-- (like a n-tuple), with a linear 'Data.Functor.Linear.Applicative' instance.
newtype V (n :: Nat) (a :: Type) = V (Vector a)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Functor)

-- Using vector rather than, say, 'Array' (or directly 'Array#') because it
-- offers many convenience function. Since all these unsafeCoerces probably
-- kill the fusion rules, it may be worth it going lower level since I
-- probably have to write my own fusion anyway. Therefore, starting from
-- Vectors at the moment.

-- | Returns an empty 'V'.
empty :: forall a. V 0 a
empty = V Vector.empty

consume :: V 0 a %1 -> ()
consume = Unsafe.toLinear (\_ -> ())
{-# INLINEABLE consume #-}

map :: (a %1 -> b) -> V n a %1 -> V n b
map f (V xs) = V $ Unsafe.toLinear (Vector.map (\x -> f x)) xs

(<*>) :: V n (a %1 -> b) %1 -> V n a %1 -> V n b
(V fs) <*> (V xs) =
  V $
    Unsafe.toLinear2 (Vector.zipWith (\f x -> f $ x)) fs xs

-- | Splits the head and tail of the 'V', returning an unboxed tuple.
uncons# :: 1 <= n => V n a %1 -> (# a, V (n - 1) a #)
uncons# = Unsafe.toLinear uncons'#
  where
    uncons'# :: 1 <= n => V n a -> (# a, V (n - 1) a #)
    uncons'# (V xs) = (# Vector.head xs, V (Vector.tail xs) #)
{-# INLINEABLE uncons# #-}

-- | Splits the head and tail of the 'V', returning a boxed tuple.
uncons :: 1 <= n => V n a %1 -> (a, V (n - 1) a)
uncons = Unsafe.toLinear uncons'
  where
    uncons' :: 1 <= n => V n a -> (a, V (n - 1) a)
    uncons' (V xs) = (Vector.head xs, V (Vector.tail xs))
{-# INLINEABLE uncons #-}

-- | @'Elim' n a b f@ asserts that @f@ is a function taking @n@ linear arguments
-- of type @a@ and then returning a value of type @b@.
--
-- It is solely used to define the type of the 'elim' function.
type Elim :: Nat -> Type -> Type -> Type -> Constraint
class (n ~ Arity b f) => Elim n a b f | n a b -> f, f b -> n where
  -- | Takes a function of type @a %1 -> a %1 -> ... %1 -> a %1 -> b@, and
  -- returns a @b@ . The 'V' is used to supply all the items of type @a@
  -- required by the function.
  --
  -- For instance:
  --
  -- > elim @1 :: (a %1 -> b) %1 -> V 1 a %1 -> b
  -- > elim @2 :: (a %1 -> a %1 -> b) %1 -> V 2 a %1 -> b
  -- > elim @3 :: (a %1 -> a %1 -> a %1 -> b) %1 -> V 3 a %1 -> b
  elim :: f %1 -> V n a %1 -> b

instance Elim 0 a b b where
  elim b v =
    consume v & \case
      () -> b
  {-# INLINE elim #-}

instance (1 <= n, n ~ Arity b (a %1 -> f), Elim (n - 1) a b f) => Elim n a b (a %1 -> f) where
  elim g v =
    uncons v & \case
      (a, v') -> elim (g a) v'
  {-# INLINE elim #-}

-- | Prepends the given element to the 'V'.
cons :: forall n a. a %1 -> V (n - 1) a %1 -> V n a
cons = Unsafe.toLinear2 $ \x (V v) -> V (Vector.cons x v)

-- | Builds a n-ary constructor for @'V' n a@ (i.e. a function taking @n@ linear
-- arguments of type @a@ and returning a @'V' n a@).
--
-- > myV :: V 3 Int
-- > myV = make 1 2 3
make :: forall n a f. Make n n a f => f
make = make' @n @n id
{-# INLINE make #-}

-- | @'Make' m n a f@ asserts that @f@ is a function taking @m@ linear arguments
-- of type @a@ and then returning a value of type @'V' n a@.
--
-- It is solely used to define the type of the 'make' function.
type Make :: Nat -> Nat -> Type -> Type -> Constraint
class (m ~ Arity (V n a) f) => Make m n a f | f -> m n a where
  -- The idea behind Make / make' / make is the following:
  --
  -- f takes m values of type a, but returns a 'V n a' (with n ≥ m),
  -- so the n - m missing values must be supplied via the accumulator.
  --
  -- @make' is initially called with m = n via make, and as m decreases,
  -- the number of lambda on the left increases and the captured values are put
  -- in the accumulator
  -- ('V[ ... ] <> ' represents the "extend" operation for 'V'):
  --
  -- >     make @n
  -- > --> make' @n @n (V[] <>)
  -- > --> λx. make' @(n - 1) @n (V[x] <>)
  -- > --> λx. λy. make' @(n - 2) @n (V[x, y] <>)
  -- > --> ...
  -- > --> λx. λy. ... λz. make' @0 @n (V[x, y, ... z] <>)    -- make' @0 @n f = f V[]
  -- > --> λx. λy. ... λz. V[x, y, ... z]
  make' :: (V m a %1 -> V n a) %1 -> f

instance Make 0 n a (V n a) where
  make' produceFrom = produceFrom empty
  {-# INLINE make' #-}

instance (m ~ Arity (V n a) (a %1 -> f), Make (m - 1) n a f) => Make m n a (a %1 -> f) where
  make' produceFrom = \x -> make' @(m - 1) @n @a (\s -> produceFrom $ cons x s)
  {-# INLINE make' #-}

-------------------------------------------------------------------------------
-- Functions below use AllowAmbiguousTypes
-------------------------------------------------------------------------------

-- | Returns the type-level 'Nat' of the context as a term-level integer.
theLength :: forall n. KnownNat n => Prelude.Int
theLength = Prelude.fromIntegral (natVal' @n (proxy# @_))

pure :: forall n a. KnownNat n => a -> V n a
pure a = V $ Vector.replicate (theLength @n) a

-- | Creates a 'V' of the specified size by consuming a 'Replicator'.
fromReplicator :: forall n a. KnownNat n => Replicator a %1 -> V n a
fromReplicator = let n' = theLength @n in V . Unsafe.toLinear Vector.fromList . Replicator.take n'

-- | Produces a @'V' n a@ from a 'Dupable' value @a@.
dupV :: forall n a. (KnownNat n, Dupable a) => a %1 -> V n a
dupV = fromReplicator . dupR
