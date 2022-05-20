{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    ArityV,
  )
where

import Data.Arity.Linear.Internal
import Data.Kind
import Data.Replicator.Linear.Internal (Replicator)
import qualified Data.Replicator.Linear.Internal as Replicator
import Data.Unrestricted.Linear.Internal.Dupable (Dupable (dupR))
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
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, Prelude.Foldable, Prelude.Functor, Prelude.Traversable)

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

infixl 4 <*> -- same fixity as base.<*>

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

-- | Takes a function of type @a %1 -> a %1 -> ... %1 -> a %1 -> b@, and
-- returns a @b@ . The @'V' n a@ is used to supply all the items of type @a@
-- required by the function.
--
-- For instance:
--
-- > elim @1 :: (a %1 -> b) %1 -> V 1 a %1 -> b
-- > elim @2 :: (a %1 -> a %1 -> b) %1 -> V 2 a %1 -> b
-- > elim @3 :: (a %1 -> a %1 -> a %1 -> b) %1 -> V 3 a %1 -> b
--
-- It is not always necessary to give the arity argument. It can be
-- inferred from the function argument.
--
-- About the constraints of this function (they won't get in your way):
--
-- * @n ~ 'PeanoToNat' ('NatToPeano' n)@ is just there to help GHC, and will always be proved
-- * @'Elim' ('NatToPeano' n) a b@ provides the actual implementation of 'elim'; there is an instance of this class for any @(n, a, b)@
-- * @'IsFunN' a b f, f ~ 'FunN' ('NatToPeano' n) a b, n ~ 'Arity' b f@ indicate the shape of @f@ to the typechecker (see documentation of 'IsFunN').
elim ::
  forall (n :: Nat) a b f.
  ( -- GHC cannot prove it for any @n@, but can prove it at call site when
    -- @n@ is known
    n ~ PeanoToNat (NatToPeano n),
    Elim (NatToPeano n) a b,
    IsFunN a b f,
    f ~ FunN (NatToPeano n) a b,
    n ~ Arity b f
  ) =>
  f %1 ->
  V n a %1 ->
  b
elim f v = elim' @(NatToPeano n) f v

-- | @'Elim' n a b@ is used to implement 'elim' without recursion
-- so that we can guarantee that 'elim' will be inlined and unrolled.
--
-- 'Elim' is solely used in the signature of 'elim'.
type Elim :: Peano -> Type -> Type -> Constraint
class Elim n a b where
  -- Note that 'elim' is, in particular, used to force eta-expansion of
  -- 'elim\''. Otherwise, 'elim\'' might not get inlined (see
  -- https://github.com/tweag/linear-base/issues/369).
  elim' :: FunN n a b %1 -> V (PeanoToNat n) a %1 -> b

instance Elim 'Z a b where
  elim' b v =
    consume v & \case
      () -> b
  {-# INLINE elim' #-}

instance (1 <= 1 + PeanoToNat n, (1 + PeanoToNat n) - 1 ~ PeanoToNat n, Elim n a b) => Elim ('S n) a b where
  elim' g v =
    uncons v & \case
      (a, v') -> elim' @n (g a) v'
  {-# INLINE elim' #-}

-- | Prepends the given element to the 'V'.
cons :: forall n a. a %1 -> V (n - 1) a %1 -> V n a
cons = Unsafe.toLinear2 $ \x (V v) -> V (Vector.cons x v)

-- | The 'ArityV' type family exists to help the type checker compute the arity
-- @n ~ 'Arity' b f@ when @b ~ 'V' n a@.
type family ArityV f where
  ArityV (V _ _) = 0
  ArityV (a %1 -> f) = 1 + ArityV f
  ArityV f =
    TypeError
      ( 'Text "Arity: "
          ':<>: 'ShowType f
          ':<>: 'Text " isn't a linear function with head (V _ _)"
      )

-- | Builds a n-ary constructor for @'V' n a@ (i.e. a function taking @n@ linear
-- arguments of type @a@ and returning a @'V' n a@).
--
-- > myV :: V 3 Int
-- > myV = make 1 2 3
--
-- About the constraints of this function (they won't get in your way):
--
-- * @n ~ 'PeanoToNat' ('NatToPeano' n)@ is just there to help GHC, and will always be proved
-- * @'Make' ('NatToPeano' n) ('NatToPeano' n) a@ provides the actual implementation of 'make'; there is an instance of this class for any @(n, a)@
-- * @'IsFunN' a ('V' n a) f, f ~ 'FunN' ('NatToPeano' n) a ('V' n a), n ~ 'ArityV' f@ indicate the shape to the typechecker of @f@ (see documentation of 'IsFunN').
make ::
  forall (n :: Nat) a f.
  ( -- GHC cannot prove it for any @n@, but can prove it at call site when
    -- @n@ is known
    n ~ PeanoToNat (NatToPeano n),
    Make (NatToPeano n) (NatToPeano n) a,
    IsFunN a (V n a) f,
    f ~ FunN (NatToPeano n) a (V n a),
    n ~ ArityV f
  ) =>
  f
make = make' @(NatToPeano n) @(NatToPeano n) @a id
{-# INLINE make #-}

-- | @'Make' m n a@ is used to avoid recursion in the implementation of 'make'
-- so that 'make' can be inlined.
--
-- 'Make' is solely used in the signature of that function.
type Make :: Peano -> Peano -> Type -> Constraint
class Make m n a where
  -- The idea behind Make / make' / make is the following:
  --
  -- The function created by make' takes m values of type a, but returns a 'V n a' (with n ≥ m),
  -- so the n - m missing values must be supplied via the accumulator.
  --
  -- make' is initially called with m = n via make, and as m decreases,
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
  make' :: (V (PeanoToNat m) a %1 -> V (PeanoToNat n) a) %1 -> FunN m a (V (PeanoToNat n) a)

instance Make 'Z n a where
  make' produceFrom = produceFrom empty
  {-# INLINE make' #-}

instance ((1 + PeanoToNat m) - 1 ~ PeanoToNat m, Make m n a) => Make ('S m) n a where
  make' produceFrom = \x -> make' @m @n @a (\s -> produceFrom $ cons x s)
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
