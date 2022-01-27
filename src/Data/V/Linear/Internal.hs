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
    consume,
    fmap,
    pure,
    (<*>),
    uncons#,
    uncons,
    elim,
    cons,
    fromReplicator,
    dupV,
    theLength,
    make,
    FunN,
  )
where

import Data.Arity.Linear.Internal.Arity
import Data.Kind
import Data.Proxy
import Data.Replicator.Linear.Internal (Replicator)
import qualified Data.Replicator.Linear.Internal as Replicator
import Data.Type.Equality
import Data.Unrestricted.Internal.Dupable (Dupable (dupR))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts (proxy#)
import GHC.TypeLits
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import Prelude (Bool (..), Either (..), Maybe (..), error, (-))
import qualified Prelude

-- | @V n a@ represents an immutable sequence of @n@ elements of type @a@
-- (like a n-tuple), with a linear @Data.Applicative@ instance.
newtype V (n :: Nat) (a :: Type) = V (Vector a)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Functor)

-- Using vector rather than, say, 'Array' (or directly 'Array#') because it
-- offers many convenience function. Since all these unsafeCoerces probably
-- kill the fusion rules, it may be worth it going lower level since I
-- probably have to write my own fusion anyway. Therefore, starting from
-- Vectors at the moment.

consume :: V 0 a %1 -> ()
consume = Unsafe.toLinear (\_ -> ())

fmap :: (a %1 -> b) -> V n a %1 -> V n b
fmap f (V xs) = V $ Unsafe.toLinear (Vector.map (\x -> f x)) xs

pure :: forall n a. KnownNat n => a -> V n a
pure a = V $ Vector.replicate (theLength @n) a

(<*>) :: V n (a %1 -> b) %1 -> V n a %1 -> V n b
(V fs) <*> (V xs) =
  V $
    Unsafe.toLinear2 (Vector.zipWith (\f x -> f $ x)) fs xs

-- | Splits the head and tail of the @V@, returning an unboxed tuple.
uncons# :: 1 <= n => V n a %1 -> (# a, V (n - 1) a #)
uncons# = Unsafe.toLinear uncons'#
  where
    uncons'# :: 1 <= n => V n a -> (# a, V (n - 1) a #)
    uncons'# (V xs) = (# Vector.head xs, V (Vector.tail xs) #)

-- | Splits the head and tail of the @V@, returning a boxed tuple.
uncons :: 1 <= n => V n a %1 -> (a, V (n - 1) a)
uncons = Unsafe.toLinear uncons'
  where
    uncons' :: 1 <= n => V n a -> (a, V (n - 1) a)
    uncons' (V xs) = (Vector.head xs, V (Vector.tail xs))

-- | @Elim n a b f@ asserts that @f@ is a function taking @n@ linear arguments
-- of type @a@ and then returning a value of type @b@.
type Elim :: Nat -> Type -> Type -> Type -> Constraint
class (n ~ Arity b f) => Elim n a b f | n a b -> f, f b -> n where
  -- | Takes a function of type @a %1 -> a %1 -> ... %1 -> a %1 -> b@, and
  -- returns a @b@ . The @V@ is used to supply all the copies of @a@
  -- required by the function (the arity of the specified function must
  -- match the @V@ size).
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

-- | Prepends the given element to the @V@.
cons :: forall n a. a %1 -> V (n - 1) a %1 -> V n a
cons = Unsafe.toLinear2 $ \x (V v) -> V (Vector.cons x v)

-- | Creates a @V@ of the specified size by consuming a @Replicator@.
fromReplicator :: forall n a. (KnownNat n, Replicator.Elim n a (V n a) (FunN n a (V n a))) => Replicator a %1 -> V n a
fromReplicator = Replicator.elim @n @a @(V n a) @(FunN n a (V n a)) (make @n @a)

-- | Produces a @V n a@ for a @Dupable@ type @a@.
dupV :: forall n a. (KnownNat n, Dupable a, Replicator.Elim n a (V n a) (FunN n a (V n a))) => a %1 -> V n a
dupV = fromReplicator . dupR

-------------------------------------------------------------------------------
-- Functions below use AllowAmbiguousTypes
-------------------------------------------------------------------------------

-- | Returns the type-level Nat of the context as a term-level integer.
theLength :: forall n. KnownNat n => Prelude.Int
theLength = Prelude.fromIntegral (natVal' @n (proxy# @_))

-- Make implementation, which needs to be improved

-- | Builds a n-ary constructor for @V n a@ (i.e. a function taking @n@
-- elements of type @a@ and returning a @V n a@).
make :: forall n a. KnownNat n => FunN n a (V n a)
make = case caseNat @n of
  Left Refl -> V Vector.empty
  Right Refl -> contractFunN @n @a @(V n a) prepend
    where
      prepend :: a %1 -> FunN (n - 1) a (V n a)
      prepend t = case predNat @n of
        Dict -> continue @(n - 1) @a @(V (n - 1) a) (cons t) (make @(n - 1) @a)

-- Helper functions/types for 'make' to typecheck

data Dict (c :: Constraint) where
  Dict :: c => Dict c

type family FunN (n :: Nat) (a :: Type) (b :: Type) :: Type where
  FunN 0 a b = b
  FunN n a b = a %1 -> FunN (n - 1) a b

predNat :: forall n. (1 <= n, KnownNat n) => Dict (KnownNat (n - 1))
predNat = case someNatVal (natVal' @n (proxy# @_) - 1) of
  Just (SomeNat (_ :: Proxy p)) -> Unsafe.coerce (Dict @(KnownNat p))
  Nothing -> error "Vector.pred: n-1 is necessarily a Nat, if 1<=n"

caseNat :: forall n. KnownNat n => Either (n :~: 0) ((1 <=? n) :~: 'True)
caseNat =
  case theLength @n of
    0 -> Left $ unsafeZero @n
    _ -> Right $ unsafeNonZero @n
{-# INLINE caseNat #-}

-- By definition.
expandFunN :: forall n a b. (1 <= n) => FunN n a b %1 -> a %1 -> FunN (n - 1) a b
expandFunN k = Unsafe.coerce k

-- By definition.
contractFunN :: (1 <= n) => (a %1 -> FunN (n - 1) a b) %1 -> FunN n a b
contractFunN k = Unsafe.coerce k

continue :: forall n a b c. KnownNat n => (b %1 -> c) %1 -> FunN n a b %1 -> FunN n a c
continue = case caseNat @n of
  Left Refl -> id
  Right Refl -> \f t -> contractFunN @n @a @c (continueS f (expandFunN @n @a @b t))
    where
      continueS :: (KnownNat n, 1 <= n) => (b %1 -> c) %1 -> (a %1 -> FunN (n - 1) a b) %1 -> (a %1 -> FunN (n - 1) a c)
      continueS f' x a = case predNat @n of Dict -> continue @(n - 1) @a @b f' (x a)

unsafeZero :: n :~: 0
unsafeZero = Unsafe.coerce Refl

unsafeNonZero :: (1 <=? n) :~: 'True
unsafeNonZero = Unsafe.coerce Refl
