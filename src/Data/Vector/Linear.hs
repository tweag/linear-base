{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines vectors of known length which can hold linear values
--
-- Having a known length matters with linear types, because many common vector
-- operations (like zip) are not total with linear types.
--
-- A much more expensive library of vectors of known size (including matrices
-- and tensors of all dimensions) is the [@linear@ library on
-- Hackage](https://hackage.haskell.org/package/linear) (that's /linear/ in the
-- sense of [linear algebra](https://en.wikipedia.org/wiki/Linear_algebra),
-- rather than linear types).

module Data.Vector.Linear
  ( V
  , Elim
  , elim
  ) where

import qualified Control.Monad.Linear as Control
import qualified Data.Functor.Linear as Data
import Data.Proxy
import Data.Type.Equality
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts (Constraint, Proxy#, proxy#)
import GHC.TypeLits
import Prelude
  ( Eq
  , Ord
  , Int
  , Bool(..)
  , Either(..)
  , Maybe(..)
  , fromIntegral
  , error
  , (-))
import qualified Prelude as Prelude
import Prelude.Linear.Internal.Simple
import qualified Unsafe.Linear as Unsafe

newtype V (n :: Nat) (a :: *) = V { unV :: Vector a }
  deriving (Eq, Ord, Prelude.Functor)
  -- Using vector rather than, say, 'Array' (or directly 'Array#') because it
  -- offers many convenience function. Since all these unsafeCoerces probably
  -- kill the fusion rules, it may be worth it going lower level since I
  -- probably have to write my own fusion anyway. Therefore, starting from
  -- Arrays at the moment.

theLength :: forall n. KnownNat n => Int
theLength = fromIntegral (natVal' (proxy# @_ @n))

instance Data.Functor (V n) where
  fmap f (V xs) = V $ Unsafe.toLinear ((Unsafe.coerce Vector.map) f) xs

instance KnownNat n => Data.Applicative (V n) where
  pure a = V $ Vector.replicate (theLength @n) a
  (V fs) <*> (V xs) = V $
    Unsafe.toLinear2 (Vector.zipWith @(_ ->. _) (\f x -> f x)) fs xs

instance KnownNat n => Data.Traversable (V n) where
  traverse f (V xs) = 
    (V . Unsafe.toLinear (Vector.fromListN (theLength @n))) Control.<$>
    Data.traverse f (Unsafe.toLinear Vector.toList xs)

type family Elim (n :: Nat) (a :: *) (b :: *) :: * where
  Elim 0 a b = b
  Elim n a b = a ->. Elim (n-1) a b

split :: 1 <= n => V n a ->. (# a, V (n-1) a #)
split = Unsafe.toLinear split'
  where
    split' :: 1 <= n => V n a -> (# a, V (n-1) a #)
    split' (V xs) = (# Vector.head xs, V (Vector.tail xs)#)

consumeV :: V 0 a ->. b ->. b
consumeV = Unsafe.toLinear (\_ -> id)

unsafeZero :: n :~: 0
unsafeZero = Unsafe.coerce Refl

unsafeNonZero :: (1 <=? n) :~: 'True
unsafeNonZero = Unsafe.coerce Refl

-- Same as in the constraints library, but it's just as easy to avoid a
-- dependency here.
data Dict (c :: Constraint) where
  Dict :: c => Dict c

predNat :: forall n. (1 <= n, KnownNat n) => Dict (KnownNat (n-1))
predNat = case someNatVal (natVal' (proxy# @_ @n) - 1) of
  Just (SomeNat (_ :: Proxy p)) -> Unsafe.coerce (Dict @(KnownNat p))
  Nothing -> error "Vector.pred: n-1 is necessarily a Nat, if 1<=n"

caseNat :: forall n. KnownNat n => Either (n :~: 0) ((1 <=? n) :~: True)
caseNat =
  case theLength @n of
    0 -> Left $ unsafeZero @n
    _ -> Right $ unsafeNonZero @n
{-# INLINE caseNat #-}

-- TODO: consider using template haskell to make this expression more efficient.
-- | This is like pattern-matching on a n-tuple. It will eventually be
-- polymorphic the same way as a case expression.
elim :: forall n a b. KnownNat n => V n a ->. Elim n a b ->. b
elim xs f =
  case caseNat @n of
    Left Refl -> consumeV xs $ Unsafe.coerce f
    Right Refl -> elimS (split xs) f
  where
    elimS :: 1 <= n => (# a, V (n-1) a #) ->. Elim n a b ->. b
    elimS (# x, xs' #) f = case predNat @n of
      Dict -> elim  xs' ((Unsafe.coerce f :: a ->. Elim (n-1) a b) x)
