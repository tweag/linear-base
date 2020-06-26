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

-- | This module defines vectors of known length which can hold linear values.
--
-- Having a known length matters with linear types, because many common vector
-- operations (like zip) are not total with linear types.
--
-- Make these vectors by giving any finite number of arguments to 'make'
-- and use them with 'elim':
--
-- > {-# LANGUAGE LinearTypes #-}
-- > {-# LANGUAGE TypeApplications #-}
-- > {-# LANGUAGE TypeInType #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- >
-- > import Prelude.Linear
-- > import qualified Data.Vector.Linear as Vector
-- >
-- > isTrue :: Bool
-- > isTrue = Vector.elim (listMaker 4 9) doSomething
-- >   where
-- >     -- GHC can't figure out this type equality, so this is needed.
-- >     listMaker :: Int #-> Int #-> Vector.V 2 Int
-- >     listMaker = Vector.make @2 @Int
-- >
-- > doSomething :: Int #-> Int #-> Bool
-- > doSomething x y = lseq x (lseq y True)
--
--
-- A much more expensive library of vectors of known size (including matrices
-- and tensors of all dimensions) is the [@linear@ library on
-- Hackage](https://hackage.haskell.org/package/linear) (that's /linear/ in the
-- sense of [linear algebra](https://en.wikipedia.org/wiki/Linear_algebra),
-- rather than linear types).

module Data.Vector.Linear
  ( V
  , FunN
  , elim
  , make
  ) where

import qualified Data.Functor.Linear.Internal as Data
import qualified Data.Functor.Linear.Internal.Traversable as Data
import Data.Kind (Type)
import Data.Proxy
import Data.Type.Equality
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import GHC.Exts (Constraint, proxy#)
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

newtype V (n :: Nat) (a :: Type) = V (Vector a)
  deriving (Eq, Ord, Prelude.Functor)
  -- Using vector rather than, say, 'Array' (or directly 'Array#') because it
  -- offers many convenience function. Since all these unsafeCoerces probably
  -- kill the fusion rules, it may be worth it going lower level since I
  -- probably have to write my own fusion anyway. Therefore, starting from
  -- Vectors at the moment.

theLength :: forall n. KnownNat n => Int
theLength = fromIntegral (natVal' @n (proxy# @_))

instance Data.Functor (V n) where
  fmap f (V xs) = V $ Unsafe.toLinear (Vector.map (\x -> f x)) xs

instance KnownNat n => Data.Applicative (V n) where
  pure a = V $ Vector.replicate (theLength @n) a
  (V fs) <*> (V xs) = V $
    Unsafe.toLinear2 (Vector.zipWith (\f x -> f $ x)) fs xs

instance KnownNat n => Data.Traversable (V n) where
  traverse f (V xs) =
    (V . Unsafe.toLinear (Vector.fromListN (theLength @n))) Data.<$>
    Data.traverse f (Unsafe.toLinear Vector.toList xs)

type family FunN (n :: Nat) (a :: Type) (b :: Type) :: Type where
  FunN 0 a b = b
  FunN n a b = a #-> FunN (n-1) a b

split :: 1 <= n => V n a #-> (# a, V (n-1) a #)
split = Unsafe.toLinear split'
  where
    split' :: 1 <= n => V n a -> (# a, V (n-1) a #)
    split' (V xs) = (# Vector.head xs, V (Vector.tail xs) #)

consumeV :: V 0 a #-> b #-> b
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
expandFunN :: forall n a b. (1 <= n) => FunN n a b #-> a #-> FunN (n-1) a b
expandFunN k = Unsafe.coerce k

-- By definition.
contractFunN :: (1 <= n) => (a #-> FunN (n-1) a b) #-> FunN n a b
contractFunN k = Unsafe.coerce k

-- TODO: consider using template haskell to make this expression more efficient.
-- | This is like pattern-matching on a n-tuple. It will eventually be
-- polymorphic the same way as a case expression.
elim :: forall n a b. KnownNat n => V n a #-> FunN n a b #-> b
elim xs f =
  case caseNat @n of
    Left Refl -> consumeV xs f
    Right Refl -> elimS (split xs) f
  where
    elimS :: 1 <= n => (# a, V (n-1) a #) #-> FunN n a b #-> b
    elimS (# x, xs' #) g = case predNat @n of
      Dict -> elim xs' (expandFunN @n @a @b g x)

-- XXX: This can probably be improved a lot.
make :: forall n a. KnownNat n => FunN n a (V n a)
make = case caseNat @n of
          Left Refl -> V Vector.empty
          Right Refl -> contractFunN @n @a @(V n a) prepend
            where prepend :: a #-> FunN (n-1) a (V n a)
                  prepend t = case predNat @n of
                                Dict -> continue @(n-1) @a @(V (n-1) a) (cons t) (make @(n-1) @a)

cons :: forall n a. a #-> V (n-1) a #-> V n a
cons = Unsafe.toLinear2 $ \x (V v) -> V (Vector.cons x v)

continue :: forall n a b c. KnownNat n => (b #-> c) #-> FunN n a b #-> FunN n a c
continue = case caseNat @n of
             Left Refl -> id
             Right Refl -> \f t -> contractFunN @n @a @c (continueS f (expandFunN @n @a @b t))
               where continueS :: (KnownNat n, 1 <= n) => (b #-> c) #-> (a #-> FunN (n-1) a b) #-> (a #-> FunN (n-1) a c)
                     continueS f' x a = case predNat @n of Dict -> continue @(n-1) @a @b f' (x a)
