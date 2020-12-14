{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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
module Data.V.Internal.Linear.V
  ( V(..)
  , FunN
  , theLength
  , elim
  , make
  , iterate
  -- * Type-level utilities
  , caseNat
  ) where

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
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe

{- Developers Note

See the "Developers Note" in Data.V.Linear to understand this module structure.

-}

-- # Type Definitions
-------------------------------------------------------------------------------

newtype V (n :: Nat) (a :: Type) = V (Vector a)
  deriving (Eq, Ord, Prelude.Functor)
  -- Using vector rather than, say, 'Array' (or directly 'Array#') because it
  -- offers many convenience function. Since all these unsafeCoerces probably
  -- kill the fusion rules, it may be worth it going lower level since I
  -- probably have to write my own fusion anyway. Therefore, starting from
  -- Vectors at the moment.

type family FunN (n :: Nat) (a :: Type) (b :: Type) :: Type where
  FunN 0 a b = b
  FunN n a b = a %1-> FunN (n-1) a b

-- # API
-------------------------------------------------------------------------------

theLength :: forall n. KnownNat n => Int
theLength = fromIntegral (natVal' @n (proxy# @_))

split :: 1 <= n => V n a %1-> (# a, V (n-1) a #)
split = Unsafe.toLinear split'
  where
    split' :: 1 <= n => V n a -> (# a, V (n-1) a #)
    split' (V xs) = (# Vector.head xs, V (Vector.tail xs) #)

consumeV :: V 0 a %1-> b %1-> b
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
expandFunN :: forall n a b. (1 <= n) => FunN n a b %1-> a %1-> FunN (n-1) a b
expandFunN k = Unsafe.coerce k

-- By definition.
contractFunN :: (1 <= n) => (a %1-> FunN (n-1) a b) %1-> FunN n a b
contractFunN k = Unsafe.coerce k

-- TODO: consider using template haskell to make this expression more efficient.
-- | This is like pattern-matching on a n-tuple. It will eventually be
-- polymorphic the same way as a case expression.
elim :: forall n a b. KnownNat n => V n a %1-> FunN n a b %1-> b
elim xs f =
  case caseNat @n of
    Left Refl -> consumeV xs f
    Right Refl -> elimS (split xs) f
  where
    elimS :: 1 <= n => (# a, V (n-1) a #) %1-> FunN n a b %1-> b
    elimS (# x, xs' #) g = case predNat @n of
      Dict -> elim xs' (expandFunN @n @a @b g x)

-- XXX: This can probably be improved a lot.
make :: forall n a. KnownNat n => FunN n a (V n a)
make = case caseNat @n of
          Left Refl -> V Vector.empty
          Right Refl -> contractFunN @n @a @(V n a) prepend
            where prepend :: a %1-> FunN (n-1) a (V n a)
                  prepend t = case predNat @n of
                                Dict -> continue @(n-1) @a @(V (n-1) a) (cons t) (make @(n-1) @a)

cons :: forall n a. a %1-> V (n-1) a %1-> V n a
cons = Unsafe.toLinear2 $ \x (V v) -> V (Vector.cons x v)

continue :: forall n a b c. KnownNat n => (b %1-> c) %1-> FunN n a b %1-> FunN n a c
continue = case caseNat @n of
             Left Refl -> id
             Right Refl -> \f t -> contractFunN @n @a @c (continueS f (expandFunN @n @a @b t))
               where continueS :: (KnownNat n, 1 <= n) => (b %1-> c) %1-> (a %1-> FunN (n-1) a b) %1-> (a %1-> FunN (n-1) a c)
                     continueS f' x a = case predNat @n of Dict -> continue @(n-1) @a @b f' (x a)

iterate :: forall n a. (KnownNat n, 1 <= n) => (a %1-> (a, a)) -> a %1-> V n a
iterate dup init =
  go @n init
 where
  go :: forall m. (KnownNat m, 1 <= m) => a %1-> V m a
  go a =
    case predNat @m of
      Dict -> case caseNat @(m-1) of
        Prelude.Left Refl ->
          case pr1 @m Refl of
            Refl ->
              (make @m @a :: a %1-> V m a) a
        Prelude.Right Refl ->
          dup a & \(a', a'') ->
            a' `cons` go @(m-1) a''

  -- An unsafe cast to prove the simple equality.
  pr1 :: forall k. 0 :~: (k - 1) -> k :~: 1
  pr1 Refl = Unsafe.coerce Refl

