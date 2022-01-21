{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.V.Linear.Internal.Ambiguous (theLength, make, FunN) where

import Data.Kind
import Data.Proxy
import Data.Type.Equality
import {-# SOURCE #-} Data.V.Linear.Internal.V (V (..), cons)
import qualified Data.Vector as Vector
import GHC.Exts (proxy#)
import GHC.TypeLits
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import Prelude
  ( Bool (..),
    Either (..),
    Int,
    Maybe (..),
    error,
    fromIntegral,
    (-),
  )

theLength :: forall n. KnownNat n => Int
theLength = fromIntegral (natVal' @n (proxy# @_))

-- TODO: improve
-------------------------------------------------------------------------------

-- Same as in the constraints library, but it's just as easy to avoid a
-- dependency here.
data Dict (c :: Constraint) where
  Dict :: c => Dict c

type family FunN (n :: Nat) (a :: Type) (b :: Type) :: Type where
  FunN 0 a b = b
  FunN n a b = a %1 -> FunN (n - 1) a b

make :: forall n a. KnownNat n => FunN n a (V n a)
make = case caseNat @n of
  Left Refl -> V Vector.empty
  Right Refl -> contractFunN @n @a @(V n a) prepend
    where
      prepend :: a %1 -> FunN (n - 1) a (V n a)
      prepend t = case predNat @n of
        Dict -> continue @(n - 1) @a @(V (n - 1) a) (cons t) (make @(n - 1) @a)

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

contractFunN :: (1 <= n) => (a %1 -> FunN (n - 1) a b) %1 -> FunN n a b
contractFunN k = Unsafe.coerce k

continue :: forall n a b c. KnownNat n => (b %1 -> c) %1 -> FunN n a b %1 -> FunN n a c
continue = case caseNat @n of
  Left Refl -> id
  Right Refl -> \f t -> contractFunN @n @a @c (continueS f (expandFunN @n @a @b t))
    where
      continueS :: (KnownNat n, 1 <= n) => (b %1 -> c) %1 -> (a %1 -> FunN (n - 1) a b) %1 -> (a %1 -> FunN (n - 1) a c)
      continueS f' x a = case predNat @n of Dict -> continue @(n - 1) @a @b f' (x a)

expandFunN :: forall n a b. (1 <= n) => FunN n a b %1 -> a %1 -> FunN (n - 1) a b
expandFunN k = Unsafe.coerce k

unsafeZero :: n :~: 0
unsafeZero = Unsafe.coerce Refl

unsafeNonZero :: (1 <=? n) :~: 'True
unsafeNonZero = Unsafe.coerce Refl
