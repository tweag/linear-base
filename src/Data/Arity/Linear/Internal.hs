{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Arity.Linear.Internal where

import Data.Kind
import GHC.TypeLits
import GHC.Types

data Peano = Z | S Peano

-- | Converts a GHC type-level 'Nat' to a structural type-level natural ('Peano').
type NatToPeano :: Nat -> Peano
type family NatToPeano n where
  NatToPeano 0 = 'Z
  NatToPeano n = 'S (NatToPeano (n - 1))

-- | Converts a structural type-level natural ('Peano') to a GHC type-level 'Nat'.
type PeanoToNat :: Peano -> Nat
type family PeanoToNat n where
  PeanoToNat 'Z = 0
  PeanoToNat ('S n) = 1 + PeanoToNat n

-- | @'FunN' n a b@ represents a function taking @n@ linear arguments of
-- type @a@ and returning a result of type @b@.
type FunN :: Peano -> Type -> Type -> Type
type family FunN n a b where
  FunN 'Z _ b = b
  FunN ('S n) a b = a %1 -> FunN n a b

-- | The 'Arity' type family exists to help the type checker fill in
-- blanks. Chances are that you can safely ignore 'Arity' completely if it's in
-- the type of a function you care. But read on if you are curious.
--
-- The idea is that in a function like 'Data.Replicator.Linear.elim' some of the
-- type arguments are redundant. The function has an ambiguous type, so you will
-- always have to help the compiler either with a type annotation or a type
-- application. But there are several complete ways to do so. In
-- 'Data.Replicator.Linear.elim', if you give the values of `n`, `a`, and `b`,
-- then you can deduce the value of `f` (indeed, it's @'FunN' n a b@). With
-- 'Arity' we can go in the other direction: if `b` and `f` are both known, then
-- we know that `n` is @'Arity' b f@
--
-- 'Arity' returns a 'Nat' rather than a 'Peano' because the result is never
-- consumed. It exists to infer arguments to functions such as
-- 'Data.Replicator.Linear.elim' from the other arguments if they are known.
--
-- 'Arity' could /theorically/ be an associated type family to the 'IsFunN' type
-- class. But it's better to make it a closed type family (which can't be
-- associated to a type class) because it lets us give a well-defined error
-- case. In addition, GHC cannot see that @0 /= 1 + (? :: Nat)@ and as a result we get
-- some overlap which is only allowed in (ordered) closed type families.
type Arity :: Type -> Type -> Nat
type family Arity b f where
  Arity b b = 0
  Arity b (a %1 -> f) = Arity b f + 1
  Arity b f =
    TypeError
      ( 'Text "Arity: "
          ':<>: 'ShowType f
          ':<>: 'Text " isn't a linear function with head "
          ':<>: 'ShowType b
          ':<>: 'Text "."
      )

-- | The 'IsFun' type class is meant to help the type checker fill in
-- blanks. Chances are that you can safely ignore 'IsFun' completely if it's in
-- the type of a function you care. But read on if you are curious.
--
-- The type class 'IsFun' is a kind of inverse to 'FunN', it is meant to be
-- read as @'IsFunN' a b f@ if and only if there exists @n@ such that @f =
-- 'FunN' n a b@ (`n` can be retrieved as @'Arity' b f@ or
-- @'Data.V.Linear.ArityV' f@).
--
-- The reason why 'Arity' (read its documentation first) is not sufficient for
-- our purpose, is that it can find @n@ /if/ @f@ is a linear function of the
-- appropriate shape. But what if @f@ is partially undetermined? Then it is
-- likely that 'Arity' will be stuck. But we know, for instance, that if @f = a1
-- %1 -> a2 %1 -> c@ then we must have @a1 ~ a2@. The trick is that instance
-- resolution of 'IsFun' will add unification constraints that the type checker
-- has to solve. Look in particular at the instance @'IsFunN' a b (a\' %p ->
-- f))@: it matches liberally, so triggers on quite underdetermined @f@, but has
-- equality constraints in its context which will help the type checker.
class IsFunN a b f

instance IsFunN a b b

instance (IsFunN a b f, a' ~ a, p ~ 'One) => IsFunN a b (a' %p -> f)
