-- | This is a very very simple prelude, which doesn't depend on anything else
-- in the linear-base library.

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Linear.Internal where

import Data.Functor.Identity

-- A note on implementation: to avoid silly mistakes, very easy functions are
-- simply reimplemented here. For harder function, we reuse the Prelude
-- definition and make an unsafe cast.

-- XXX: Add runtime-representation polymorphism such that
-- `($)`+`-XImpredicativeType` (starting with 9.2) has the complete behaviour of
-- GHC's native `($)` rule.
($) :: (a %p-> b) %q-> a %p-> b
($) f x = f x
infixr 0 $

(&) :: a %p-> (a %p-> b) %q-> b
x & f = f x
infixl 1 &

id :: a %q-> a
id x = x

const :: a %q-> b -> a
const x _ = x

asTypeOf :: a %q-> a -> a
asTypeOf = const

-- | @seq x y@ only forces @x@ to head normal form, therefore is not guaranteed
-- to consume @x@ when the resulting computation is consumed. Therefore, @seq@
-- cannot be linear in it's first argument.
seq :: a -> b %q-> b
seq !_ y = y

($!) :: (a %p-> b) %q-> a %p-> b
($!) f !a = f a

curry :: ((a, b) %p-> c) %q-> a %p-> b %p-> c
curry f x y = f (x, y)

uncurry :: (a %p-> b %p-> c) %q-> (a, b) %p-> c
uncurry f (x,y) = f x y

-- | Beware: @(.)@ is not compatible with the standard one because it is
-- higher-order and we don't have sufficient multiplicity polymorphism yet.
(.) :: (b %1-> c) %q-> (a %1-> b) %m-> a %n-> c
f . g = \x -> f (g x)

-- | Convenience operator when a higher-order function expects a non-linear
-- arrow but we have a linear arrow.
forget :: (a %1-> b) %1-> a -> b
forget f a = f a

-- XXX: Temporary, until newtype record projections are linear.
runIdentity' :: Identity a %p-> a
runIdentity' (Identity x) = x
