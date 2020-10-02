-- | This is a very very simple prelude, which doesn't depend on anything else
-- in the linear-base library (except possibly "Unsafe.Linear").

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Linear.Internal where

import qualified Prelude as Prelude
import qualified Unsafe.Linear as Unsafe

-- A note on implementation: to avoid silly mistakes, very easy functions are
-- simply reimplemented here. For harder function, we reuse the Prelude
-- definition and make an unsafe cast.

-- | Beware: @($)@ is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
($) :: (a %1-> b) %1-> a %1-> b
-- XXX: Temporary as `($)` should get its typing rule directly from the type
-- inference mechanism.
($) f x = f x
infixr 0 $

(&) :: a %1-> (a %1-> b) %1-> b
x & f = f x
infixl 1 &

id :: a %1-> a
id x = x

const :: a %1-> b -> a
const x _ = x

asTypeOf :: a %1-> a -> a
asTypeOf = const

-- | @seq x y@ only forces @x@ to head normal form, therefore is not guaranteed
-- to consume @x@ when the resulting computation is consumed. Therefore, @seq@
-- cannot be linear in it's first argument.
seq :: a -> b %1-> b
seq x = Unsafe.toLinear (Prelude.seq x)

($!) :: (a %1-> b) %1-> a %1-> b
($!) f !a = f a

-- | Beware, 'curry' is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
curry :: ((a, b) %1-> c) %1-> a %1-> b %1-> c
curry f x y = f (x, y)

-- | Beware, 'uncurry' is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
uncurry :: (a %1-> b %1-> c) %1-> (a, b) %1-> c
uncurry f (x,y) = f x y

-- | Beware: @(.)@ is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
(.) :: (b %1-> c) %1-> (a %1-> b) %1-> a %1-> c
f . g = \x -> f (g x)

-- XXX: temporary: with multiplicity polymorphism functions expecting a
-- non-linear arrow would allow a linear arrow passed, so this would be
-- redundant
-- | Convenience operator when a higher-order function expects a non-linear
-- arrow but we have a linear arrow.
forget :: (a %1-> b) %1-> a -> b
forget f a = f a
