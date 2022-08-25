{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This is a very very simple prelude, which doesn't depend on anything else
-- in the linear-base library.
module Prelude.Linear.Internal
  ( module Prelude.Linear.Internal
  , module Data.Type.Equality
  ) where

import Data.Coerce
import Data.Functor.Identity
import GHC.Exts (TYPE)
import Data.Type.Equality

-- A note on implementation: to avoid silly mistakes, very easy functions are
-- simply reimplemented here. For harder function, we reuse the Prelude
-- definition and make an unsafe cast.


($) :: forall {rep} a (b :: TYPE rep) p q. (a %p -> b) %q -> a %p -> b
($) f x = f x

infixr 0 $ -- same fixity as base.$

(&) :: forall {rep} a (b :: TYPE rep) p q. a %p -> (a %p -> b) %q -> b
x & f = f x

infixl 1 & -- same fixity as base.&

id :: a %q -> a
id x = x

const :: a %q -> b -> a
const x _ = x

-- | @seq x y@ only forces @x@ to head normal form, therefore is not guaranteed
-- to consume @x@ when the resulting computation is consumed. Therefore, @seq@
-- cannot be linear in it's first argument.
seq :: a -> b %q -> b
seq !_ y = y

infixr 0 `seq` -- same fixity as base.seq

($!) :: forall {rep} a (b :: TYPE rep) p q. (a %p -> b) %q -> a %p -> b
($!) f !a = f a

infixr 0 $! -- same fixity as base.$!

curry :: ((a, b) %p -> c) %q -> a %p -> b %p -> c
curry f x y = f (x, y)

uncurry :: (a %p -> b %p -> c) %q -> (a, b) %p -> c
uncurry f (x, y) = f x y

-- | Beware: @(.)@ is not compatible with the standard one because it is
-- higher-order and we don't have sufficient multiplicity polymorphism yet.
(.) :: forall {rep} b (c :: TYPE rep) a q m n. (b %1 -> c) %q -> (a %1 -> b) %m -> a %n -> c
f . g = \x -> f (g x)

infixr 9 . -- same fixity as base..

-- | Convenience operator when a higher-order function expects a non-linear
-- arrow but we have a linear arrow.
forget :: forall {rep} a (b :: TYPE rep). (a %1 -> b) %1 -> a -> b
forget f a = f a

-- XXX: Temporary, until newtype record projections are linear.
runIdentity' :: Identity a %p -> a
runIdentity' (Identity x) = x

-- | A linear version of 'Data.Coerce.coerce' for types of kind 'Data.Kind.Type'.
lcoerce :: forall a b. Coercible a b => a %1 -> b
lcoerce = coerce ((\x -> x) :: a %1 -> a)
{-# INLINE CONLIKE lcoerce #-}
