-- | This is a very very simple prelude, which doesn't depend on anything else
-- in the linear-base library (except possibly "Unsafe.Linear").

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LinearTypes #-}

module Prelude.Linear.Internal.Simple where

import qualified Prelude as Prelude
import qualified Unsafe.Linear as Unsafe

-- A note on implementation: to avoid silly mistakes, very easy functions are
-- simply reimplemented here. For harder function, we reuse the Prelude
-- definition and make an unsafe cast.

-- | Beware: @($)@ is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
($) :: (a #-> b) #-> a #-> b
-- XXX: Temporary as `($)` should get its typing rule directly from the type
-- inference mechanism.
($) f x = f x
infixr 0 $

(&) :: a #-> (a #-> b) #-> b
x & f = f x
infixl 1 &

id :: a #-> a
id x = x

const :: a #-> b -> a
const x _ = x

-- | @seq x y@ only forces @x@ to head normal form, therefore is not guaranteed
-- to consume @x@ when the resulting computation is consumed. Therefore, @seq@
-- cannot be linear in it's first argument.
seq :: a -> b #-> b
seq x = Unsafe.toLinear (Prelude.seq x)

-- | Beware, 'curry' is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
curry :: ((a, b) #-> c) #-> a #-> b #-> c
curry f x y = f (x, y)

-- | Beware, 'uncurry' is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
uncurry :: (a #-> b #-> c) #-> (a, b) #-> c
uncurry f (x,y) = f x y

-- | Beware: @(.)@ is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
(.) :: (b #-> c) #-> (a #-> b) #-> a #-> c
f . g = \x -> f (g x)

-- | Linearly typed replacement for the standard 'Prelude.foldr' function,
-- to allow linear consumption of a list.
foldr :: (a #-> b #-> b) -> b #-> [a] #-> b
foldr f z = \case
  [] -> z
  x:xs -> f x (foldr f z xs)

foldl :: (b #-> a -> b) -> b #-> [a] -> b
foldl _ b [] = b
foldl f b (x:xs) = foldl f (f b x) xs


