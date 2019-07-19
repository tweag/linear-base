{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | = The data functor hierarchy
--
-- This module defines the data functor library. These are linear functors which
-- are better understood as containers of data. Unlike unrestricted functor,
-- there is a split between such data functors and control functors which
-- represent effects (see "Control.Monad.Linear" for more).
--
-- The data functor hierarchy contains a notion of applicative functors
-- (containers which can be zipped) and traversable functors (containers which
-- store a finite number of values).

module Data.Functor.Linear where

import Prelude.Linear.Internal.Simple
import qualified Control.Monad.Linear as Control

class Functor f where
  fmap :: (a ->. b) -> f a ->. f b

(<$>) :: Functor f => (a ->. b) -> f a ->. f b
(<$>) = fmap

-- | Data 'Applicative'-s can be seen as containers which can be zipped
-- together. A prime example of data 'Applicative' are vectors of known lengths
-- ('ZipLists' would be, if it were not for the fact that zipping them together
-- drops values, which we are not allowed to do in a linear container).
--
-- In fact, an applicative functor is precisely a functor equipped with (pure
-- and) @liftA2 :: (a ->. b ->. c) -> f a ->. f b ->. f c@. In the case where
-- @f = []@, the signature of 'liftA2' specialises to that of 'zipWith'.
--
-- == Remarks for the mathematically inclined
--
-- An 'Applicative' is, as in the restricted case, a lax monoidal endofunctor of
-- the category of linear types. That is, it is equipped with
--
-- * a (linear) function @() ->. f ()@
-- * a (linear) natural transformation @(f a, f b) ->. f (a, b)@
--
-- It is a simple exercise to verify that these are equivalent to the definition
-- of 'Applicative'. Hence that the choice of linearity of the various arrow is
-- indeed natural.
class Functor f => Applicative f where
  {-# MINIMAL pure, (liftA2 | (<*>)) #-}
  pure :: a -> f a
  (<*>) :: f (a ->. b) ->. f a ->. f b
  f <*> x = liftA2 ($) f x
  liftA2 :: (a ->. b ->. c) -> f a ->. f b ->. f c
  liftA2 f x y = f <$> x <*> y

class Functor t => Traversable t where
  traverse :: Control.Applicative e => (a ->. e b) -> t a ->. e (t b)

---------------
-- Instances --
---------------

instance Functor [] where
  fmap _f [] = []
  fmap f (a:as) = f a : fmap f as

instance Traversable [] where
  traverse _f [] = Control.pure []
  traverse f (a : as) = (:) Control.<$> (f a) Control.<*> (traverse f as)
