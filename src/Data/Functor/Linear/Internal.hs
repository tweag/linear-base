{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

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

module Data.Functor.Linear.Internal where

import Prelude.Linear.Internal.Simple
import Prelude (Maybe(..), Either(..))
import qualified Prelude
import Data.Functor.Const
import Data.Monoid.Linear
import GHC.Generics

import qualified Unsafe.Linear as Unsafe

class Functor f where
  fmap :: (a ->. b) -> f a ->. f b
  default fmap :: (Generic1 f, GFunctor (Rep1 f)) => (a ->. b) -> f a ->. f b
  fmap = fmapDefault

fmapDefault :: (Generic1 f, GFunctor (Rep1 f)) => (a ->. b) -> f a ->. f b
fmapDefault f = to1' . gmap f . from1'

to1' :: Generic1 f => Rep1 f a ->. f a
to1' = Unsafe.toLinear to1

from1' :: Generic1 f => f a ->. Rep1 f a
from1' = Unsafe.toLinear from1

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

---------------
-- Instances --
---------------

instance Functor [] where
  fmap _f [] = []
  fmap f (a:as) = f a : fmap f as

instance Functor (Const x) where
  fmap _ (Const x) = Const x

instance Monoid x => Applicative (Const x) where
  pure _ = Const mempty
  Const x <*> Const y = Const (x <> y)

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Functor (Either e) where
  fmap _ (Left x) = Left x
  fmap f (Right x) = Right (f x)

newtype Reader r x = Reader (r ->. x)
-- TODO: replace below with a newtype deconstructor once record projections
-- are inferred properly
runReader :: Reader r x ->. r ->. x
runReader (Reader f) = f

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)

class GFunctor f where
  gmap :: (a ->. b) -> f a ->. f b

instance GFunctor V1 where
  gmap _ = Prelude.error "Void gmap"

instance GFunctor U1 where
  gmap _ U1 = U1

instance GFunctor (K1 i c) where
  gmap _ (K1 a) = K1 a

instance GFunctor x => GFunctor (M1 i c x) where
  gmap f (M1 x) = M1 (gmap f x)

instance (GFunctor f, GFunctor g) => GFunctor (f :+: g) where
  gmap f (L1 r) = L1 (gmap f r)
  gmap f (R1 r) = R1 (gmap f r)

instance (GFunctor f, GFunctor g) => GFunctor (f :*: g) where
  gmap f (r :*: s) = gmap f r :*: gmap f s

instance (GFunctor f, GFunctor g) => GFunctor (f :.: g) where
  gmap f (Comp1 x) = Comp1 (gmap (gmap f) x)

class GFunctor f => GApplicative f where
  gpure :: a -> f a
  gap :: f (a ->. b) ->. f a ->. f b

instance GApplicative U1 where
  gpure _ = U1
  gap U1 U1 = U1

instance Monoid c => GApplicative (K1 i c) where
  gpure _ = K1 mempty
  gap (K1 f) (K1 x) = K1 (f <> x)

instance (GApplicative f, GApplicative g) => GApplicative (f :*: g) where
  gpure x = gpure x :*: gpure x
  gap (f :*: g) (x :*: y) = gap f x :*: gap g y

instance (GApplicative f, GApplicative g) => GApplicative (f :.: g) where
  gpure x = Comp1 (gpure (gpure x))
  gap (Comp1 f) (Comp1 x) = Comp1 (gap (gmap gap f) x)
