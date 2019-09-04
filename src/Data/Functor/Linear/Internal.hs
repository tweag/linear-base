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

module Data.Functor.Linear.Internal where

import Prelude.Linear.Internal.Simple
import Prelude (Maybe(..), Either(..))
import Data.Functor.Const
import Data.Monoid.Linear
import Data.Functor.Identity
import qualified Control.Monad.Trans.Reader as NonLinear
import qualified Control.Monad.Trans.Cont as NonLinear
import qualified Control.Monad.Trans.Maybe as NonLinear
import qualified Control.Monad.Trans.Except as NonLinear
import qualified Control.Monad.Trans.State.Strict as Strict

class Functor f where
  fmap :: (a ->. b) -> f a ->. f b

(<$>) :: Functor f => (a ->. b) -> f a ->. f b
(<$>) = fmap

-- | Data 'Applicative'-s can be seen as containers which can be zipped
-- together. A prime example of data 'Applicative' are vectors of known length
-- ('ZipLists' would be, if it were not for the fact that zipping them together
-- drops values, which we are not allowed to do in a linear container).
--
-- In fact, an applicative functor is precisely a functor equipped with (pure
-- and) @liftA2 :: (a ->. b ->. c) -> f a ->. f b ->. f c@. In the case where
-- @f = []@, the signature of 'liftA2' would specialise to that of 'zipWith'.
--
-- Intuitively, Data 'Applicative's can be seen as containers whose "number" of
-- elements is known at compile-time. This includes vectors of known length
-- but excludes 'Maybe', since this may contain either zero or one value.
-- Similarly, @((->) r)@ forms a Data 'Applicative', since this is a (possibly
-- infinitary) container indexed by @r@, while lists do not, since they may
-- contain any number of elements.
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

-- Standard instances
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

instance Functor ((,) a) where
  fmap f (x,y) = (x, f y)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u,f) <*> (v,x) = (u <> v, f x)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

---------------------------------
-- Monad transformer instances --
---------------------------------

instance Functor m => Functor (NonLinear.ReaderT r m) where
  fmap f (NonLinear.ReaderT g) = NonLinear.ReaderT (\r -> fmap f (g r))

instance Applicative m => Applicative (NonLinear.ReaderT r m) where
  pure x = NonLinear.ReaderT (\_ -> pure x)
  NonLinear.ReaderT f <*> NonLinear.ReaderT x = NonLinear.ReaderT (\r -> f r <*> x r)

-- The below transformers are all Data.Functors and all fail to be
-- Data.Applicatives without further restriction. In every case however,
-- @pure :: a -> f a@ can be defined in the standard way.
-- For @MaybeT@ and @ExceptT e@, the failure to be applicative is as detailed
-- above: @Maybe@ and @Either e@ can contain 0 or 1 elements, and so fail
-- to be applicative.
-- To give applicative instances for ContT (resp. StateT), we require the
-- parameter r (resp. s) to be Movable.

instance Functor m => Functor (NonLinear.MaybeT m) where
  fmap f (NonLinear.MaybeT x) = NonLinear.MaybeT $ fmap (fmap f) x

instance Functor m => Functor (NonLinear.ExceptT e m) where
  fmap f (NonLinear.ExceptT x) = NonLinear.ExceptT $ fmap (fmap f) x

instance Functor (NonLinear.ContT r m) where
  fmap f (NonLinear.ContT x) = NonLinear.ContT $ \k -> x (\a -> k (f a))

instance Functor m => Functor (Strict.StateT s m) where
  fmap f (Strict.StateT x) = Strict.StateT (\s -> fmap (\(a, s') -> (f a, s')) (x s))
