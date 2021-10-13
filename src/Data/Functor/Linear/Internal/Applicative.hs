{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Trustworthy #-}

module Data.Functor.Linear.Internal.Applicative
  (
    Applicative(..)
  ) where

import Data.Functor.Linear.Internal.Functor
import Prelude.Linear.Internal
import qualified Control.Monad.Trans.Reader as NonLinear
import Data.Monoid.Linear hiding (Sum)
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.V.Linear.Internal.V (V (..))
import qualified Data.V.Linear.Internal.V as V
import qualified Data.Vector as Vector
import GHC.TypeNats (KnownNat)
import qualified Unsafe.Linear as Unsafe

-- # Applicative definition
-------------------------------------------------------------------------------

-- | Data 'Applicative'-s can be seen as containers which can be zipped
-- together. A prime example of data 'Applicative' are vectors of known length
-- ('ZipList's would be, if it were not for the fact that zipping them together
-- drops values, which we are not allowed to do in a linear container).
--
-- In fact, an applicative functor is precisely a functor equipped with (pure
-- and) @liftA2 :: (a %1-> b %1-> c) -> f a %1-> f b %1-> f c@. In the case where
-- @f = []@, the signature of 'liftA2' would specialise to that of 'zipWith'.
--
-- Intuitively, the type of 'liftA2' means that 'Applicative's can be seen as
-- containers whose "number" of elements is known at compile-time. This
-- includes vectors of known length but excludes 'Maybe', since this may
-- contain either zero or one value.  Similarly, @((->) r)@ forms a Data
-- 'Applicative', since this is a (possibly infinitary) container indexed by
-- @r@, while lists do not, since they may contain any number of elements.
--
-- == Remarks for the mathematically inclined
--
-- An 'Applicative' is, as in the restricted case, a lax monoidal endofunctor of
-- the category of linear types. That is, it is equipped with
--
-- * a (linear) function @() %1-> f ()@
-- * a (linear) natural transformation @(f a, f b) %1-> f (a, b)@
--
-- It is a simple exercise to verify that these are equivalent to the definition
-- of 'Applicative'. Hence that the choice of linearity of the various arrow is
-- indeed natural.
class Functor f => Applicative f where
  {-# MINIMAL pure, (liftA2 | (<*>)) #-}
  pure :: a -> f a
  (<*>) :: f (a %1-> b) %1-> f a %1-> f b
  f <*> x = liftA2 ($) f x
  liftA2 :: (a %1-> b %1-> c) -> f a %1-> f b %1-> f c
  liftA2 f x y = f <$> x <*> y

-- # Instances
-------------------------------------------------------------------------------

instance KnownNat n => Applicative (V n) where
  pure a = V $ Vector.replicate (V.theLength @n) a
  (V fs) <*> (V xs) = V $
    Unsafe.toLinear2 (Vector.zipWith (\f x -> f $ x)) fs xs

instance Monoid x => Applicative (Const x) where
  pure _ = Const mempty
  Const x <*> Const y = Const (x <> y)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u,f) <*> (v,x) = (u <> v, f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
   pure x = Compose (pure (pure x))
   (Compose f) <*> (Compose x) = Compose (liftA2 (<*>) f x)
   liftA2 f (Compose x) (Compose y) = Compose (liftA2 (liftA2 f) x y)

instance Applicative m => Applicative (NonLinear.ReaderT r m) where
  pure x = NonLinear.ReaderT (\_ -> pure x)
  NonLinear.ReaderT f <*> NonLinear.ReaderT x = NonLinear.ReaderT (\r -> f r <*> x r)

