{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Data.Traversable.Linear
  ( -- * Linear traversable hierarchy
    -- $ traversable
    Traversable(..)
  , mapM, sequenceA, for, forM
  , mapAccumL, mapAccumR
  ) where

import qualified Control.Monad.Linear.Internal as Control
import qualified Data.Functor.Linear.Internal as Data
import qualified Data.Applicative.Linear as Data
import Data.Functor.Const
import Prelude.Linear.Internal
import Prelude (Maybe(..), Either(..))

-- $traversable

-- TODO: write the laws
-- TODO: maybe add a Foldable class between Functor and Traversable as well

-- | A linear data traversible is a functor of type @t a@ where you can apply a
-- linear effectful action of type @a %1-> f b@ on each value of type @a@ and
-- compose this to perform an action on the whole functor, resulting in a value
-- of type @f (t b)@.
--
-- To learn more about 'Traversable', see here:
--
--  * \"Applicative Programming with Effects\",
--    by Conor McBride and Ross Paterson,
--    /Journal of Functional Programming/ 18:1 (2008) 1-13, online at
--    <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>.
--
--  * \"The Essence of the Iterator Pattern\",
--    by Jeremy Gibbons and Bruno Oliveira,
--    in /Mathematically-Structured Functional Programming/, 2006, online at
--    <http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator>.
--
--  * \"An Investigation of the Laws of Traversals\",
--    by Mauro Jaskelioff and Ondrej Rypacek,
--    in /Mathematically-Structured Functional Programming/, 2012, online at
--    <http://arxiv.org/pdf/1202.2919>.
--
class Data.Functor t => Traversable t where
  {-# MINIMAL traverse | sequence #-}

  traverse :: Control.Applicative f => (a %1-> f b) -> t a %1-> f (t b)
  {-# INLINE traverse #-}
  traverse f x = sequence (Data.fmap f x)

  sequence :: Control.Applicative f => t (f a) %1-> f (t a)
  {-# INLINE sequence #-}
  sequence = traverse id

mapM :: (Traversable t, Control.Monad m) => (a %1-> m b) -> t a %1-> m (t b)
mapM = traverse
{-# INLINE mapM #-}

sequenceA :: (Traversable t, Control.Applicative f) => t (f a) %1-> f (t a)
sequenceA = sequence
{-# INLINE sequenceA #-}

for :: (Traversable t, Control.Applicative f) => t a %1-> (a %1-> f b) -> f (t b)
for t f = traverse f t
{-# INLINE for #-}

forM :: (Traversable t, Control.Monad m) => t a %1-> (a %1-> m b) -> m (t b)
forM = for
{-# INLINE forM #-}

mapAccumL :: Traversable t => (a %1-> b %1-> (a,c)) -> a %1-> t b %1-> (a, t c)
mapAccumL f s t = swap $ Control.runState (traverse (\b -> Control.state $ \i -> swap $ f i b) t) s

mapAccumR :: Traversable t => (a %1-> b %1-> (a,c)) -> a %1-> t b %1-> (a, t c)
mapAccumR f s t = swap $ runStateR (traverse (\b -> StateR $ \i -> swap $ f i b) t) s

swap :: (a,b) %1-> (b,a)
swap (x,y) = (y,x)

-- | A right-to-left state transformer
newtype StateR s a = StateR (s %1-> (a, s))
  deriving (Data.Functor, Data.Applicative) via Control.Data (StateR s)

runStateR :: StateR s a %1-> s %1-> (a, s)
runStateR (StateR f) = f

instance Control.Functor (StateR s) where
  fmap f (StateR x) = StateR $ (\(a, s') -> (f a, s')) . x

instance Control.Applicative (StateR s) where
  pure x = StateR $ \s -> (x,s)
  StateR f <*> StateR x = StateR (go . Control.fmap f . x)
    where go :: (a, (a %1-> b, s)) %1-> (b, s)
          go (a, (h, s'')) = (h a, s'')

------------------------
-- Standard instances --
------------------------

instance Traversable [] where
  traverse _f [] = Control.pure []
  traverse f (a : as) = (:) Control.<$> f a Control.<*> traverse f as

instance Traversable ((,) a) where
  sequence (a, fb) = (a,) Control.<$> fb

instance Traversable Maybe where
  sequence Nothing = Control.pure Nothing
  sequence (Just x) = Control.fmap Just x

instance Traversable (Const a) where
  sequence (Const x) = Control.pure (Const x)

instance Traversable (Either a) where
  sequence (Left x) = Control.pure (Left x)
  sequence (Right x) = Right Control.<$> x

