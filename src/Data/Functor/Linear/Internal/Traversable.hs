{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Data.Functor.Linear.Internal.Traversable
  ( -- * Linear traversable hierarchy
    -- $ traversable
    Traversable(..)
  , mapM, sequenceA, for, forM
  , mapAccumL, mapAccumR
  , batch, runWith, Batch(..), fuse
  ) where

import qualified Control.Monad.Linear.Internal as Control
import qualified Data.Functor.Linear.Internal as Data
import Data.Functor.Const
import Prelude.Linear.Internal.Simple
import Prelude (Maybe(..), Either(..))

-- $traversable

-- TODO: write the laws
-- TODO: maybe add a Foldable class between Functor and Traversable as well

class Data.Functor t => Traversable t where
  {-# MINIMAL traverse | sequence #-}

  traverse :: Control.Applicative f => (a ->. f b) -> t a ->. f (t b)
  {-# INLINE traverse #-}
  traverse f x = sequence (Data.fmap f x)

  sequence :: Control.Applicative f => t (f a) ->. f (t a)
  {-# INLINE sequence #-}
  sequence = traverse id

mapM :: (Traversable t, Control.Monad m) => (a ->. m b) -> t a ->. m (t b)
mapM = traverse
{-# INLINE mapM #-}

sequenceA :: (Traversable t, Control.Applicative f) => t (f a) ->. f (t a)
sequenceA = sequence
{-# INLINE sequenceA #-}

for :: (Traversable t, Control.Applicative f) => t a ->. (a ->. f b) -> f (t b)
for t f = traverse f t
{-# INLINE for #-}

forM :: (Traversable t, Control.Monad m) => t a ->. (a ->. m b) -> m (t b)
forM = for
{-# INLINE forM #-}

mapAccumL :: Traversable t => (a ->. b ->. (a,c)) -> a ->. t b ->. (a, t c)
mapAccumL f s t = swap $ Control.runState (traverse (\b -> Control.state $ \i -> swap $ f i b) t) s

mapAccumR :: Traversable t => (a ->. b ->. (a,c)) -> a ->. t b ->. (a, t c)
mapAccumR f s t = swap $ runStateR (traverse (\b -> StateR $ \i -> swap $ f i b) t) s

swap :: (a,b) ->. (b,a)
swap (x,y) = (y,x)

-- right-to-left state transformer
newtype StateR s a = StateR (s ->. (a, s))
  deriving (Data.Functor, Data.Applicative) via Control.Data (StateR s)

runStateR :: StateR s a ->. s ->. (a, s)
runStateR (StateR f) = f

instance Control.Functor (StateR s) where
  fmap f (StateR x) = StateR $ (\(a, s') -> (f a, s')) . x

instance Control.Applicative (StateR s) where
  pure x = StateR $ \s -> (x,s)
  StateR f <*> StateR x = StateR (go . Control.fmap f . x)
    where go :: (a, (a ->. b, s)) ->. (b, s)
          go (a, (h, s'')) = (h a, s'')

data Batch a b c = P c | Batch a b (b ->. c) :*: a
  deriving (Data.Functor, Data.Applicative) via Control.Data (Batch a b)
instance Control.Functor (Batch a b) where
  fmap f (P c) = P (f c)
  fmap f (u :*: a) = Control.fmap (f.) u :*: a

instance Control.Applicative (Batch a b) where
  pure = P
  P f <*> P x = P (f x)
  (u :*: a) <*> P x = ((P $ help x) Control.<*> u) :*: a
  u <*> (v :*: a) = (P (.) Control.<*> u Control.<*> v) :*: a

help :: d ->. ((b ->. d ->. e) ->. b ->. e)
help d bde b = bde b d

batch :: a ->. Batch a b b
batch x = P id :*: x

runWith :: Control.Applicative f => (a ->. f b) -> Batch a b c ->. f c
runWith _ (P x) = Control.pure x
runWith f (u :*: x) = runWith f u Control.<*> f x

fuse :: Batch b b t ->. t
fuse (P i) = i
fuse (u :*: x) = fuse u x

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
