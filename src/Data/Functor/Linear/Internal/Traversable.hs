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
