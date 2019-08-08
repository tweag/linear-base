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
  ) where

import qualified Control.Monad.Linear as Control
import qualified Data.Functor.Linear as Data
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

-- TODO: when polymorphic flip is available, implement in terms of flip
-- forM :: (Linear.Monad m, Traversable t) => t a ->. (a ->. m b) -> m (t b)
-- forM cont act = mapM act cont

-- TODO: We can drop the @Consumable (t ())@ constraint if we make
-- @traverse_@ a member of the 'Traverse' type class. This is probably desirable
-- | Note that the linear 'forM_' has a @'Consumable' (t ())@ constraint: @const
-- ()@ is not linear, therefore to be able to deduce 'forM_' from the
-- 'Traversable' instance, we need to consume the resulting container (which is
-- not free).
-- forM_
--   :: (Linear.Monad m, Traversable t, Consumable (t ()))
--   => t a ->. (a ->. m ()) -> m ()
-- forM_ cont act = Linear.void $ forM cont act

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
