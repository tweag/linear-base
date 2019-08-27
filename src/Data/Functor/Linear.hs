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

module Data.Functor.Linear
  ( Functor(..)
  , Applicative(..)
  , (<$>)
  , (<$)
  , Const(..)
    -- * Linear traversable hierarchy
    -- $ traversable
  , Traversable(..)
  , mapM, sequenceA, for, forM
  )
  where

import Data.Functor.Linear.Internal
import Data.Functor.Linear.Internal.Traversable
import Data.Functor.Const
import Data.Unrestricted.Linear

(<$) :: (Functor f, Consumable b) => a -> f b ->. f a
a <$ fb = fmap (`lseq` a) fb
