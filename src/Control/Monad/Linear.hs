{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Linear
  ( -- * Linear monad hierarchy
    -- $monad
    Functor(..)
  , (<$>)
  , (<$)
  , dataFmapDefault
  , Applicative(..)
  , dataPureDefault
  , Monad(..)
  , MonadFail(..)
  , return
  , join
  , ap
  , Data(..)
  , foldM
  ) where

import Control.Monad.Linear.Internal
import Data.Unrestricted.Linear

-- | Linearly typed replacement for the standard '(Prelude.<$)' function.
(<$) :: (Functor f, Consumable b) => a ->. f b ->. f a
a <$ fb = fmap (`lseq` a) fb
