{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Functor.Linear.Internal.MonadTrans
  ( MonadTrans (..),
  )
where

import Control.Functor.Linear.Internal.Class

class (forall m. (Monad m) => Monad (t m)) => MonadTrans t where
  lift :: (Monad m) => m a %1 -> t m a
