{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
module Control.Functor.Linear.Internal.MonadTrans
  ( MonadTrans(..)
  ) where

import Control.Functor.Linear.Internal.Class

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a %1-> t m a

