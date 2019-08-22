{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.IO.Class.Linear where

import qualified Control.Monad.IO.Class as NonLinear
import Control.Monad.Lift.Linear
import Prelude.Linear

-- | Like 'NonLinear.MonadIO' but allows to lift non-linear 'IO'
-- actions into a linear monad
type MonadIO m = (MonadLift m, NonLinear.MonadIO (LiftedM m))

-- | Like 'NonLinear.liftIO' but allows to lift non-linear 'IO'
-- actions into a linear monad
liftIO :: MonadIO m => IO a -> m a
liftIO io = lift (NonLinear.liftIO io)

-- | Like 'liftIO' but allows to use the return value unrestrictedly
liftIOU :: MonadIO m => IO a -> m (Unrestricted a)
liftIOU io = liftU (NonLinear.liftIO io)
