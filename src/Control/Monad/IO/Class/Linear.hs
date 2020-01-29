{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.IO.Class.Linear where

import qualified Control.Monad.Linear as Linear
import Prelude.Linear
import qualified System.IO.Linear as Linear

-- | Like 'NonLinear.MonadIO' but allows to lift linear 'IO'
-- actions into a linear monad
class Linear.Monad m => MonadIO m where
  liftIO :: Linear.IO a ->. m a

instance MonadIO Linear.IO where
  liftIO = id
