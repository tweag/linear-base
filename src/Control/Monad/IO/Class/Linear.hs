{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.IO.Class.Linear where

import qualified Control.Functor.Linear as Linear
import Prelude.Linear
import qualified System.IO as System
import qualified System.IO.Linear as Linear

-- | Like 'NonLinear.MonadIO' but allows to lift both linear
-- and non-linear 'IO' actions into a linear monad.
class Linear.Monad m => MonadIO m where
  liftIO :: Linear.IO a %1-> m a
  liftSystemIO :: System.IO a -> m a
  liftSystemIO io = liftIO (Linear.fromSystemIO io)
  liftSystemIOU :: System.IO a -> m (Ur a)
  liftSystemIOU io = liftIO (Linear.fromSystemIOU io)

instance MonadIO Linear.IO where
  liftIO = id
