-- | A class to lift non-linear monads into linear monads.
--
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Lift.Linear where

import qualified Control.Monad as NonLinear
import qualified Control.Monad.Linear as Linear
import Prelude.Linear
import qualified System.IO.Linear as Linear

-- | Lift a non-linear monad into a linear monad.
--
-- Laws:
--
-- > lift . return = return
--
-- > liftU . return = return . Unrestricted
--
-- > lift (m >>= f) = liftU m >>= \(Unrestricted a) -> lift (f a)
--
-- > liftU (m >>= f) = liftU m >>= \(Unrestricted a) -> liftU (f a)
--
class (Linear.Monad m, NonLinear.Monad (LiftedM m)) => MonadLift m where

  -- | The type of the monad whose computations are lifted
  type LiftedM m :: * -> *

  -- | Lift a nonlinear monadic computation into a linear monad
  lift :: LiftedM m a -> m a

  -- | Like 'lift' but allows to use the result unrestrictedly
  liftU :: LiftedM m a -> m (Unrestricted a)

instance MonadLift Linear.IO where
  type LiftedM Linear.IO = IO
  lift io = Linear.fromSystemIO io
  liftU = Linear.fromSystemIOU
