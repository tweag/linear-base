{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

-- | This module provides tools to use do notation with normal monads after
-- '-XRebindableSyntax' is turned on.
--
-- > {-# LANGUAGE LinearTypes #-}
-- > {-# LANGUAGE RebindableSyntax #-}
-- > {-# LANGUAGE RecordWildCards #-}
-- > import qualified Control.Monad.Builder as Control
-- > func = do
-- >   ...
-- >   where
-- >     Control.Builder {..} = Control.monadBuilder
--
-- Using '-XRebindableSyntax' means that do notation uses in scope definitions
-- of @(>>=)@, @return@, and @(>>)@ to desugar the @do@ block.
--
-- In this module, we create a record 'monadBuilder' that has @(>>=)@ and so on
-- as members. We use -XRecordWildCards to those functions in scope with the
-- wildcard @Control.Builder {..}@.
module Control.Monad.Builder
  ( BuilderType(..)
  , monadBuilder
  ) where

-- TODO: Link to example of builder

import qualified Control.Monad as Unrestricted hiding (fail)
import qualified Control.Monad.Fail as Unrestricted
import Prelude.Linear (String)

-- | Type of 'monadBuilder'. Note how the constraint on @m@ varies depending on
-- the field. The constraints are solved lazily when a field is used by the do
-- notation (in particular, if you don't do a pattern-matching, then you don't
-- need a 'LMonadFail').
data BuilderType = Builder
  { (>>=) :: forall m a b. Unrestricted.Monad m => m a -> (a -> m b) -> m b
  , (>>) :: forall m b. Unrestricted.Monad m => m () -> m b -> m b
  , fail :: forall m a. Unrestricted.MonadFail m => String -> m a
  , return :: forall m a. Unrestricted.Monad m => a -> m a
    -- See also 'Control.Monad.Linear.Builder.return'
  }

-- | A builder to be used with @-XRebindableSyntax@ in conjunction with
-- @RecordWildCards@
monadBuilder :: BuilderType
monadBuilder = Builder
  { (>>=) = (Unrestricted.>>=)
  , (>>) = (Unrestricted.>>)
  , fail = Unrestricted.fail
  , return = Unrestricted.return }
