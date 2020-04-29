{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

-- | This module provides tools to use do notation with linear monads after
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
module Control.Monad.Linear.Builder
  ( BuilderType(..)
  , monadBuilder
  ) where

-- TODO: Link to example of builder


import qualified Control.Monad.Linear as Linear
import Prelude.Linear (String)

-- | Type of 'monadBuilder'. Note how the constraint on @m@ varies depending on
-- the field. The constraints are solved lazily when a field is used by the do
-- notation (in particular, if you don't do a pattern-matching, then you don't
-- need a 'LMonadFail').
data BuilderType = Builder
  { (>>=) :: forall m a b. Linear.Monad m => m a #-> (a #-> m b) #-> m b
  , (>>) :: forall m b. Linear.Monad m => m () #-> m b #-> m b
  , fail :: forall m a. Linear.MonadFail m => String -> m a
  , return :: forall m a. Linear.Monad m => a #-> m a
    -- I [aspiwack] need `return` in my builder due to
    -- https://ghc.haskell.org/trac/ghc/ticket/14670
    --
    -- I originally intended `return` to be used qualified. But this is fine
    -- too. So we may stick to it
  }

-- | A builder to be used with @-XRebindableSyntax@ in conjunction with
-- @RecordWildCards@
monadBuilder :: BuilderType
monadBuilder = Builder
  { (>>=) = (Linear.>>=)
  , (>>) = (Linear.>>)
  , fail = Linear.fail
  , return = Linear.return }
