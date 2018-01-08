{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

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
  { (>>=) :: forall m a b. Linear.Monad m => m a ->. (a ->. m b) ->. m b
  , (>>) :: forall m b. Linear.Monad m => m () ->. m b ->. m b
  , fail :: forall m a. Linear.MonadFail m => String -> m a
  }

-- | A builder to be used with @-XRebindableSyntax@ in conjunction with
-- @RecordWildCards@
monadBuilder :: BuilderType
monadBuilder = Builder
  { (>>=) = (Linear.>>=)
  , (>>) = (Linear.>>)
  , fail = Linear.fail }
