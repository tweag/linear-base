{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains functions for interoperating with other
-- streaming libraries.
module Streaming.Interop
  ( reread
  ) where

import Streaming.Type
import Streaming.Produce
import Data.Unrestricted.Linear
import Prelude.Linear (($), (&))
import Prelude (Maybe(..))
import qualified Control.Monad.Linear as Control
import Control.Monad.Linear.Builder (BuilderType(..), monadBuilder)


reread :: Control.Monad m =>
  (s -> m (Unrestricted (Maybe a))) -> s -> Stream (Of a) m ()
reread f s = Effect $ do
  Unrestricted maybeA <- f s
  case maybeA of
    Nothing -> return $ Return ()
    Just a -> return $ (yield a >> reread f s)
  where
    Builder{..} = monadBuilder

