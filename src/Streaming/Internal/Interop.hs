{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains functions for interoperating with other
-- streaming libraries.
module Streaming.Internal.Interop
  ( -- * Interoperating with other streaming libraries
    reread
  ) where

import Streaming.Internal.Type
import Streaming.Internal.Produce
import Data.Unrestricted.Linear
import Prelude.Linear (($), (&))
import Prelude (Maybe(..))
import qualified Control.Monad.Linear as Control
import Control.Monad.Linear.Builder (BuilderType(..), monadBuilder)

{-| Read an @IORef (Maybe a)@ or a similar device until it reads @Nothing@.
    @reread@ provides convenient exit from the @io-streams@ library

> reread readIORef    :: IORef (Maybe a) -> Stream (Of a) IO ()
> reread Streams.read :: System.IO.Streams.InputStream a -> Stream (Of a) IO ()
-}
reread :: Control.Monad m =>
  (s -> m (Unrestricted (Maybe a))) -> s -> Stream (Of a) m ()
reread f s = reread' f s
  where
    Builder{..} = monadBuilder

    reread' :: Control.Monad m =>
      (s -> m (Unrestricted (Maybe a))) -> s -> Stream (Of a) m ()
    reread' f s = Effect $ do
      Unrestricted maybeA <- f s
      case maybeA of
        Nothing -> return $ Return ()
        Just a -> return $ (yield a >> reread f s)
{-# INLINABLE reread #-}

