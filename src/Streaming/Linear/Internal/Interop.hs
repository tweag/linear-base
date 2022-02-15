{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module contains functions for interoperating with other
-- streaming libraries.
module Streaming.Linear.Internal.Interop
  ( -- * Interoperating with other streaming libraries
    reread,
  )
where

import qualified Control.Functor.Linear as Control
import Data.Unrestricted.Linear
import Prelude.Linear (($))
import Streaming.Linear.Internal.Produce
import Streaming.Linear.Internal.Type
import Prelude (Maybe (..))

-- | Read an @IORef (Maybe a)@ or a similar device until it reads @Nothing@.
--    @reread@ provides convenient exit from the @io-streams@ library
--
-- > reread readIORef    :: IORef (Maybe a) -> Stream (Of a) IO ()
-- > reread Streams.read :: System.IO.Streams.InputStream a -> Stream (Of a) IO ()
reread ::
  Control.Monad m =>
  (s -> m (Ur (Maybe a))) ->
  s ->
  Stream (Of a) m ()
reread f s = reread' f s
  where
    reread' ::
      Control.Monad m =>
      (s -> m (Ur (Maybe a))) ->
      s ->
      Stream (Of a) m ()
    reread' f s = Effect $ Control.do
      Ur maybeA <- f s
      case maybeA of
        Nothing -> Control.return $ Return ()
        Just a -> Control.return $ (yield a Control.>> reread f s)
{-# INLINEABLE reread #-}
