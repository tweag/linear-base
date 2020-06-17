{-# LANGUAGE LinearTypes #-}

-- | This module provides functions that take one input
-- stream and produce one output stream. These are functions that
-- process a single stream.
module Streaming.Process
  (
  -- * Stream processors
  -- ** Splitting and inspecting streams of elements
    next
  {-
  , uncons
  , splitAt
  , split
  , break
  , breakWhen
  , span
  , group
  , groupBy
  -- * Partitions
  , partitionEithers
  , partition
  -- * Maybes
  , catMaybes
  , mapMaybe
  -- ** Direct Transformations
  , map
  , mapM
  , maps
  , mapped
  , for
  , with
  , subst
  , copy
  , copy'
  , store
  , chain
  , sequence
  , filter
  , filterM
  , delay
  , intersperse
  , take
  , takeWhile
  , takeWhileM
  , drop
  , dropWhile
  , concat
  , scan
  , scanM
  , scanned
  , read
  , show
  , cons
  , duplicate
  , duplicate'
  -}
  ) where

import Streaming.Type
import qualified Prelude.Linear as Linear
import qualified Control.Monad.Linear as Linear
import qualified Control.Monad.Linear.Builder as Control

next :: Linear.Monad m => Stream (Of a) m r #-> m (Either r (a, Stream (Of a) m r))
next (Return r) = Linear.return Linear.$ Left r
next (Effect m) = m Linear.>>= next
next (Step (a :> rest)) = Linear.return Linear.$ Right (a, rest)
{-# INLINABLE next #-}









