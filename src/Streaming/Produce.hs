{-# LANGUAGE LinearTypes #-}

-- | This module provides all producers of a stream
module Streaming.Produce
  ( yield
  , each
    {-
  , each'
  , unfoldr
  , stdinLn
  , readLn
  , fromHandle
  , readFile
  , iterate
  , iterateM
  , repeat
  , repeatM
  , replicate
  , untilRight
  , cycle
  , replicateM
  , enumFrom
  , enumFromThen
  , seconds
  -}
  ) where

import Prelude hiding (readLn, replicate)
import Streaming.Type
import qualified Control.Monad.Linear as Linear
import qualified Data.Foldable.Linear as Linear
import System.IO hiding (readLn)


yield :: Linear.Monad m => a -> Stream (Of a) m ()
yield a = Step (a :> Return ())

each :: (Linear.Monad m, Linear.Foldable f) => f a #-> Stream (Of a) m ()
each = Linear.foldr (\a p -> Step (a :> p)) (Return ())



