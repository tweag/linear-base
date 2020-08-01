{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides functions that take one input
-- stream and produce one output stream. These are functions that
-- process a single stream.
module Streaming.Process
  (
  -- * Stream processors
  -- ** Splitting and inspecting streams of elements
    next
  --, uncons
  --, splitAt
  --, split
  --, break
  --, breakWhen
  --, span
  --, group
  --, groupBy
  --, distinguish
  --, switch
  --, seperate
  --, unseparate
  --, eitherToSum
  --, sumToEither
  --, sumToCompose
  --, composeToSum
  -- * Partitions
  --, partitionEithers
  --, partition
  -- * Maybes
  --, catMaybes
  , mapMaybe
  -- ** Direct Transformations
  , map
  --, mapM
  --, maps
  --, mapped
  --, for
  --, with
  --, subst
  --, copy
  --, copy'
  --, store
  --, chain
  --, sequence
  --, filter
  --, filterM
  --, delay
  --, intersperse
  --, take
  --, takeWhile
  --, takeWhileM
  --, drop
  --, dropWhile
  --, concat
  --, scan
  --, scanM
  --, scanned
  --, read
  --, show
  --, cons
  --, duplicate
  --, duplicate'
  ) where

import Streaming.Type
import Prelude.Linear ((&), ($), (.))
import Prelude (Maybe(..), Either(..))
import qualified Control.Monad.Linear as Control
import Control.Monad.Linear.Builder (BuilderType(..), monadBuilder)


-- # Splitting and inspecting streams of elements
-------------------------------------------------------------------------------

next :: Control.Monad m =>
  Stream (Of a) m r #-> m (Either r (a, Stream (Of a) m r))
next stream = stream & \case
  Return r -> return $ Left r
  Effect ms -> ms >>= next
  Step (a :> as) -> return $ Right (a, as)
  where
    Builder{..} = monadBuilder


-- # Maybes
-------------------------------------------------------------------------------

mapMaybe :: Control.Monad m =>
  (a -> Maybe b) -> Stream (Of a) m r #-> Stream (Of b) m r
mapMaybe f stream = stream & \case
  Return r -> Return r
  Effect ms -> Effect $ ms >>= (return . mapMaybe f)
  Step (a :> s) -> case f a of
    Just b -> Step $ b :> (mapMaybe f s)
    Nothing -> mapMaybe f s
  where
    Builder{..} = monadBuilder


-- # Direct Transformations
-------------------------------------------------------------------------------

map :: Control.Monad m => (a -> b) -> Stream (Of a) m r #-> Stream (Of b) m r
map f stream = stream & \case
  Return r -> Return r
  Step (a :> rest) -> Step $ (f a) :> map f rest
  Effect ms -> Effect $ Control.fmap (map f) ms

