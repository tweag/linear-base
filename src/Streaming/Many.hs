{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains all functions that do something with
-- multiple streams as input or output. This includes combining
-- streams, splitting a stream, etc.
module Streaming.Many
  (
  -- * Unzip
    unzip
  -- * Merging
  , merge
  , mergeOn
  , mergeBy
  ) where

import Streaming.Type
import Streaming.Consume
import Prelude (undefined, Bool(..), Either(..), Ord(..), Ordering(..), (.))
import Prelude.Linear (($), (&))
import qualified Prelude.Linear
import qualified Prelude.Linear as Linear
import qualified Control.Monad.Linear as Control
import Control.Monad.Linear.Builder (BuilderType(..), monadBuilder)


-- # Unzip
-------------------------------------------------------------------------------

unzip :: Control.Monad m =>
  Stream (Of (a, b)) m r #-> Stream (Of a) (Stream (Of b) m) r
unzip = loop
  where
  Builder{..} = monadBuilder
  loop :: Control.Monad m =>
    Stream (Of (a, b)) m r #-> Stream (Of a) (Stream (Of b) m) r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap loop $ Control.lift m
    Step ((a,b):> rest) -> Step (a :> Effect (Step (b :> Return (loop rest))))


-- # Merging
-------------------------------------------------------------------------------

merge :: (Control.Monad m, Ord a) =>
  Stream (Of a) m r #-> Stream (Of a) m s #-> Stream (Of a) m (r,s)
merge = mergeBy compare

mergeOn :: (Control.Monad m, Ord b) =>
  (a -> b) ->
  Stream (Of a) m r #->
  Stream (Of a) m s #->
  Stream (Of a) m (r,s)
mergeOn f = mergeBy (\x y -> compare (f x) (f y))

mergeBy :: Control.Monad m =>
  (a -> a -> Ordering) ->
  Stream (Of a) m r #->
  Stream (Of a) m s #->
  Stream (Of a) m (r,s)
mergeBy comp s1 s2 = s1 & \case
  Return r -> Effect $ effects s2 >>= \s -> return $ Return (r, s)
  Effect ms -> Effect $
    ms >>= \s1' -> return $ mergeBy comp s1' s2
  Step (a :> as) -> s2 & \case
    Return s -> Effect $ effects as >>= \r -> return $ Return (r, s)
    Effect ms -> Effect $
      ms >>= \s2' -> return $ mergeBy comp (Step (a :> as)) s2'
    Step (b :> bs) -> case comp a b of
      LT -> Step (a :> Step (b :> mergeBy comp as bs))
      _ -> Step (b :> Step (a :> mergeBy comp as bs))
  where
    Builder{..} = monadBuilder

