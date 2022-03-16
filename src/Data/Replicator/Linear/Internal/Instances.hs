{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Replicator.Linear.Internal.Instances where

import qualified Data.Functor.Linear as Data
import Data.Replicator.Linear.Internal
import qualified Data.Replicator.Linear.Internal as Replicator
import Data.Replicator.Linear.Internal.ReplicationStream
import qualified Data.Replicator.Linear.Internal.ReplicationStream as ReplicationStream

instance Data.Functor ReplicationStream where
  fmap = ReplicationStream.map

instance Data.Applicative ReplicationStream where
  pure = ReplicationStream.pure
  f <*> x = f ReplicationStream.<*> x

instance Data.Functor Replicator where
  fmap = Replicator.map

instance Data.Applicative Replicator where
  pure = Replicator.pure
  f <*> x = f Replicator.<*> x
  liftA2 = Replicator.liftA2
