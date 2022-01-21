{-# OPTIONS -Wno-orphans #-}

module Data.Replicator.Linear.Internal.Instances where

import qualified Data.Functor.Linear as Data
import Data.Replicator.Linear.Internal.ReplicationStream
import qualified Data.Replicator.Linear.Internal.ReplicationStream as ReplicationStream
import Data.Replicator.Linear.Internal.Replicator
import qualified Data.Replicator.Linear.Internal.Replicator as Replicator

instance Data.Functor ReplicationStream where
  fmap = ReplicationStream.fmap

instance Data.Applicative ReplicationStream where
  pure = ReplicationStream.pure
  f <*> x = f ReplicationStream.<*> x

instance Data.Functor Replicator where
  fmap = Replicator.fmap

instance Data.Applicative Replicator where
  pure = Replicator.pure
  f <*> x = f Replicator.<*> x
