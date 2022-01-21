{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Replicator.Linear.Internal.ReplicationStream (ReplicationStream (..), consume, fmap, pure, (<*>)) where

import Data.Unrestricted.Internal.Ur
import Prelude.Linear.Internal

data ReplicationStream a where
  ReplicationStream :: (s %1 -> a) -> (s %1 -> (s, s)) -> (s %1 -> ()) -> s %1 -> ReplicationStream a

consume :: ReplicationStream a %1 -> ()
consume (ReplicationStream _ _ consumes s) = consumes s

fmap :: (a %1 -> b) -> ReplicationStream a %1 -> ReplicationStream b
fmap f (ReplicationStream give dups consumes s) =
  ReplicationStream (f . give) dups consumes s

pure :: a -> ReplicationStream a
pure x =
  ReplicationStream
    unur
    ( \case
        Ur x' -> (Ur x', Ur x')
    )
    ( \case
        Ur _ -> ()
    )
    (Ur x)

(<*>) :: ReplicationStream (a %1 -> b) %1 -> ReplicationStream a %1 -> ReplicationStream b
(ReplicationStream givef dupsf consumesf sf) <*> (ReplicationStream givex dupsx consumesx sx) =
  ReplicationStream
    (\(sf', sx') -> givef sf' (givex sx'))
    ( \(sf', sx') ->
        (dupsf sf', dupsx sx') & \case
          ((sf1, sf2), (sx1, sx2)) -> ((sf1, sx1), (sf2, sx2))
    )
    ( \(sf', sx') ->
        consumesf sf' & \case
          () -> consumesx sx'
    )
    (sf, sx)
