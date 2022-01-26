{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Replicator.Linear.Internal.ReplicationStream (ReplicationStream (..), consume, map, pure, (<*>)) where

import Data.Unrestricted.Internal.Ur
import Prelude.Linear.Internal

data ReplicationStream a where
  ReplicationStream :: s %1 -> (s %1 -> a) -> (s %1 -> (s, s)) -> (s %1 -> ()) -> ReplicationStream a

consume :: ReplicationStream a %1 -> ()
consume (ReplicationStream s _ _ consumes) = consumes s

map :: (a %1 -> b) -> ReplicationStream a %1 -> ReplicationStream b
map f (ReplicationStream s give dups consumes) =
  ReplicationStream s (f . give) dups consumes

pure :: a -> ReplicationStream a
pure x =
  ReplicationStream
    (Ur x)
    unur
    ( \case
        Ur x' -> (Ur x', Ur x')
    )
    ( \case
        Ur _ -> ()
    )

(<*>) :: ReplicationStream (a %1 -> b) %1 -> ReplicationStream a %1 -> ReplicationStream b
(ReplicationStream sf givef dupsf consumesf) <*> (ReplicationStream sx givex dupsx consumesx) =
  ReplicationStream
    (sf, sx)
    (\(sf', sx') -> givef sf' (givex sx'))
    ( \(sf', sx') ->
        (dupsf sf', dupsx sx') & \case
          ((sf1, sf2), (sx1, sx2)) -> ((sf1, sx1), (sf2, sx2))
    )
    ( \(sf', sx') ->
        consumesf sf' & \case
          () -> consumesx sx'
    )
