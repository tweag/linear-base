{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

module Data.Replicator.Linear
  ( Replicator,
    next,
    next#,
    extract,
    elim,
    module Data.Replicator.Linear.Internal.Instances,
  )
where

import Data.Replicator.Linear.Internal
import Data.Replicator.Linear.Internal.Instances ()
