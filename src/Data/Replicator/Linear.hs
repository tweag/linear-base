{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

-- | This module defines a stream-like type named 'Replicator', which is
-- mainly used in the definition of the 'Dupable' class to provide efficient
-- linear duplication.
-- The API of 'Replicator' is close to the one of an infinite stream: it
-- can either produce a new value linearly (with 'next' or 'next#'), or be
-- linearly discarded (with 'consume' or 'extract').
-- 'Data.Unrestricted.Movable' types are deep-copied only once (no matter
-- how many copies are required), whereas non-'Data.Unrestricted.Movable'
-- values are duplicated on demand using an internal stream type.
-- 'Replicator' also have a linear 'Data.Functor.Linear.Applicative' instance.
module Data.Replicator.Linear
  ( Replicator,
    consume,
    map,
    pure,
    (<*>),
    next,
    next#,
    take,
    extract,
    elim,
  )
where

import Data.Replicator.Linear.Internal
import Data.Replicator.Linear.Internal.Instances ()
