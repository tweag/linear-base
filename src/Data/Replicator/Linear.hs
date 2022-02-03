{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module defines a stream-like type named 'Replicator', which is
-- mainly used in the definition of the 'Data.Unrestricted.Dupable' class
-- to provide efficient linear duplication.
-- The API of 'Replicator' is close to the one of an infinite stream: it
-- can either produce a new value linearly (with 'next' or 'next#'), or be
-- linearly discarded (with 'consume' or 'extract').
--
-- A crucial aspect, from a performance standpoint, is that the 'pure' function
-- (which takes an unrestricted argument) is implemented efficiently: the
-- 'Replicator' returns /the same/ value on each call to 'next'. That is, the
-- pointer is always shared. This will allow 'Data.Unrestricted.Movable' types
-- to be given an efficient instance of 'Data.Unrestricted.Dupable'. Instances
-- of both 'Data.Unrestricted.Movable' and 'Data.Unrestricted.Dupable' typically
-- involve deep copies. The implementation of 'pure' lets us make sure that, for
-- @Movable@ types, only one deep copy is performed, rather than one per
-- additional replica.
--
-- Strictly speaking, the implementation of '(<*>)' plays a role in all this as
-- well:
-- For two 'pure' 'Replicators' @fs@ and @as@, @fs <*> as@  is a pure
-- 'Replicator'. Together, 'pure' and '(<*>)' form the
-- 'Data.Functor.Linear.Applicative' instance of 'Replicator'.
module Data.Replicator.Linear
  ( Replicator,
    consume,
    duplicate,
    map,
    pure,
    (<*>),
    next,
    next#,
    take,
    extract,
    Elim,
    elim,
  )
where

import Data.Replicator.Linear.Internal
import Data.Replicator.Linear.Internal.Instances ()
