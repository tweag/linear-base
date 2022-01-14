{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- This module provides mutable hashmaps with a linear interface.
--
-- It is implemented with Robin Hood hashing which has amortized
-- constant time lookups and updates.
module Data.HashMap.Mutable.Linear
  ( -- * A mutable hashmap
    HashMap,
    Keyed,

    -- * Constructors
    empty,
    fromList,

    -- * Modifiers
    insert,
    insertAll,
    delete,
    filter,
    filterWithKey,
    mapMaybe,
    mapMaybeWithKey,
    shrinkToFit,
    alter,
    alterF,

    -- * Accessors
    size,
    capacity,
    lookup,
    member,
    toList,

    -- * Combining maps
    union,
    unionWith,
    intersectionWith,
  )
where

import Data.HashMap.Mutable.Linear.Internal
