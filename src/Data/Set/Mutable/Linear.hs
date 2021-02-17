{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- This module defines linear mutable sets.
--
-- The underlying implementation uses 'Data.HashMap.Linear', so it inherits
-- the time and memory characteristics of it.
--
-- Please import this module qualified to avoid name clashes.
module Data.Set.Mutable.Linear
  ( -- * Mutable Sets
    Set,
    empty,
    insert,
    delete,
    union,
    intersection,
    size,
    member,
    fromList,
    toList,
    Keyed,
  )
where

import Data.Set.Mutable.Linear.Internal
