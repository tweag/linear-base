{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

-- |
-- This module provides an unlifted mutable array with a pure
-- interface. The array elements are stored linearly, so operations
-- usually require the either a linear callback or a 'Consumable' or
-- 'Dupable' constraint.
--
-- Accessing out-of-bounds indices causes undefined behaviour.
--
-- This module is meant to be imported qualified.
module Data.LArray.Mutable.Unlifted.Linear
  ( LArray#
  , alloc
  , fromList
  , allocBeside
  , lseq
  , size
  , get
  , set
  , update
  , map
  , toList
  , append
  , dup2
  ) where

import Data.LArray.Mutable.Unlifted.Linear.Internal
