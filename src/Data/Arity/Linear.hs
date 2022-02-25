-- |
-- This module provides type-level helpers and classes to deal with n-ary
-- functions.
--
-- See 'Data.V.Linear.make', 'Data.V.Linear.elim' and
-- 'Data.Replicator.Linear.elim' for use-cases.
module Data.Arity.Linear
  ( Peano (..),
    NatToPeano,
    PeanoToNat,
    FunN,
    Arity,
    IsFunN,
  )
where

import Data.Arity.Linear.Internal
