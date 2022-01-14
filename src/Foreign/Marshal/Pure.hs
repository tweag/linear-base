{-# LANGUAGE NoImplicitPrelude #-}

-- | This module introduces primitives to /safely/ allocate and discard system
-- heap memory (/not GC heap memory/) for storing  values /explicitly/.
-- (Basically, a haskell program has a GC that at runtime, manages its own heap
-- by freeing and allocating space from the system heap.) Values discarded
-- explicitly don't need to be managed by the garbage collector (GC), which
-- therefore has less work to do. Less work for the GC can sometimes mean more
-- predictable request latencies in multi-threaded and distributed
-- applications.
--
-- This module is meant to be imported qualified.
--
-- == The Interface
--
-- Run a computation that uses heap memory by passing a continuation to
-- 'withPool' of type @Pool %1-> Ur b@. Allocate and free with
-- 'alloc' and 'deconstruct'. Make as many or as few pools you need, by
-- using the 'Dupable' and 'Consumable' instances of  'Pool'.
--
-- A toy example:
--
-- >>> :set -XLinearTypes
-- >>> import Data.Unrestricted.Linear
-- >>> import qualified Foreign.Marshal.Pure as Manual
-- >>> :{
--   nothingWith3 :: Pool %1-> Ur Int
--   nothingWith3 pool = move (Manual.deconstruct (Manual.alloc 3 pool))
-- :}
--
-- >>> unur (Manual.withPool nothingWith3)
-- 3
--
--
-- === What are 'Pool's?
--
-- 'Pool's are memory pools from which a user can safely allocate and use
-- heap memory manually by passing 'withPool' a continuation.
-- An alternative design would have allowed passing continuations to
-- allocation functions but this could break tail-recursion in certain cases.
--
-- Pools play another role: resilience to exceptions. If an exception is raised,
-- all the data in the pool is deallocated.
--
-- Note that data from one pool can refer to data in another pool and vice
-- versa.
--
-- == Large Examples
--
-- You can find example data structure implementations in @Foreign.List@ and
-- @Foreign.Heap@ [here](https://github.com/tweag/linear-base/tree/master/examples/Foreign).
module Foreign.Marshal.Pure
  ( -- * Allocating and using values on the heap
    Pool,
    withPool,
    Box,
    alloc,
    deconstruct,

    -- * Typeclasses for values that can be allocated
    KnownRepresentable,
    Representable (..),
    MkRepresentable (..),
  )
where

import Foreign.Marshal.Pure.Internal
