-- | This module introduces primitives to /safely/ store data off-heap. The
-- benefit of off-heap data is that it does not add to the GC pressure, and help
-- ensure predictable latency (/e.g./ in distributed applications). The cost is
-- that memory management is much more explicit: the programmer has to allocate
-- and free memory manually. Safety (in particular that every pointer is freed)
-- is enforced by linear types, which constrain usage, in particular
-- sharing. Off-heap data types also have less convenient syntax since they are
-- not directly supported by the compiler.
--
-- This module focuses on /pure/ off-heap data. That is data types like standard
-- Haskell. The only difference is that their lifetime is not managed by the
-- GC. Despite calling @malloc@ and @free@ under the hood, the entire API is
-- pure, and does not make calls in IO.
--
-- The allocation API is organised around a notion of memory pool. From the API
-- point of view, a pool serves as a source of linearity. That is: it ensures
-- that the allocation primitive need not take a continuation to delimit its
-- lifetime. Besides convenience, it avoids needlessly preventing functions from
-- being tail-recursive.
--
-- Pools play another role: resilience to exceptions. If an exception is raised,
-- all the data in the pool is deallocated. It does not, however, impose a stack
-- discipline: data in pools is normally freed by calls to the 'free' function,
-- only in case of exception are the pool deallocated in a stack-like
-- manner. Moreover, pool A can have data pointing to pool B, while at the same
-- time, pool B having data pointing to pool A.
--
-- Functions in this module are meant to be qualified.

-- TODO: add link to an example in module header
-- TODO: change some words into link to the relevant API entry in the above description.

module Data.OffHeap
  (
  ) where
