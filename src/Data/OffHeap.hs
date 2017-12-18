{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
-- The allocation API is organised around a notion of memory 'Pool'. From the API
-- point of view, a pool serves as a source of linearity. That is: it ensures
-- that the allocation primitive need not take a continuation to delimit its
-- lifetime. Besides convenience, it avoids needlessly preventing functions from
-- being tail-recursive.
--
-- Pools play another role: resilience to exceptions. If an exception is raised,
-- all the data in the pool is deallocated. It does not, however, impose a stack
-- discipline: data in pools is normally freed by the destruction primitives of ,
-- only in case of exception are the pool deallocated in a stack-like
-- manner. Moreover, pool A can have data pointing to pool B, while at the same
-- time, pool B having data pointing to pool A.
--
-- Functions in this module are meant to be qualified.

-- TODO: add link to an example in module header
-- TODO: change some words into link to the relevant API entry in the above description.

module Data.OffHeap
  ( Pool
  , withPool
  , Box
  , alloc
  , deconstruct
  ) where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Prelude.Linear
import System.IO.Unsafe

-- TODO: ignoring exceptions for the moment. So that I can get some tests to
-- work first.

-- | A 'Pool' can be 'consume'-ed. This is a no-op: it does not deallocate the
-- data in that pool. It cannot as there may still be accessible data in the
-- pool. It simply makes it impossible to add new data to the pool. It is
-- actually necessary to so consume a pool allocated with 'withPool' in order to
-- write a well-typed scope @Pool ->. Unrestricted b@.
data Pool = Pool

-- TODO: document individual functions

withPool :: (Pool ->. Unrestricted b) ->. Unrestricted b
withPool scope = scope Pool

instance Consumable Pool where
  consume Pool = ()

-- XXX: this indirection is possibly not necessary. It's here because the inner
-- Ptr must be unrestricted (in order to implement deconstruct at the moment).
data Box a where
  Box :: Ptr a -> Box a

-- XXX: if Box is a newtype, can be derived
instance Storable (Box a) where
  sizeOf _ = sizeOf (undefined :: Ptr a)
  alignment _ = alignment (undefined :: Ptr a)
  peek ptr = Box <$> (peek (castPtr ptr :: Ptr (Ptr a)))
  poke ptr (Box ptr') = poke (castPtr ptr :: Ptr (Ptr a)) ptr'

-- TODO: a way to store GC'd data using a StablePtr

-- TODO: The fact that the 'a' is unrestricted is a problem. This is due to the
-- `Storable` abstraction. The most immediate workaround is to pretend that
-- `Storable` functions are linear. It's not very robust. This also ties in the
-- next point.

-- TODO: Ideally, we would like to avoid having a boxed representation of the
-- data before a pointer is created. A better solution is to have a destination
-- passing-style API (but there is still some design to be done there). This
-- alloc primitive would then be derived (but most of the time we would rather
-- write bespoke constructors).
alloc :: Storable a => a -> Pool ->. (Box a, Pool)
alloc a Pool =
    (Box thePtr, Pool)
  where
    thePtr = unsafeDupablePerformIO $ do
      ptr <- malloc
      poke ptr a
      return ptr

deconstruct :: Storable a => Box a ->. a
deconstruct (Box ptr) = unsafeDupablePerformIO $ do
  res <- peek ptr
  free ptr
  return res
