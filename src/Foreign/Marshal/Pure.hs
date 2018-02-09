{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module introduces primitives to /safely/ allocate and discard
-- in-memory storage for values /explicitly/. Values discarded explicitly don't
-- need to be managed by the garbage collector (GC), which therefore has less
-- work to do. Less work for the GC can sometimes mean more predictable request
-- latencies in multi-threaded and distributed applications.
--
-- This module is meant to be imported qualified.
--
-- == Examples
--
-- You can find example data structure implementations in @Foreign.List@ and
-- @Foreign.Heap@ of the @example@ directory in the source repository.
--
-- == Pools
--
-- The module interface is structured around a notion of memory 'Pool'. Passing
-- linear pool arguments is an alternative to passing continuations to
-- functions. Passing continuations can break tail-recursion in certain cases.
--
-- Pools play another role: resilience to exceptions. If an exception is raised,
-- all the data in the pool is deallocated.
--
-- Data from one pool can refer to data in another pool and vice versa.

module Foreign.Marshal.Pure
  ( Pool
  , withPool
  , Box
  , alloc
  , deconstruct
  ) where

import Control.Exception
import Data.Kind (Constraint)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple ()
import Prelude.Linear
import System.IO.Unsafe
import qualified Unsafe.Linear as Unsafe

-- XXX: [2018-02-09] I'm having trouble with the `constraints` package (it seems
-- that the version of Type.Reflection.Unsafe in the linear ghc compiler is not
-- the one that was released with 8.2, and that `mtl` fails to compile against
-- it), therefore, I'm redefining `Dict` here, as it's cheap.
data Dict :: Constraint -> * where
  Dict :: c => Dict c

-- TODO: organise into sections

-- | This abstract type class represents values natively known to have a GC-less
-- implementation.
class KnownRepresentable a where
  storable :: Dict (Storable a)

  default storable :: Storable a => Dict (Storable a)
  storable = Dict
  -- This ought to be read a `newtype` around `Storable`.

instance KnownRepresentable Word -- TODO: more word types
instance KnownRepresentable Int
instance
  (KnownRepresentable a, KnownRepresentable b)
  => KnownRepresentable (a, b) where
  storable =
    case (storable @a, storable @b) of
      (Dict, Dict) -> Dict
instance
  (KnownRepresentable a, KnownRepresentable b, KnownRepresentable c)
  => KnownRepresentable (a, b, c) where
  storable =
    case (storable @a, storable @b, storable @c) of
      (Dict, Dict, Dict) -> Dict

-- TODO: Briefly explain the Dupable-reader style of API, below, and fix
-- details.

-- | Pools represent collections of values. A 'Pool' can be 'consume'-ed. This
-- is a no-op: it does not deallocate the data in that pool. It cannot do so,
-- because accessible values might still exist. Consuming a pool simply makes it
-- impossible to add new data to the pool.
data Pool where
  Pool :: DLL (Ptr ()) -> Pool
  -- /!\ Black magic: the pointers in the pool are only used to deallocate
  -- dangling pointers. Therefore their 'sizeOf' does not matter. It is simpler
  -- to cast all the pointers to some canonical type (here `Ptr ()`) so that we
  -- don't have to deal with heterogeneous types. /!\

-- Implementing a doubly-linked list with `Ptr`

data DLL a = DLL { prev :: Ptr (DLL a), elt :: Ptr a, next :: Ptr (DLL a) }
  deriving Eq

-- XXX: probably replaceable by storable-generic
instance Storable (DLL a) where
  sizeOf _ = sizeOf (undefined :: (Ptr (DLL a), Ptr a, Ptr (DLL a)))
  alignment _ = alignment (undefined :: (Ptr (DLL a), Ptr a, Ptr (DLL a)))

  peek ptr = do
    (p, e, n) <- peek (castPtr ptr :: Ptr (Ptr (DLL a), Ptr a, Ptr (DLL a)))
    return $ DLL p e n

  poke ptr (DLL p e n) =
    poke (castPtr ptr :: Ptr (Ptr (DLL a), Ptr a, Ptr (DLL a))) (p, e, n)

-- Precondition: in `insertAfter start ptr`, `next start` must be initalised,
-- and so must be `prev =<< peek (next start)`
insertAfter :: Storable a => DLL a -> a -> IO (Ptr (DLL a))
insertAfter start ptr = do
  secondLink <- peek $ next start
  newLink <- DLL <$> new start <*> new ptr <*> new secondLink
  poke (next start) newLink
  poke (prev secondLink) newLink
  new newLink

delete :: DLL a -> IO ()
delete link = do
  prevLink <- peek $ prev link
  nextLink <- peek $ next link
  poke (next prevLink) nextLink
  poke (prev nextLink) prevLink

-- /Doubly-linked list

-- @freeAll start end@ frees all pointer in the linked list. Assumes that @end@
-- doesn't have a pointer, and indeed terminates the list.
--
freeAll :: DLL (Ptr ()) -> DLL (Ptr ()) -> IO ()
freeAll start end = do
  nextLink <- peek (next start)
  if nextLink == end then do
    free (next start)
    free (prev end)
  else do
    delete nextLink
    free (prev nextLink)
    free (elt nextLink)
    free (next nextLink)
    freeAll start end

-- TODO: document individual functions

withPool :: (Pool ->. Unrestricted b) ->. Unrestricted b
withPool scope = Unsafe.toLinear performScope scope
    -- XXX: do ^ without `toLinear` by using linear IO
  where
    performScope :: (Pool ->. Unrestricted b) -> Unrestricted b
    performScope scope' = unsafeDupablePerformIO $ do
      -- Initialise the pool
      backPtr <- malloc
      let end = DLL backPtr nullPtr nullPtr -- always at the end of the list
      start <- DLL nullPtr nullPtr <$> new end -- always at the start of the list
      poke backPtr start
      -- Run the computation
      evaluate (scope' (Pool start)) `finally`
      -- Clean up remaining variables.
        (freeAll start end)

instance Consumable Pool where
  consume (Pool _) = ()

instance Dupable Pool where
  dup (Pool l) = (Pool l, Pool l)

-- | 'Box a' is the abstract type of manually managed data. It can be used as
-- part of data type definitions in order to store linked data structure off
-- heap. See @Foreign.List@ and @Foreign.Pair@ in the @examples@ directory of
-- the source repository.
data Box a where
  Box :: Ptr (DLL (Ptr ())) -> Ptr a -> Box a

-- XXX: if Box is a newtype, can be derived
instance Storable (Box a) where
  sizeOf _ = sizeOf (undefined :: (Ptr (DLL (Ptr ())), Ptr a))
  alignment _ = alignment (undefined :: (Ptr (DLL (Ptr ())), Ptr a))
  peek ptr = do
    (pool, ptr') <- peek (castPtr ptr :: Ptr (Ptr (DLL (Ptr ())), Ptr a))
    return (Box pool ptr')
  poke ptr (Box pool ptr') =
    poke (castPtr ptr :: Ptr (Ptr (DLL (Ptr ())), Ptr a)) (pool, ptr')

-- TODO: a way to store GC'd data using a StablePtr

-- TODO: reference counted pointer. Remarks: rc pointers are Dupable but not
-- Movable. In order to be useful, need some kind of borrowing on the values, I
-- guess. 'Box' can be realloced, but not RC pointers.

-- XXX: We brazenly suppose that the `Storable` API can be seen as exposing
-- linear functions. It's not very robust. This also ties in the next point.

-- TODO: Ideally, we would like to avoid having a boxed representation of the
-- data before a pointer is created. A better solution is to have a destination
-- passing-style API (but there is still some design to be done there). This
-- alloc primitive would then be derived (but most of the time we would rather
-- write bespoke constructors).
alloc :: forall a. Storable a => a ->. Pool ->. Box a
alloc a (Pool pool) =
    Unsafe.toLinear mkPtr a
  where
    mkPtr :: a -> Box a
    mkPtr a' = unsafeDupablePerformIO $ do
      ptr <- new a'
      poolPtr <- insertAfter pool (castPtr ptr :: Ptr ())
      return (Box poolPtr ptr)


deconstruct :: Storable a => Box a ->. a
deconstruct (Box poolPtr ptr) = unsafeDupablePerformIO $ mask_ $ do
  res <- peek ptr
  delete =<< peek poolPtr
  free ptr
  free poolPtr
  return res
