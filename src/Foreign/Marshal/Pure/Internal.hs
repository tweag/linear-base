-- XXX: deactivate orphan instance warning as we're defining a few Storable
-- instances here. It's not worth fixing as I [aspiwack] intend to change the
-- interface for something more appropriate, which won't require these Storable
-- instances.
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

module Foreign.Marshal.Pure.Internal where

import Control.Exception
import qualified Data.Functor.Linear as Data
import Data.Kind (Constraint, Type)
import Data.Word (Word8)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple ()
import Prelude.Linear hiding (Eq (..), ($))
import System.IO.Unsafe
import qualified Unsafe.Linear as Unsafe
import Prelude (Eq (..), return, ($), (<$>), (<*>), (=<<))

-- XXX: [2018-02-09] I'm having trouble with the `constraints` package (it seems
-- that the version of Type.Reflection.Unsafe in the linear ghc compiler is not
-- the one that was released with 8.2, and that `mtl` fails to compile against
-- it), therefore, I'm redefining `Dict` here, as it's cheap.
data Dict :: Constraint -> Type where
  Dict :: (c) => Dict c

-- TODO: organise into sections

-- | This abstract type class represents values natively known to have a GC-less
-- implementation. Basically, these are sequences (represented as tuples) of
-- base types.
class KnownRepresentable a where
  storable :: Dict (Storable a)
  default storable :: (Storable a) => Dict (Storable a)
  storable = Dict

-- This ought to be read a `newtype` around `Storable`. This type is abstract,
-- because using Storable this way is highly unsafe: Storable uses IO so we
-- will call unsafePerformIO, and Storable doesn't guarantee linearity. But
-- Storable comes with a lot of machinery, in particular for
-- architecture-independent alignment. So we can depend on it.
--
-- So, we restrict ourselves to known instances that we trust. For base types
-- there is no reason to expect problems. Tuples are a bit more subtle in that
-- they use non-linear operations. But the way they are used should be ok. At
-- any rate: in case a bug is found, the tuple instances are a good place to
-- look.

instance KnownRepresentable Word -- TODO: more word types

instance KnownRepresentable Int

instance KnownRepresentable (Ptr a)

instance KnownRepresentable ()

instance
  (KnownRepresentable a, KnownRepresentable b) =>
  KnownRepresentable (a, b)
  where
  storable =
    case (storable @a, storable @b) of
      (Dict, Dict) -> Dict

instance
  (KnownRepresentable a, KnownRepresentable b, KnownRepresentable c) =>
  KnownRepresentable (a, b, c)
  where
  storable =
    case (storable @a, storable @b, storable @c) of
      (Dict, Dict, Dict) -> Dict

-- TODO: move to the definition of Ur
instance (Storable a) => Storable (Ur a) where
  sizeOf _ = sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek ptr = Ur <$> peek (castPtr ptr :: Ptr a)
  poke ptr (Ur a) = poke (castPtr ptr :: Ptr a) a

instance (KnownRepresentable a) => KnownRepresentable (Ur a) where
  storable | Dict <- storable @a = Dict

-- Below is a KnownRepresentable instance for Maybe. The Storable instance is
-- taken from
-- https://www.schoolofhaskell.com/user/snoyberg/random-code-snippets/storable-instance-of-maybe
--
-- aspiwack: This does not yield very good data representation for the general
-- case. But I believe that to improve on it we need to rethink the abstraction
-- in more depths.

instance (Storable a) => Storable (Maybe a) where
  sizeOf x = sizeOf (stripMaybe x) + 1
  alignment x = alignment (stripMaybe x)
  peek ptr = do
    filled <- peekByteOff ptr $ sizeOf $ stripMaybe $ stripPtr ptr
    case filled == (1 :: Word8) of
      True -> do
        x <- peek (stripMaybePtr ptr)
        return (Just x)
      False ->
        return Nothing
  poke ptr Nothing = pokeByteOff ptr (sizeOf $ stripMaybe $ stripPtr ptr) (0 :: Word8)
  poke ptr (Just a) = do
    poke (stripMaybePtr ptr) a
    pokeByteOff ptr (sizeOf a) (1 :: Word8)

stripMaybe :: Maybe a -> a
stripMaybe _ = error "stripMaybe"

stripMaybePtr :: Ptr (Maybe a) -> Ptr a
stripMaybePtr = castPtr

stripPtr :: Ptr a -> a
stripPtr _ = error "stripPtr"

instance (KnownRepresentable a) => KnownRepresentable (Maybe a) where
  storable | Dict <- storable @a = Dict

-- | Laws of 'Representable':
--
-- * 'toKnown' must be total
-- * 'ofKnown' may be partial, but must be total on the image of 'toKnown'
-- * @ofKnown . toKnown == id@
class (KnownRepresentable (AsKnown a)) => Representable a where
  type AsKnown a :: Type

  toKnown :: a %1 -> AsKnown a
  ofKnown :: AsKnown a %1 -> a

  default toKnown ::
    (MkRepresentable a b, AsKnown a ~ AsKnown b) => a %1 -> AsKnown a
  default ofKnown ::
    (MkRepresentable a b, AsKnown a ~ AsKnown b) => AsKnown a %1 -> a

  toKnown a = toKnown (toRepr a)
  ofKnown b = ofRepr (ofKnown b)

-- Some boilerplate: all the KnownRepresentable are Representable, by virtue of
-- the identity being a retraction. We generalise a bit for the types of tuples:
-- tuples of Representable (not only KnownRepresentable) are Representable.
instance Representable Word where
  type AsKnown Word = Word
  toKnown = id
  ofKnown = id

instance Representable Int where
  type AsKnown Int = Int
  toKnown = id
  ofKnown = id

instance Representable (Ptr a) where
  type AsKnown (Ptr a) = Ptr a
  toKnown = id
  ofKnown = id

instance Representable () where
  type AsKnown () = ()
  toKnown = id
  ofKnown = id

instance
  (Representable a, Representable b) =>
  Representable (a, b)
  where
  type AsKnown (a, b) = (AsKnown a, AsKnown b)
  toKnown (a, b) = (toKnown a, toKnown b)
  ofKnown (x, y) = (ofKnown x, ofKnown y)

instance
  (Representable a, Representable b, Representable c) =>
  Representable (a, b, c)
  where
  type AsKnown (a, b, c) = (AsKnown a, AsKnown b, AsKnown c)
  toKnown (a, b, c) = (toKnown a, toKnown b, toKnown c)
  ofKnown (x, y, z) = (ofKnown x, ofKnown y, ofKnown z)

instance (Representable a) => Representable (Maybe a) where
  type AsKnown (Maybe a) = Maybe (AsKnown a)
  toKnown (Just x) = Just (toKnown x)
  toKnown Nothing = Nothing
  ofKnown (Just x) = Just (ofKnown x)
  ofKnown Nothing = Nothing

-- | This is an easier way to create an instance of 'Representable'. It is a bit
-- abusive to use a type class for this (after all, it almost never makes sense
-- to use this as a constraint). But it works in practice.
--
-- To use, define an instance of @MkRepresentable <myType> <intermediateType>@
-- then declare the following instance:
--
-- @instance Representable <myType> where {type AsKnown = AsKnown <intermediateType>}@
--
-- And the default instance mechanism will create the appropriate
-- 'Representable' instance.
--
-- Laws of 'MkRepresentable':
--
-- * 'toRepr' must be total
-- * 'ofRepr' may be partial, but must be total on the image of 'toRepr'
-- * @ofRepr . toRepr = id@
class (Representable b) => MkRepresentable a b | a -> b where
  toRepr :: a %1 -> b
  ofRepr :: b %1 -> a

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

data DLL a = DLL {prev :: Ptr (DLL a), elt :: Ptr a, next :: Ptr (DLL a)}
  deriving (Eq)

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
insertAfter :: (Storable a) => DLL a -> a -> IO (Ptr (DLL a))
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
  if nextLink == end
    then do
      free (next start)
      free (prev end)
    else do
      delete nextLink
      free (prev nextLink)
      free (elt nextLink)
      free (next nextLink)
      freeAll start end

-- TODO: document individual functions

-- | Given a linear computation that manages memory, run that computation.
withPool :: (Pool %1 -> Ur b) %1 -> Ur b
withPool scope = Unsafe.toLinear performScope scope
  where
    -- XXX: do ^ without `toLinear` by using linear IO

    performScope :: (Pool %1 -> Ur b) -> Ur b
    performScope scope' = unsafeDupablePerformIO $ do
      -- Initialise the pool
      backPtr <- malloc
      let end = DLL backPtr nullPtr nullPtr -- always at the end of the list
      start <- DLL nullPtr nullPtr <$> new end -- always at the start of the list
      poke backPtr start
      -- Run the computation
      evaluate (scope' (Pool start))
        `finally`
        -- Clean up remaining variables.
        (freeAll start end)

instance Consumable Pool where
  consume (Pool _) = ()

instance Dupable Pool where
  dupR (Pool l) = Data.pure (Pool l)

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

instance KnownRepresentable (Box a)

instance Representable (Box a) where
  type AsKnown (Box a) = Box a
  ofKnown = id
  toKnown = id

-- TODO: a way to store GC'd data using a StablePtr

-- TODO: reference counted pointer. Remarks: rc pointers are Dupable but not
-- Movable. In order to be useful, need some kind of borrowing on the values, I
-- guess. 'Box' can be realloced, but not RC pointers.

reprPoke :: forall a. (Representable a) => Ptr a -> a %1 -> IO ()
reprPoke ptr a
  | Dict <- storable @(AsKnown a) =
      Unsafe.toLinear (poke (castPtr ptr :: Ptr (AsKnown a))) (toKnown a)

reprNew :: forall a. (Representable a) => a %1 -> IO (Ptr a)
reprNew a =
  Unsafe.toLinear mkPtr a
  where
    -- XXX: should be improved by using linear IO
    mkPtr :: a -> IO (Ptr a)
    mkPtr a' | Dict <- storable @(AsKnown a) =
      do
        ptr0 <- malloc @(AsKnown a)
        let ptr = castPtr ptr0 :: Ptr a
        reprPoke ptr a'
        return ptr

-- TODO: Ideally, we would like to avoid having a boxed representation of the
-- data before a pointer is created. A better solution is to have a destination
-- passing-style API (but there is still some design to be done there). This
-- alloc primitive would then be derived (but most of the time we would rather
-- write bespoke constructors).

-- | Store a value @a@ on the system heap that is not managed by the GC.
alloc :: forall a. (Representable a) => a %1 -> Pool %1 -> Box a
alloc a (Pool pool) =
  Unsafe.toLinear mkPtr a
  where
    -- XXX: should be improved by using linear IO
    mkPtr :: a -> Box a
    mkPtr a' = unsafeDupablePerformIO $ do
      ptr <- reprNew a'
      poolPtr <- insertAfter pool (castPtr ptr :: Ptr ())
      return (Box poolPtr ptr)

-- TODO: would be better in linear IO, for we pretend that we are making an
-- unrestricted 'a', where really we are not.
reprPeek :: forall a. (Representable a) => Ptr a -> IO a
reprPeek ptr | Dict <- storable @(AsKnown a) = do
  knownRepr <- peek (castPtr ptr :: Ptr (AsKnown a))
  return (ofKnown knownRepr)

-- | Retrieve the value stored on system heap memory.
deconstruct :: (Representable a) => Box a %1 -> a
deconstruct (Box poolPtr ptr) = unsafeDupablePerformIO $
  mask_ $ do
    res <- reprPeek ptr
    delete =<< peek poolPtr
    free ptr
    free poolPtr
    return res
