{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Data.Array.Polarised where

import GHC.TypeLits
import Prelude.Linear

import Control.Exception (evaluate)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MVector
import GHC.Exts (RealWorld)
import qualified Prelude as Prelude
import System.IO.Unsafe
import qualified Unsafe.Linear as Unsafe

class Fillable (f :: Nat -> * -> *) a where
  empty :: f 0 a
  fillLoc :: Int -> a ->. f n a ->. f (n+1) a

newtype FillMonoid n a = Value a

instance Monoid a => Fillable FillMonoid a where
  empty = Value mempty
  fillLoc _ a (Value b) = Value (a <> b)

instance Fillable DArray a where

data PushArray n a where
  PushArray :: (forall b f. Fillable f b => (a ->. b) -> f n b) ->. PushArray n a

newtype DArray n a = DArray (MVector RealWorld a)

fillDArray :: n -> a ->. DArray n a ->. DArray (n+1) a

-- -- XXX: use of Vector in types is temporary. I will probably move away from
-- -- vectors and implement most stuff in terms of Array# and MutableArray#
-- -- eventually, anyway. This would allow to move the MutableArray logic to linear
-- -- IO, possibly, and segregate the unsafe casts to the Linear IO module.
-- alloc :: Int -> (DArray n a ->. ()) ->. Vector a
-- alloc n = Unsafe.toLinear unsafeAlloc
--   where
--     unsafeAlloc :: (DArray n a ->. ()) -> Vector a
--     unsafeAlloc build = unsafeDupablePerformIO Prelude.$ do
--       dest <- MVector.unsafeNew n
--       evaluate (build (DArray dest))
--       Vector.unsafeFreeze dest
--
-- replicate :: a -> DArray n a ->. ()
-- replicate a = Unsafe.toLinear unsafeReplicate
--   where
--     unsafeReplicate (DArray ds) = unsafeDupablePerformIO Prelude.$ do
--       temp <- MVector.replicate (MVector.length ds) a
--       -- Note that it is indeed linear to compute the length of a MVector and
--       -- keep it unchanged. Therefore, syntax apart, it is true that this
--       -- function is linear.
--       MVector.unsafeCopy ds temp
--     -- XXX: this allocation is unnecessary, it's just a good short-cut for an
--     -- initial implementation.
--
-- -- | Caution, @'fill' a dest@ will fail is @dest@ isn't of length exactly one.
-- fill :: a ->. DArray 1 a ->. ()
-- fill = Unsafe.toLinear2 unsafeFill
--     -- XXX: we will probably be able to spare this unsafe cast given a (linear)
--     -- length function on destination.
--   where
--     unsafeFill a (DArray ds) =
--       if MVector.length ds /= 1 then
--         error "Destination.fill: requires a destination of size 1"
--       else
--         unsafeDupablePerformIO Prelude.$ MVector.write ds 0 a
--
-- -- | @'split' n dest = (destl, destr)@ such as @destl@ has length @n@.
-- --
-- -- 'split' is total: if @n@ is larger than the length of @dest@, then @destr@ is
-- -- empty.
-- split :: Int -> DArray a ->. (DArray a, DArray a)
-- split n = Unsafe.toLinear unsafeSplit
--   where
--     unsafeSplit (DArray ds) =
--       let (dsl, dsr) = MVector.splitAt n ds in
--         (DArray dsl, DArray dsr)
--
-- -- | Assumes both arrays have the same size
-- mirror :: forall a b. Vector a -> (a ->. b) -> DArray b ->. ()
-- mirror as f ds
--   | Vector.length as == 0 = fill (f $ as Vector.! 0) ds
--   | otherwise =
--       mirrorHeadAndTail (Vector.head as) (Vector.tail as) (split 1 ds)
--   where
--     -- /!\ The first DArray has length 1.
--     mirrorHeadAndTail :: a -> Vector a -> (DArray b, DArray b) ->. ()
--     mirrorHeadAndTail x xs (dl, dr) =
--       fill (f x) dl `lseq` mirror xs f dr
