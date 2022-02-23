{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module exports instances of Consumable, Dupable and Movable
--
-- We export instances in this module to avoid a circular dependence
-- and keep things clean. Movable depends on the defintion of Ur yet
-- many instances of Movable which we might have put in the module with
-- Movable depend on Ur. So, we just put the instances of Movable and the
-- other classes (for cleanness) in this module to avoid this dependence.
module Data.Unrestricted.Linear.Internal.Instances where

import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import Data.List.NonEmpty
import Data.Monoid.Linear
import Data.Replicator.Linear.Internal (Replicator (..))
import qualified Data.Replicator.Linear.Internal as Replicator
import Data.Replicator.Linear.Internal.Instances ()
import Data.Replicator.Linear.Internal.ReplicationStream (ReplicationStream (..))
import qualified Data.Replicator.Linear.Internal.ReplicationStream as ReplicationStream
import Data.Unrestricted.Linear.Internal.Consumable
import Data.Unrestricted.Linear.Internal.Dupable
import Data.Unrestricted.Linear.Internal.Movable
import Data.Unrestricted.Linear.Internal.Ur
import Data.V.Linear.Internal (V (..))
import qualified Data.V.Linear.Internal as V
import qualified Data.Vector as Vector
import GHC.Int
import GHC.TypeLits
import GHC.Types hiding (Any)
import GHC.Word
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import qualified Prelude

-- | Newtype that must be used with @DerivingVia@ to get efficient 'Dupable'
-- and 'Consumable' implementations for 'Movable' types.
newtype AsMovable a = AsMovable a

instance Movable a => Movable (AsMovable a) where
  move (AsMovable x) =
    move x & \case
      Ur x' -> Ur (AsMovable x')

instance Movable a => Consumable (AsMovable a) where
  consume x =
    move x & \case
      Ur _ -> ()

instance Movable a => Dupable (AsMovable a) where
  dupR x =
    move x & \case
      Ur x' -> Data.pure x'

instance Movable () where
  move () = Ur ()

instance Movable Bool where
  move True = Ur True
  move False = Ur False

instance Movable Int where
  -- /!\ 'Int#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int#' and using it several times. /!\
  move (I# i) = Unsafe.toLinear (\j -> Ur (I# j)) i

deriving via (AsMovable Int8) instance Consumable Int8

deriving via (AsMovable Int8) instance Dupable Int8

instance Movable Int8 where
  -- /!\ 'Int8#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int8#' and using it several times. /!\
  move (I8# i) = Unsafe.toLinear (\j -> Ur (I8# j)) i

deriving via (AsMovable Int16) instance Consumable Int16

deriving via (AsMovable Int16) instance Dupable Int16

instance Movable Int16 where
  -- /!\ 'Int16#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int16#' and using it several times. /!\
  move (I16# i) = Unsafe.toLinear (\j -> Ur (I16# j)) i

deriving via (AsMovable Int32) instance Consumable Int32

deriving via (AsMovable Int32) instance Dupable Int32

instance Movable Int32 where
  -- /!\ 'Int32#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int32#' and using it several times. /!\
  move (I32# i) = Unsafe.toLinear (\j -> Ur (I32# j)) i

deriving via (AsMovable Int64) instance Consumable Int64

deriving via (AsMovable Int64) instance Dupable Int64

instance Movable Int64 where
  -- /!\ 'Int64#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Int64#' and using it several times. /!\
  move (I64# i) = Unsafe.toLinear (\j -> Ur (I64# j)) i

instance Movable Double where
  -- /!\ 'Double#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Double#' and using it several times. /!\
  move (D# i) = Unsafe.toLinear (\j -> Ur (D# j)) i

deriving via (AsMovable Word) instance Dupable Word

instance Movable Word where
  -- /!\ 'Word#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word#' and using it several times. /!\
  move (W# i) = Unsafe.toLinear (\j -> Ur (W# j)) i

deriving via (AsMovable Word8) instance Consumable Word8

deriving via (AsMovable Word8) instance Dupable Word8

instance Movable Word8 where
  -- /!\ 'Word8#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word8#' and using it several times. /!\
  move (W8# i) = Unsafe.toLinear (\j -> Ur (W8# j)) i

deriving via (AsMovable Word16) instance Consumable Word16

deriving via (AsMovable Word16) instance Dupable Word16

instance Movable Word16 where
  -- /!\ 'Word16#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word16#' and using it several times. /!\
  move (W16# i) = Unsafe.toLinear (\j -> Ur (W16# j)) i

deriving via (AsMovable Word32) instance Consumable Word32

deriving via (AsMovable Word32) instance Dupable Word32

instance Movable Word32 where
  -- /!\ 'Word32#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word32#' and using it several times. /!\
  move (W32# i) = Unsafe.toLinear (\j -> Ur (W32# j)) i

deriving via (AsMovable Word64) instance Consumable Word64

deriving via (AsMovable Word64) instance Dupable Word64

instance Movable Word64 where
  -- /!\ 'Word64#' is an unboxed unlifted data-types, therefore it cannot have any
  -- linear values hidden in a closure anywhere. Therefore it is safe to call
  -- non-linear functions linearly on this type: there is no difference between
  -- copying an 'Word64#' and using it several times. /!\
  move (W64# i) = Unsafe.toLinear (\j -> Ur (W64# j)) i

instance Movable Char where
  move (C# c) = Unsafe.toLinear (\x -> Ur (C# x)) c

deriving via (AsMovable Ordering) instance Dupable Ordering

instance Movable Ordering where
  move LT = Ur LT
  move GT = Ur GT
  move EQ = Ur EQ

-- TODO: instances for longer primitive tuples
-- TODO: default instances based on the Generic framework

instance (Movable a, Movable b) => Movable (a, b) where
  move (a, b) = (,) Data.<$> move a Data.<*> move b

instance (Movable a, Movable b, Movable c) => Movable (a, b, c) where
  move (a, b, c) = (,,) Data.<$> move a Data.<*> move b Data.<*> move c

instance Consumable (V 0 a) where
  consume = V.consume

instance (KnownNat n, Consumable a) => Consumable (V n a) where
  consume (V xs) = consume (Unsafe.toLinear Vector.toList xs)

instance (KnownNat n, Dupable a) => Dupable (V n a) where
  dupR (V xs) =
    V . Unsafe.toLinear (Vector.fromListN (V.theLength @n))
      Data.<$> dupR (Unsafe.toLinear Vector.toList xs)

instance Movable a => Movable (Prelude.Maybe a) where
  move (Prelude.Nothing) = Ur Prelude.Nothing
  move (Prelude.Just x) = Data.fmap Prelude.Just (move x)

instance (Movable a, Movable b) => Movable (Prelude.Either a b) where
  move (Prelude.Left a) = Data.fmap Prelude.Left (move a)
  move (Prelude.Right b) = Data.fmap Prelude.Right (move b)

instance Movable a => Movable [a] where
  move [] = Ur []
  move (a : l) = (:) Data.<$> move a Data.<*> move l

instance Movable a => Movable (NonEmpty a) where
  move (x :| xs) = (:|) Data.<$> move x Data.<*> move xs

instance Movable (Ur a) where
  move (Ur a) = Ur (Ur a)

-- Some stock instances
deriving instance Movable a => Movable (Sum a)

deriving instance Movable a => Movable (Product a)

deriving instance Movable All

deriving instance Movable Any

newtype MovableMonoid a = MovableMonoid a
  deriving (Prelude.Semigroup, Prelude.Monoid)

instance (Movable a, Prelude.Semigroup a) => Semigroup (MovableMonoid a) where
  MovableMonoid a <> MovableMonoid b = MovableMonoid (combine (move a) (move b))
    where
      combine :: Prelude.Semigroup a => Ur a %1 -> Ur a %1 -> a
      combine (Ur x) (Ur y) = x Prelude.<> y

instance (Movable a, Prelude.Monoid a) => Monoid (MovableMonoid a) where
  mempty = MovableMonoid Prelude.mempty

instance Consumable (ReplicationStream a) where
  consume = ReplicationStream.consume

instance Dupable (ReplicationStream a) where
  dupR = Streamed . ReplicationStream.duplicate

instance Consumable (Replicator a) where
  consume = Replicator.consume

instance Dupable (Replicator a) where
  dupR = Replicator.duplicate
