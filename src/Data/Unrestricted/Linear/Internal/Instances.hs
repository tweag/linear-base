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
import Data.Monoid.Linear
import Data.Replicator.Linear.Internal.Instances ()
import Data.Unrestricted.Linear.Internal.Consumable
import Data.Unrestricted.Linear.Internal.Dupable
import Data.Unrestricted.Linear.Internal.Movable
import Data.Unrestricted.Linear.Internal.Ur
import Data.V.Linear.Internal (V (..))
import qualified Data.V.Linear.Internal as V
import qualified Data.Vector as Vector
import GHC.Int
import GHC.TypeLits
import GHC.Word
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import qualified Prelude

-- | Newtype that must be used with @DerivingVia@ to get efficient 'Dupable'
-- and 'Consumable' implementations for 'Movable' types.
newtype AsMovable a = AsMovable a

instance (Movable a) => Movable (AsMovable a) where
  move (AsMovable x) =
    move x & \case
      Ur x' -> Ur (AsMovable x')

instance (Movable a) => Consumable (AsMovable a) where
  consume x =
    move x & \case
      Ur _ -> ()

instance (Movable a) => Dupable (AsMovable a) where
  dupR x =
    move x & \case
      Ur x' -> Data.pure x'

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

-- TODO: instances for longer primitive tuples
-- TODO: default instances based on the Generic framework

instance Consumable (V 0 a) where
  consume = V.consume

instance (KnownNat n, Consumable a) => Consumable (V n a) where
  consume (V xs) = consume (Unsafe.toLinear Vector.toList xs)

instance (KnownNat n, Dupable a) => Dupable (V n a) where
  dupR (V xs) =
    V . Unsafe.toLinear (Vector.fromListN (V.theLength @n))
      Data.<$> dupR (Unsafe.toLinear Vector.toList xs)

-- Some stock instances

newtype MovableMonoid a = MovableMonoid a
  deriving (Prelude.Semigroup, Prelude.Monoid)

instance (Movable a, Prelude.Semigroup a) => Semigroup (MovableMonoid a) where
  MovableMonoid a <> MovableMonoid b = MovableMonoid (combine (move a) (move b))
    where
      combine :: (Prelude.Semigroup a) => Ur a %1 -> Ur a %1 -> a
      combine (Ur x) (Ur y) = x Prelude.<> y

instance (Movable a, Prelude.Monoid a) => Monoid (MovableMonoid a) where
  mempty = MovableMonoid Prelude.mempty

instance Prelude.Show a => Prelude.Show (Ur a) where
  show (Ur x) = "Ur " Prelude.++ (Prelude.show x)
