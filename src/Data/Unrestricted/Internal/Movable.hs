{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Unrestricted.Internal.Movable
  (
  -- * Movable
    Movable(..)
  -- * Generic deriving
  , GMovable
  , genericMove
  ) where

import Data.Unrestricted.Internal.Ur
import Data.Unrestricted.Internal.Dupable
import GHC.Types (Multiplicity (..))
import Generics.Linear
import Prelude.Linear.Generically
import Prelude.Linear.Internal
import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Prelude as Prelude
import qualified Data.Semigroup as Semigroup
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Exts (Char(..), Int(..), Word(..), Double(..), Float(..))
import Prelude (Ordering (..), Bool (..))
import qualified Unsafe.Linear as Unsafe

-- | Use @'Movable' a@ to represent a type which can be used many times even
-- when given linearly. Simple data types such as 'Bool' or @[]@ are 'Movable'.
-- Though, bear in mind that this typically induces a deep copy of the value.
--
-- Formally, @'Movable' a@ is the class of
-- [coalgebras](https://ncatlab.org/nlab/show/coalgebra+over+a+comonad) of the
-- 'Ur' comonad. That is
--
-- * @unur (move x) = x@
-- * @move \@(Ur a) (move \@a x) = fmap (move \@a) $ move \@a x@
--
-- Additionally, a 'Movable' instance must be compatible with its 'Dupable' parent instance. That is:
--
-- * @case move x of {Ur _ -> ()} = consume x@
-- * @case move x of {Ur x -> (x, x)} = dup2 x@
class Dupable a => Movable a where
  move :: a %1-> Ur a

-- -------------
-- Instances

instance Movable () where
  move () = Ur ()

instance Movable Bool where
  move True = Ur True
  move False = Ur False

deriving via Generically Char
  instance Movable Char
deriving via Generically Double
  instance Movable Double
deriving via Generically Float
  instance Movable Float
deriving via Generically Int
  instance Movable Int
deriving via Generically Word
  instance Movable Word

instance Movable Prelude.Ordering where
  move LT = Ur LT
  move GT = Ur GT
  move EQ = Ur EQ

instance (Movable a, Movable b) => Movable (a, b) where
  move (a, b) = (,) Data.<$> move a Data.<*> move b

instance (Movable a, Movable b, Movable c) => Movable (a, b, c) where
  move (a, b, c) = (,,) Data.<$> move a Data.<*> move b Data.<*> move c

instance Movable a => Movable (Prelude.Maybe a) where
  move (Prelude.Nothing) = Ur Prelude.Nothing
  move (Prelude.Just x) = Data.fmap Prelude.Just (move x)

instance (Movable a, Movable b) => Movable (Prelude.Either a b) where
  move (Prelude.Left a) = Data.fmap Prelude.Left (move a)
  move (Prelude.Right b) = Data.fmap Prelude.Right (move b)

instance Movable a => Movable [a] where
  move [] = Ur []
  move (a:l) = (:) Data.<$> move a Data.<*> move l

instance Movable a => Movable (NonEmpty a) where
  move (x :| xs) = (:|) Data.<$> move x Data.<*> move xs

instance Movable (Ur a) where
  move (Ur a) = Ur (Ur a)

-- Some stock instances
deriving newtype instance Movable a => Movable (Semigroup.Sum a)
deriving newtype instance Movable a => Movable (Semigroup.Product a)
deriving newtype instance Movable Semigroup.All
deriving newtype instance Movable Semigroup.Any


-- -------------
-- Generic deriving

instance (Generic a, GMovable (Rep a)) => Movable (Generically a) where
  move = Data.fmap (Generically . to) . gmove . from . unGenerically

genericMove :: (Generic a, GMovable (Rep a)) => a -> Ur a
genericMove = Data.fmap to . gmove . from

class GDupable f => GMovable f where
  gmove :: f p %1-> Ur (f p)
instance GMovable V1 where
  gmove = \case
instance GMovable U1 where
  gmove U1 = Ur U1
instance (GMovable f, GMovable g) => GMovable (f :+: g) where
  gmove (L1 a) = gmove a & \case (Ur x) -> Ur (L1 x)
  gmove (R1 a) = gmove a & \case (Ur x) -> Ur (R1 x)
instance (GMovable f, GMovable g) => GMovable (f :*: g) where
  gmove (a :*: b) = gmove a & \case
    (Ur x) -> gmove b & \case
      (Ur y) -> Ur (x :*: y)
instance Movable c => GMovable (K1 i c) where
  gmove (K1 c) = move c & \case (Ur x) -> Ur (K1 x)
instance GMovable f => GMovable (M1 i t f) where
  gmove (M1 a) = gmove a & \case (Ur x) -> Ur (M1 x)

instance GMovable (MP1 'Many f) where
  gmove (MP1 x) = Ur (MP1 x)
instance GMovable f => GMovable (MP1 'One f) where
  gmove (MP1 a) = gmove a & \case Ur x -> Ur (MP1 x)

instance GMovable UChar where
  gmove (UChar c) = Unsafe.toLinear (\x -> Ur (UChar x)) c

instance GMovable UDouble where
  gmove (UDouble c) = Unsafe.toLinear (\x -> Ur (UDouble x)) c

instance GMovable UFloat where
  gmove (UFloat c) = Unsafe.toLinear (\x -> Ur (UFloat x)) c

instance GMovable UInt where
  gmove (UInt c) = Unsafe.toLinear (\x -> Ur (UInt x)) c

instance GMovable UWord where
  gmove (UWord c) = Unsafe.toLinear (\x -> Ur (UWord x)) c
