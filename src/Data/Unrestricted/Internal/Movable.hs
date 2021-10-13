{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import Data.Functor.Linear.Internal.Functor

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
-- Generic deriving

instance (Generic a, GMovable (Rep a)) => Movable (Generically a) where
  move = fmap (Generically . to) . gmove . from . unGenerically

genericMove :: (Generic a, GMovable (Rep a)) => a -> Ur a
genericMove = fmap to . gmove . from

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
