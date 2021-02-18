{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Unrestricted.Internal.Movable
  (
  -- * Movable
    Movable(..)
  , GMovable(..)
  ) where

import GHC.Generics
import Data.Unrestricted.Internal.Ur
import Data.Unrestricted.Internal.Dupable
import qualified Unsafe.Linear as Unsafe
import Prelude.Linear.Internal ((&))


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
  default move :: (Generic a, GMovable (Rep a)) => a %1-> Ur a
  move a = gmove (Unsafe.toLinear from a) & \case (Ur fx) -> Ur (to fx)

class GMovable f where
  gmove :: f x %1-> Ur (f x)
