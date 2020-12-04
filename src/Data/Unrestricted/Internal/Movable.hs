{-# LANGUAGE LinearTypes #-}
module Data.Unrestricted.Internal.Movable
  (
  -- * Movable
    Movable(..)
  ) where

import Data.Unrestricted.Internal.Ur
import Data.Unrestricted.Internal.Dupable

-- | The laws of the @Movable@ class mean that @move@ is compatible with
-- @consume@ and @dup@.
--
-- * @case move x of {Ur _ -> ()} = consume x@
-- * @case move x of {Ur x -> x} = x@
-- * @case move x of {Ur x -> (x, x)} = dup2 x@
class Dupable a => Movable a where
  move :: a %1-> Ur a

