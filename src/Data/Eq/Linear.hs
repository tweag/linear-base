{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a linear 'Eq' class for testing equality between
-- values, along with standard instances.
module Data.Eq.Linear
  (
    Eq(..)
  )
  where

import Data.Bool.Linear
import qualified Prelude as Ur
import qualified Unsafe.Linear as Unsafe

-- | Testing equality on values.
--
-- The laws are that (==) and (/=) are compatible
-- and (==) is an equivalence relation. So, for all @x@, @y@, @z@,
--
-- * @x == x@ always
-- * @x == y@ implies @y == x@
-- * @x == y@ and @y == z@ implies @x == z@
-- * @(x == y)@ ≌ @not (x /= y)@
--
class Eq a where
  {-# MINIMAL (==) | (/=) #-}
  (==) :: a #-> a #-> Bool
  x == y = not (x /= y)
  (/=) :: a #-> a #-> Bool
  x /= y = not (x == y)

-- This is sound because we consume all parts of a data type when we inspect
-- for equality
instance Ur.Eq a => Eq a where
  (==) = Unsafe.toLinear2 (Ur.==)
