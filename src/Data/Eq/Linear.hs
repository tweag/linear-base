{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides a linear 'Eq' class for testing equality between
-- values, along with standard instances.
module Data.Eq.Linear
  ( Eq(..)
  )
  where

import Data.Bool.Linear
import qualified Prelude
import Data.Unrestricted.Linear

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

deriving via MovableEq () instance Eq ()
deriving via MovableEq Prelude.Int instance Eq Prelude.Int
deriving via MovableEq Prelude.Double instance Eq Prelude.Double

newtype MovableEq a = MovableEq a

instance (Prelude.Eq a, Movable a) => Eq (MovableEq a) where
  MovableEq ar == MovableEq br
    | Ur a <- move ar , Ur b <- move br
    = a Prelude.== b

  MovableEq ar /= MovableEq br
    | Ur a <- move ar , Ur b <- move br
    = a Prelude./= b
