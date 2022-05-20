{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module provides a linear 'Eq' class for testing equality between
-- values, along with standard instances.
module Data.Ord.Linear.Internal.Eq
  ( Eq (..),
  )
where

import Data.Bool.Linear
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Unrestricted.Linear
import Data.Word (Word16, Word32, Word64, Word8)
import Prelude.Linear.Internal
import qualified Prelude

-- | Testing equality on values.
--
-- The laws are that (==) and (/=) are compatible
-- and (==) is an equivalence relation. So, for all @x@, @y@, @z@,
--
-- * @x == x@ always
-- * @x == y@ implies @y == x@
-- * @x == y@ and @y == z@ implies @x == z@
-- * @(x == y)@ ≌ @not (x /= y)@
class Eq a where
  {-# MINIMAL (==) | (/=) #-}
  (==) :: a %1 -> a %1 -> Bool
  x == y = not (x /= y)
  infix 4 == -- same fixity as base.==
  (/=) :: a %1 -> a %1 -> Bool
  x /= y = not (x == y)
  infix 4 /= -- same fixity as base./=

-- * Instances

instance Prelude.Eq a => Eq (Ur a) where
  Ur x == Ur y = x Prelude.== y
  Ur x /= Ur y = x Prelude./= y

instance (Consumable a, Eq a) => Eq [a] where
  [] == [] = True
  (x : xs) == (y : ys) = x == y && xs == ys
  xs == ys = (xs, ys) `lseq` False

instance (Consumable a, Eq a) => Eq (Prelude.Maybe a) where
  Prelude.Nothing == Prelude.Nothing = True
  Prelude.Just x == Prelude.Just y = x == y
  x == y = (x, y) `lseq` False

instance
  (Consumable a, Consumable b, Eq a, Eq b) =>
  Eq (Prelude.Either a b)
  where
  Prelude.Left x == Prelude.Left y = x == y
  Prelude.Right x == Prelude.Right y = x == y
  x == y = (x, y) `lseq` False

instance (Eq a, Eq b) => Eq (a, b) where
  (a, b) == (a', b') =
    a == a' && b == b'

instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  (a, b, c) == (a', b', c') =
    a == a' && b == b' && c == c'

instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d) where
  (a, b, c, d) == (a', b', c', d') =
    a == a' && b == b' && c == c' && d == d'

deriving via MovableEq () instance Eq ()

deriving via MovableEq Prelude.Int instance Eq Prelude.Int

deriving via MovableEq Prelude.Double instance Eq Prelude.Double

deriving via MovableEq Prelude.Bool instance Eq Prelude.Bool

deriving via MovableEq Prelude.Char instance Eq Prelude.Char

deriving via MovableEq Prelude.Ordering instance Eq Prelude.Ordering

deriving via MovableEq Int16 instance Eq Int16

deriving via MovableEq Int32 instance Eq Int32

deriving via MovableEq Int64 instance Eq Int64

deriving via MovableEq Int8 instance Eq Int8

deriving via MovableEq Word16 instance Eq Word16

deriving via MovableEq Word32 instance Eq Word32

deriving via MovableEq Word64 instance Eq Word64

deriving via MovableEq Word8 instance Eq Word8

newtype MovableEq a = MovableEq a

instance (Prelude.Eq a, Movable a) => Eq (MovableEq a) where
  MovableEq ar == MovableEq br =
    move (ar, br) & \(Ur (a, b)) ->
      a Prelude.== b

  MovableEq ar /= MovableEq br =
    move (ar, br) & \(Ur (a, b)) ->
      a Prelude./= b
