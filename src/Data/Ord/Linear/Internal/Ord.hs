{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Data.Ord.Linear.Internal.Ord
  ( Ord(..)
  , Ordering(..)
  , min
  , max
  )
  where

import Data.Ord.Linear.Internal.Eq
import qualified Prelude
import Prelude.Linear.Internal
import Data.Ord (Ordering(..))
import Data.Bool.Linear ( Bool (..), not )
import Data.Unrestricted.Linear
import Data.Monoid.Linear

-- | Linear Orderings
--
-- Linear orderings provide a strict order. The laws for @(<=)@ for
-- all \(a,b,c\):
--
-- * reflexivity: \(a \leq a \)
-- * antisymmetry: \((a \leq b) \land (b \leq a) \rightarrow (a = b) \)
-- * transitivity: \((a \leq b) \land (b \leq c) \rightarrow (a \leq c) \)
--
-- and these \"agree\" with @<@:
--
-- * @x <= y@ = @not (y > x)@
-- * @x >= y@ = @not (y < x)@
--
-- Unlike in the non-linear setting, a linear @compare@ doesn't follow from
-- @<=@ since it requires calls: one to @<=@ and one to @==@. However,
-- from a linear @compare@ it is easy to implement the others. Hence, the
-- minimal complete definition only contains @compare@.
class Eq a => Ord a where
  {-# MINIMAL compare #-}

  -- | @compare x y@ returns an @Ordering@ which is
  -- one of @GT@ (greater than), @EQ@ (equal), or @LT@ (less than)
  -- which should be understood as \"x is @(compare x y)@ y\".
  compare :: a %1-> a %1-> Ordering

  (<=) :: a %1-> a %1-> Bool
  x <= y = not (x > y)

  (<) :: a %1-> a %1-> Bool
  x < y = compare x y == LT

  (>) :: a %1-> a %1-> Bool
  x > y = compare x y == GT

  (>=) :: a %1-> a %1-> Bool
  x >= y = not (x < y)

  infix 4 <=, <, >, >=


-- | @max x y@ returns the larger input, or  'y'
-- in case of a tie.
max :: (Dupable a, Ord a) =>  a %1-> a %1-> a
max x y =
  dup2 x & \(x', x'') ->
    dup2 y & \(y', y'') ->
      if x' <= y'
      then x'' `lseq` y''
      else y'' `lseq` x''

-- | @min x y@ returns the smaller input, or 'y'
-- in case of a tie.
min :: (Dupable a, Ord a) =>  a %1-> a %1-> a
min x y =
  dup2 x & \(x', x'') ->
    dup2 y & \(y', y'') ->
      if x' <= y'
      then y'' `lseq` x''
      else x'' `lseq` y''

-- * Instances

instance Prelude.Ord a => Ord (Ur a) where
  Ur x `compare` Ur y = x `Prelude.compare` y

instance (Consumable a, Ord a) => Ord (Prelude.Maybe a) where
  Prelude.Nothing `compare` Prelude.Nothing = EQ
  Prelude.Nothing `compare` Prelude.Just y = y `lseq` LT
  Prelude.Just x `compare` Prelude.Nothing = x `lseq` GT
  Prelude.Just x `compare` Prelude.Just y = x `compare` y

instance (Consumable a, Consumable b, Ord a, Ord b)
  => Ord (Prelude.Either a b) where
  Prelude.Left x `compare` Prelude.Right y = (x, y) `lseq` LT
  Prelude.Right x `compare` Prelude.Left y = (x, y) `lseq` GT
  Prelude.Left x `compare` Prelude.Left y = x `compare` y
  Prelude.Right x `compare` Prelude.Right y = x `compare` y

instance (Consumable a, Ord a) => Ord [a] where
  {-# SPECIALISE instance Ord [Prelude.Char] #-}
  compare [] [] = EQ
  compare xs [] = xs `lseq` GT
  compare [] ys = ys `lseq` LT
  compare (x:xs) (y:ys) =
    compare x y & \case
      EQ -> compare xs ys
      res -> (xs, ys) `lseq` res

instance (Ord a, Ord b) => Ord (a, b) where
  (a, b) `compare` (a', b') =
    compare a a' <> compare b b'

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
  (a, b, c) `compare` (a', b', c') =
    compare a a' <> compare b b' <> compare c c'

instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d) where
  (a, b, c, d) `compare` (a', b', c', d') =
    compare a a' <> compare b b' <> compare c c' <> compare d d'

deriving via MovableOrd () instance Ord ()
deriving via MovableOrd Prelude.Int instance Ord Prelude.Int
deriving via MovableOrd Prelude.Double instance Ord Prelude.Double
deriving via MovableOrd Prelude.Bool instance Ord Prelude.Bool
deriving via MovableOrd Prelude.Char instance Ord Prelude.Char
deriving via MovableOrd Prelude.Ordering instance Ord Prelude.Ordering

newtype MovableOrd a = MovableOrd a

instance (Prelude.Eq a, Movable a) => Eq (MovableOrd a) where
  MovableOrd ar == MovableOrd br
    = move (ar, br) & \(Ur (a, b)) ->
        a Prelude.== b

  MovableOrd ar /= MovableOrd br
    = move (ar, br) & \(Ur (a, b)) ->
        a Prelude./= b

instance (Prelude.Ord a, Movable a) => Ord (MovableOrd a) where
  MovableOrd ar `compare` MovableOrd br
    = move (ar, br) & \(Ur (a, b)) ->
        a `Prelude.compare` b

