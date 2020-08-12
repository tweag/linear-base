{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ord.Linear
  ( Ord(..)
  , Ordering(..)
  , min
  , max
  , compare
  )
  where

import Data.Eq.Linear
import qualified Prelude as Unrestricted
import Prelude (Bool(..))
import Data.Ord (Ordering(..))
import Data.Bool.Linear ( not )
import Data.Unrestricted.Linear
import qualified Unsafe.Linear as Unsafe

{-

== Design notes on linear orderings.

@compare@ can't be a part of a linear ordering typeclass. Unlike in the
non-linear setting, a linear @compare@ doesn't follow from @<=@ since you need
two calls: one to @<=@ and one to @==@.

So what should the typeclass have, @(<)@ or @(<=)@?

Observe that by using @not@ these /linear/ functions are equivalent (i.e., you
can use one to implement the other and satisfy all axioms, even with linear
types for the conversions).

 * @(<)@  \( \iff \) @(>=)@
 * @(>)@  \( \iff \) @(<=)@

Thus, you can't separate a class with @(<)@ from one with @<=@. Further, this
equivalence shows that a minimal definition is one function from each bullet
point. (And thus there are 4 minimal definitions.)

-}

-- | Linear Orderings
--
-- Linear orderings provide a strict order but they do not
-- implement @compare@ because that requires two uses of a linear
-- @(<=)@. The laws for @(<=)@ for all \(a,b,c\):
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
class Eq a => Ord a where
  {-# MINIMAL (<), (>) | (<=), (>=) | (<), (<=) | (>), (>=) #-}
  (<=) :: a #-> a #-> Bool
  x <= y = not (x > y)

  (<) :: a #-> a #-> Bool
  x < y = not (x >= y)

  (>) :: a #-> a #-> Bool
  x > y = not (x <= y)

  (>=) :: a #-> a #-> Bool
  x >= y = not (x < y)

-- NOTE: the unsafe linear coercing in the three functions below makes sense
-- ONLY because any type satisfying these constraints has a linear ord
-- instance, (from the instance in this file).  Hence, with a linear @Ord@
-- instance and @Dupable@, there is a purely linear implementation of this
-- function (which is sometimes less efficient than the one in base, and
-- hence we unsafely coerce).

-- | @max x y@ returns the largest input
max :: (Dupable a, Unrestricted.Ord a) =>  a #-> a #-> a
max = Unsafe.toLinear2 (Unrestricted.max)

-- | @min x y@ returs the smallest input
min :: (Dupable a, Unrestricted.Ord a) => a #-> a #-> a
min = Unsafe.toLinear2 (Unrestricted.min)

-- | @compare x y@ returns an @Ordering@ which is
-- one of @GT@ (greater than), @EQ@ (equal), or @LT@ (less than)
-- which should be understood as \"x is @(compare x y)@ y\".
compare :: (Dupable a, Unrestricted.Ord a) => a #-> a #-> Ordering
compare = Unsafe.toLinear2 Unrestricted.compare

-- Note: the reason we coerce the unrestricted implementation
-- is because it's optimized for many base types
instance (Dupable a, Unrestricted.Ord a) => Ord a where
  (<) = Unsafe.toLinear2 (Unrestricted.<)
  (>) = Unsafe.toLinear2 (Unrestricted.>)
  (>=) = Unsafe.toLinear2 (Unrestricted.>=)
  (<=) = Unsafe.toLinear2 (Unrestricted.<=)
