{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module provides a linear version of 'Semigroup'.
module Data.Monoid.Linear.Internal.Semigroup
  ( -- * Semigroup
    Semigroup (..),

    -- * Endo
    Endo (..),
    appEndo,

    -- * NonLinear newtype
    NonLinear (..),

    -- * Data.Semigroup reexports
    All (..),
    Any (..),
    First (..),
    Last (..),
    Dual (..),
    Sum (..),
    Product (..),
  )
where

import qualified Data.Functor.Compose as Functor
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Product as Functor
import qualified Data.Monoid as Monoid
import Data.Ord (Down (..))
import Data.Proxy (Proxy (..))
import Data.Semigroup
  ( All (..),
    Any (..),
    Dual (..),
    First (..),
    Last (..),
    Product (..),
    Sum (..),
  )
import qualified Data.Semigroup as Prelude
import qualified Data.Tuple.Linear.Compat as Tuple
import Data.Unrestricted.Linear.Internal.Consumable (Consumable, lseq)
import Data.Void (Void)
import GHC.Tuple
import GHC.Types hiding (Any)
import Prelude.Linear.Internal
import Prelude (Either (..), Maybe (..))

-- | A linear semigroup @a@ is a type with an associative binary operation @<>@
-- that linearly consumes two @a@s.
--
-- Laws (same as 'Data.Semigroup.Semigroup'):
--   * ∀ x ∈ G, y ∈ G, z ∈ G, x <> (y <> z) = (x <> y) <> z
class Semigroup a where
  (<>) :: a %1 -> a %1 -> a
  infixr 6 <> -- same fixity as base.<>

-- | An @'Endo' a@ is just a linear function of type @a %1-> a@.
-- This has a classic monoid definition with 'id' and '(.)'.
newtype Endo a = Endo (a %1 -> a)
  deriving (Prelude.Semigroup) via NonLinear (Endo a)

-- TODO: have this as a newtype deconstructor once the right type can be
-- correctly inferred

-- | A linear application of an 'Endo'.
appEndo :: Endo a %1 -> a %1 -> a
appEndo (Endo f) = f

-- | @DerivingVia@ combinator for 'Prelude.Semigroup' (resp. 'Prelude.Monoid')
-- given linear 'Semigroup' (resp. 'Monoid').
--
-- > newtype Endo a = Endo (a %1-> a)
-- >   deriving (Prelude.Semigroup) via NonLinear (Endo a)
newtype NonLinear a = NonLinear a

---------------
-- Instances --
---------------

instance Semigroup a => Prelude.Semigroup (NonLinear a) where
  NonLinear a <> NonLinear b = NonLinear (a <> b)

-- Instances below are listed in the same order as in https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Semigroup.html

instance Semigroup All where
  All False <> All False = All False
  All False <> All True = All False
  All True <> All False = All False
  All True <> All True = All True

instance Semigroup Any where
  Any False <> Any False = Any False
  Any False <> Any True = Any True
  Any True <> Any False = Any True
  Any True <> Any True = Any True

instance Semigroup Void where
  (<>) = \case {}

instance Semigroup Ordering where
  LT <> LT = LT
  LT <> GT = LT
  LT <> EQ = LT
  EQ <> y = y
  GT <> LT = GT
  GT <> GT = GT
  GT <> EQ = GT

instance Semigroup () where
  () <> () = ()

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Consumable a => Semigroup (Monoid.First a) where
  (Monoid.First Nothing) <> y = y
  x <> (Monoid.First y) =
    y & \case
      Nothing -> x
      Just y' -> y' `lseq` x

instance Consumable a => Semigroup (Monoid.Last a) where
  x <> (Monoid.Last Nothing) = x
  (Monoid.Last x) <> y =
    x & \case
      Nothing -> y
      Just x' -> x' `lseq` y

instance Semigroup a => Semigroup (Down a) where
  (Down x) <> (Down y) = Down (x <> y)

instance Consumable a => Semigroup (First a) where
  x <> (First y) = y `lseq` x

instance Consumable a => Semigroup (Last a) where
  (Last x) <> y = x `lseq` y

-- Cannot add instance Ord a => Semigroup (Max a); would require (NonLinear.Ord a, Consumable a)
-- Cannot add instance Ord a => Semigroup (Min a); would require (NonLinear.Ord a, Consumable a)

instance Semigroup a => Semigroup (Dual a) where
  Dual x <> Dual y = Dual (y <> x)

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

-- See Data.Num.Linear for instance ... => Semigroup (Product a)
-- See Data.Num.Linear for instance ... => Semigroup (Sum a)
-- See System.IO.Linear for instance ... => Semigroup (IO a)
-- See System.IO.Resource.Internal for instance ... => Semigroup (RIO a)
-- See Data.List.Linear for instance ... => Semigroup (NonEmpty a)

instance Semigroup a => Semigroup (Maybe a) where
  x <> Nothing = x
  Nothing <> y = y
  Just x <> Just y = Just (x <> y)

instance Semigroup a => Semigroup (Solo a) where
  x <> y = Tuple.mkSolo (Tuple.unSolo x <> Tuple.unSolo y)

-- See Data.List.Linear for instance ... => Semigroup [a]

instance (Consumable a, Consumable b) => Semigroup (Either a b) where
  Left x <> y = x `lseq` y
  x <> y =
    y & \case
      Left y' -> y' `lseq` x
      Right y' -> y' `lseq` x

-- Cannot add instance Semigroup a => Semigroup (Op a b); would require Dupable b

instance Semigroup (Proxy a) where
  Proxy <> Proxy = Proxy

-- Cannot add instance Semigroup a => Semigroup (ST s a); I think that it would require a linear ST monad
-- Cannot add instance Semigroup b => Semigroup (a -> b); would require Dupable a

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (x1, x2) <> (y1, y2) = (x1 <> y1, x2 <> y2)

instance Semigroup a => Semigroup (Const a b) where
  Const x <> Const y = Const (x <> y)

-- See Data.Functor.Linear.Applicative for instance ... => Semigroup (Ap f a)
-- Cannot add instance Alternative f => Semigroup (Alt f a); we don't have a linear Alternative

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
  (x1, x2, x3) <> (y1, y2, y3) = (x1 <> y1, x2 <> y2, x3 <> y3)

instance (Semigroup (f a), Semigroup (g a)) => Semigroup (Functor.Product f g a) where
  Functor.Pair x1 x2 <> Functor.Pair y1 y2 = Functor.Pair (x1 <> y1) (x2 <> y2)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a, b, c, d) where
  (x1, x2, x3, x4) <> (y1, y2, y3, y4) = (x1 <> y1, x2 <> y2, x3 <> y3, x4 <> y4)

instance (Semigroup (f (g a))) => Semigroup (Functor.Compose f g a) where
  Functor.Compose x <> Functor.Compose y = Functor.Compose (x <> y)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Semigroup (a, b, c, d, e) where
  (x1, x2, x3, x4, x5) <> (y1, y2, y3, y4, y5) = (x1 <> y1, x2 <> y2, x3 <> y3, x4 <> y4, x5 <> y5)
