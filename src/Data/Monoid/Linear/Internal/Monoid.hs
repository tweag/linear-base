{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module provides linear versions of 'Monoid'.
--
-- To learn about how these classic monoids work, go to this school of haskell
-- [post](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour).
module Data.Monoid.Linear.Internal.Monoid
  ( -- * Monoid operations
    Monoid (..),
    mconcat,
    mappend,
    -- Cannot export Data.Monoid.{First,Last} because of the name clash with Data.Semigroup.{First,Last}
  )
where

import Data.Functor.Compose (Compose (Compose))
import qualified Data.Functor.Compose as Functor
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity (Identity))
import Data.Functor.Product (Product (Pair))
import qualified Data.Functor.Product as Functor
import qualified Data.Monoid as Monoid
import Data.Monoid.Linear.Internal.Semigroup
import Data.Ord (Down (Down))
import Data.Proxy (Proxy (Proxy))
import Data.Unrestricted.Linear.Internal.Consumable (Consumable)
import qualified Data.Unrestricted.Linear.Internal.Ur as Ur
import GHC.Types hiding (Any)
import Prelude.Linear.Internal
import Prelude (Maybe (Nothing))
import qualified Prelude

-- | A linear monoid is a linear semigroup with an identity on the binary
-- operation.
--
-- Laws (same as 'Data.Monoid.Monoid'):
--   * ∀ x ∈ G, x <> mempty = mempty <> x = x
class (Semigroup a) => Monoid a where
  {-# MINIMAL mempty #-}
  mempty :: a

instance (Prelude.Semigroup a, Monoid a) => Prelude.Monoid (NonLinear a) where
  mempty = NonLinear mempty

-- convenience redefine

mconcat :: (Monoid a) => [a] %1 -> a
mconcat (xs' :: [a]) = go mempty xs'
  where
    go :: a %1 -> [a] %1 -> a
    go acc [] = acc
    go acc (x : xs) = go (acc <> x) xs

mappend :: (Monoid a) => a %1 -> a %1 -> a
mappend = (<>)

---------------
-- Instances --
---------------

instance Prelude.Monoid (Endo a) where
  mempty = Endo id

-- Instances below are listed in the same order as in https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Monoid.html

instance Monoid All where
  mempty = All True

instance Monoid Any where
  mempty = Any False

instance Monoid Ordering where
  mempty = EQ

instance Monoid () where
  mempty = ()

instance (Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance (Consumable a) => Monoid (Monoid.First a) where
  mempty = Monoid.First Nothing

instance (Consumable a) => Monoid (Monoid.Last a) where
  mempty = Monoid.Last Nothing

instance (Monoid a) => Monoid (Down a) where
  mempty = Down mempty

-- Cannot add instance (Ord a, Bounded a) => Monoid (Max a); would require (NonLinear.Ord a, Consumable a)
-- Cannot add instance (Ord a, Bounded a) => Monoid (Min a); would require (NonLinear.Ord a, Consumable a)

instance (Monoid a) => Monoid (Dual a) where
  mempty = Dual mempty

instance Monoid (Endo a) where
  mempty = Endo id

-- See Data.Num.Linear for instance ... => Monoid (Product a)
-- See Data.Num.Linear for instance ... => Monoid (Sum a)
-- See System.IO.Linear for instance ... => Monoid (IO a)
-- See System.IO.Resource.Internal for instance ... => Monoid (RIO a)

instance (Semigroup a) => Monoid (Maybe a) where
  mempty = Nothing

-- See Data.List.Linear for instance ... => Monoid [a]
-- Cannot add instance Monoid a => Monoid (Op a b); would require Dupable b

instance Monoid (Proxy a) where
  mempty = Proxy

-- Cannot add instance Monoid a => Monoid (ST s a); I think that it would require a linear ST monad
-- Cannot add instance Monoid b => Monoid (a -> b); would require Dupable a

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance (Monoid a) => Monoid (Const a b) where
  mempty = mempty

-- See Data.Functor.Linear.Applicative for instance ... => Monoid (Ap f a)
-- Cannot add instance Alternative f => Monoid (Alt f a); we don't have a linear Alternative

instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c) where
  mempty = (mempty, mempty, mempty)

instance (Monoid (f a), Monoid (g a)) => Monoid (Functor.Product f g a) where
  mempty = Pair mempty mempty

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a, b, c, d) where
  mempty = (mempty, mempty, mempty, mempty)

instance (Monoid (f (g a))) => Monoid (Functor.Compose f g a) where
  mempty = Compose mempty

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) => Monoid (a, b, c, d, e) where
  mempty = (mempty, mempty, mempty, mempty, mempty)

-- | Useful to treat /unrestricted/ monoids as linear ones.
instance (Prelude.Monoid a) => Monoid (Ur.Ur a) where
  mempty = Ur.Ur Prelude.mempty
  {-# INLINE mempty #-}
