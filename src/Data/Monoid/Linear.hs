{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides linear versions of 'Monoid' and related classes.
--
-- To learn about how these classic monoids work, go to this school of haskell
-- [post](https://www.schoolofhaskell.com/user/mgsloan/monoids-tour).

module Data.Monoid.Linear
  ( -- * Monoid operations
    Monoid(..)
  , mconcat
  -- * Semigroup operations
  , module Data.Semigroup.Linear
  )
  where

import Prelude.Linear.Internal
import Data.Semigroup.Linear
import GHC.Types hiding (Any)
import qualified Prelude

-- | A linear monoid is a linear semigroup with an identity on the binary
-- operation.
class (Semigroup a, Prelude.Monoid a) => Monoid a where
  {-# MINIMAL #-}
  mempty :: a
  mempty = Prelude.mempty
  -- convenience redefine

mconcat :: Monoid a => [a] %1-> a
mconcat (xs' :: [a]) = go mempty xs'
  where
    go :: a %1-> [a] %1-> a
    go acc [] = acc
    go acc (x:xs) = go (acc <> x) xs

---------------
-- Instances --
---------------

instance Prelude.Monoid (Endo a) where
  mempty = Endo id
instance Monoid (Endo a)

instance (Monoid a, Monoid b) => Monoid (a,b)

instance Monoid a => Monoid (Dual a)

instance Monoid Ordering where
    mempty = EQ
