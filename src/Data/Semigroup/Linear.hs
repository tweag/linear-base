{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | = The linear semigroup hierarchy
--
-- TODO: documentation

module Data.Semigroup.Linear
  ( Semigroup(..)
  , Monoid(..)
  , LEndo(..), appLEndo
  , module Data.Semigroup
  )
  where

import Prelude.Linear.Internal.Simple
import Data.Semigroup hiding (Semigroup(..))
import qualified Data.Semigroup as Prelude
import qualified Prelude
import qualified Unsafe.Linear as Unsafe

class Prelude.Semigroup a => Semigroup a where
  (<>) :: a ->. a ->. a

class (Semigroup a, Prelude.Monoid a) => Monoid a where
  {-# MINIMAL #-}
  mempty :: a
  mempty = mempty
  -- convenience redefine

---------------
-- Instances --
---------------

instance Semigroup () where
  () <> () = ()

newtype LEndo a = LEndo (a ->. a)

-- TODO: have this as a newtype deconstructor once the right type can be
-- correctly inferred
appLEndo :: LEndo a ->. a ->. a
appLEndo (LEndo f) = f

instance Prelude.Semigroup (LEndo a) where
  LEndo f <> LEndo g = LEndo (f . g)
instance Prelude.Monoid (LEndo a) where
  mempty = LEndo id
instance Semigroup (LEndo a) where
  LEndo f <> LEndo g = LEndo (f . g)
instance Monoid (LEndo a) where

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
  (a,x) <> (b,y) = (a <> b, x <> y)
instance (Monoid a, Monoid b) => Monoid (a,b)

instance Semigroup a => Semigroup (Dual a) where
  Dual x <> Dual y = Dual (y <> x)
instance Monoid a => Monoid (Dual a)

newtype LWrap a = LWrap a
  deriving (Prelude.Semigroup, Prelude.Monoid)

-- This instance is unsafe: do not export LWrap so it cannot be used.
instance Prelude.Semigroup a => Semigroup (LWrap a) where
  LWrap a <> LWrap b = LWrap (Unsafe.toLinear2 (Prelude.<>) a b)
instance Prelude.Monoid a => Monoid (LWrap a)

-- XXX: I think these are safe but I'm not fully confident
deriving via (LWrap (Sum a)) instance Prelude.Num a => Semigroup (Sum a)
deriving via (LWrap (Sum a)) instance Prelude.Num a => Monoid (Sum a)
deriving via (LWrap (Product a)) instance Prelude.Num a => Semigroup (Product a)
deriving via (LWrap (Product a)) instance Prelude.Num a => Monoid (Product a)

-- Bools are movable so this is fine
deriving via LWrap All instance Semigroup All
deriving via LWrap All instance Monoid All
deriving via LWrap Any instance Semigroup Any
deriving via LWrap Any instance Monoid Any
