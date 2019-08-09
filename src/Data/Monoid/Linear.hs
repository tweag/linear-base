{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | = The linear monoid hierarchy
--
-- TODO: documentation

module Data.Monoid.Linear
  ( Semigroup(..)
  , Monoid(..)
  , LEndo(..), appLEndo
  , module Data.Semigroup
  )
  where

import Prelude.Linear.Internal.Simple
import Data.Unrestricted.Linear
import Data.Functor.Linear as Data
import Data.Functor.Const
import Data.Semigroup hiding (Semigroup(..))
import qualified Data.Semigroup as Prelude
import qualified Prelude

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

newtype Endo a = Endo (a ->. a)

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

instance (Movable a, Prelude.Semigroup a) => Semigroup (LWrap a) where
  LWrap a <> LWrap b = LWrap (combine (move a) (move b))
    where combine :: Prelude.Semigroup a => Unrestricted a ->. Unrestricted a ->. a
          combine (Unrestricted x) (Unrestricted y) = x Prelude.<> y
instance (Movable a, Prelude.Monoid a) => Monoid (LWrap a)

deriving instance Consumable a => Consumable (Sum a)
deriving instance Dupable a => Dupable (Sum a)
deriving instance Movable a => Movable (Sum a)
deriving instance Consumable a => Consumable (Product a)
deriving instance Dupable a => Dupable (Product a)
deriving instance Movable a => Movable (Product a)
deriving instance Consumable All
deriving instance Dupable All
deriving instance Movable All
deriving instance Consumable Any
deriving instance Dupable Any
deriving instance Movable Any

deriving via (LWrap (Sum a)) instance (Movable a, Prelude.Num a) => Semigroup (Sum a)
deriving via (LWrap (Sum a)) instance (Movable a, Prelude.Num a) => Monoid (Sum a)
deriving via (LWrap (Product a)) instance (Movable a, Prelude.Num a) => Semigroup (Product a)
deriving via (LWrap (Product a)) instance (Movable a, Prelude.Num a) => Monoid (Product a)

deriving via LWrap All instance Semigroup All
deriving via LWrap All instance Monoid All
deriving via LWrap Any instance Semigroup Any
deriving via LWrap Any instance Monoid Any

instance Monoid x => Data.Applicative (Const x) where
  pure _ = Const mempty
  Const x <*> Const y = Const (x <> y)
