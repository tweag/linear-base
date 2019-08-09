{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | = The linear monoid hierarchy
--
-- TODO: documentation

module Data.Monoid.Linear
  ( Semigroup(..)
  , Monoid(..)
  , Endo(..), appEndo
  , module Data.Semigroup
  )
  where

import Prelude.Linear.Internal.Simple
import Data.Semigroup hiding (Semigroup(..), Endo(..))
import qualified Data.Semigroup as Prelude
import GHC.Types hiding (Any)
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
appEndo :: Endo a ->. a ->. a
appEndo (Endo f) = f

instance Prelude.Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)
instance Prelude.Monoid (Endo a) where
  mempty = Endo id
instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)
instance Monoid (Endo a) where

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
  (a,x) <> (b,y) = (a <> b, x <> y)
instance (Monoid a, Monoid b) => Monoid (a,b)

instance Semigroup a => Semigroup (Dual a) where
  Dual x <> Dual y = Dual (y <> x)
instance Monoid a => Monoid (Dual a)

instance Semigroup All where
  All False <> All False = All False
  All False <> All True = All False
  All True  <> All False = All False
  All True  <> All True = All True
instance Semigroup Any where
  Any False <> Any False = Any False
  Any False <> Any True = Any True
  Any True  <> Any False = Any True
  Any True  <> Any True = Any True
