{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module provides a linear version of 'Semigroup'.
module Data.Monoid.Linear.Internal.Semigroup
  ( -- * Semigroup
    Semigroup(..)
    -- * Endo
  , Endo(..), appEndo
  , NonLinear(..)
  , module Data.Semigroup
  )
  where

import Prelude.Linear.Internal
import Data.Semigroup hiding (Semigroup(..), Endo(..))
import qualified Data.Semigroup as Prelude
import GHC.Types hiding (Any)

-- | A linear semigroup @a@ is a type with an associative binary operation @<>@
-- that linearly consumes two @a@s.
class Prelude.Semigroup a => Semigroup a where
  (<>) :: a %1-> a %1-> a

---------------
-- Instances --
---------------

instance Semigroup () where
  () <> () = ()

-- | An @Endo a@ is just a linear function of type @a %1-> a@.
-- This has a classic monoid definition with 'id' and '(.)'.
newtype Endo a = Endo (a %1-> a)
  deriving (Prelude.Semigroup) via NonLinear (Endo a)

-- TODO: have this as a newtype deconstructor once the right type can be
-- correctly inferred
-- | A linear application of an 'Endo'.
appEndo :: Endo a %1-> a %1-> a
appEndo (Endo f) = f

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
  (a,x) <> (b,y) = (a <> b, x <> y)

instance Semigroup a => Semigroup (Dual a) where
  Dual x <> Dual y = Dual (y <> x)

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

-- | DerivingVia combinator for Prelude.Semigroup given (linear) Semigroup.
-- For linear monoids, you should supply a Prelude.Monoid instance and either
-- declare an empty Monoid instance, or use DeriveAnyClass. For example:
--
-- > newtype Endo a = Endo (a %1-> a)
-- >   deriving (Prelude.Semigroup) via NonLinear (Endo a)
newtype NonLinear a = NonLinear a

instance Semigroup a => Prelude.Semigroup (NonLinear a) where
  NonLinear a <> NonLinear b = NonLinear (a <> b)

instance Semigroup Ordering where
    LT <> LT = LT
    LT <> GT = LT
    LT <> EQ = LT
    EQ <> y = y
    GT <> LT = GT
    GT <> GT = GT
    GT <> EQ = GT
    -- We can not use `lseq` above because of an import loop.
    -- So it's easier to just expand the cases here.

