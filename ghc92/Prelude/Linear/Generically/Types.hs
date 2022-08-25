{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Prior to GHC 9.4, linear-base defined its own versions of @Generically@ and 
-- @Generically1@. As a temporary workaround to enable compilation on both GHC 
-- 9.4 and 9.2, this module exposes linear-base's own implementations of those 
-- types, while the 9.4 version simply re-exports @Generics.Linear@.
module Prelude.Linear.Generically.Types
  ( Generically (..),
    Generically1 (..),
    module Generics.Linear
  )
where

import Generics.Linear

-- | A datatype whose instances are defined generically, using the
-- 'Generics.Linear.Generic' representation.
-- Generic instances can be derived via @'Generically' A@ using
-- @-XDerivingVia@.
newtype Generically a = Generically a

-- | A type whose instances are defined generically, using the
-- 'Generics.Linear.Generic1' representation. 'Generically1' is a higher-kinded
-- version of 'Generically'.
--
-- Generic instances can be derived for type constructors via
-- @'Generically1' F@ using @-XDerivingVia@.
newtype Generically1 f a = Generically1 (f a)
