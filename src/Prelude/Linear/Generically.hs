{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Linear.Generically
  ( Generically (..),
    unGenerically,
    Generically1 (..),
    unGenerically1,
  )
where

-- | A datatype whose instances are defined generically, using the
-- 'Generics.Linear.Generic' representation.
-- Generic instances can be derived via @'Generically' A@ using
-- @-XDerivingVia@.
newtype Generically a = Generically a

unGenerically :: Generically a %1 -> a
unGenerically (Generically a) = a

-- | A type whose instances are defined generically, using the
-- 'Generics.Linear.Generic1' representation. 'Generically1' is a higher-kinded
-- version of 'Generically'.
--
-- Generic instances can be derived for type constructors via
-- @'Generically1' F@ using @-XDerivingVia@.
newtype Generically1 f a = Generically1 (f a)

unGenerically1 :: Generically1 f a %1 -> f a
unGenerically1 (Generically1 fa) = fa
