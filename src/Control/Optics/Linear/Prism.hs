-- | This module provides linear prisms.
--
-- A @Prism s t a b@ is equivalent to @(s %1-> Either a t, b %1-> t)@ for some
-- /sum type/ @s@. In the non-polymorphic version, this is a @(s %1-> Either a
-- s, a %1-> s)@ which represents taking one case of a sum type and a way to
-- build the sum-type given that one case. A prism is a traversal focusing on
-- one branch or case that a sum type could be.
--
-- = Example
--
-- @
-- {-# LANGUAGE LinearTypes #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE GADTs #-}
--
-- import Control.Optics.Linear.Internal
-- import Prelude.Linear
-- import qualified Data.Functor.Linear as Data
--
-- -- We can use a prism to do operations on one branch of a sum-type
-- -- (This is a bit of a toy example since we could use @over@ for this.)
-- formatLicenceName :: PersonId %1-> PersonId
-- formatLicenceName personId =
--   case Data.fmap modLisc (match pIdLiscPrism personId) of
--     Left personId' -> personId'
--     Right lisc -> build pIdLiscPrism lisc
--   where
--     modLisc :: Licence %1-> Licence
--     modLisc (Licence nm x) = Licence (nm ++ "\n") x
--
-- data PersonId where
--   IdLicence :: Licence %1-> PersonId
--   SSN :: Int %1-> PersonId
--   BirthCertif :: String %1-> PersonId
--   -- And there could be many more constructors ...
--
-- -- A Licence is a name and number
-- data Licence = Licence String Int
--
-- pIdLiscPrism :: Prism' PersonId Licence
-- pIdLiscPrism = prism IdLicence decompose where
--   decompose :: PersonId %1-> Either PersonId Licence
--   decompose (IdLicence l) = Right l
--   decompose x = Left x
-- @
module Control.Optics.Linear.Prism
  ( -- * Types
    Prism,
    Prism',

    -- * Composing optics
    (.>),

    -- * Common optics
    _Left,
    _Right,
    _Just,
    _Nothing,

    -- * Using optics
    match,
    build,
    withPrism,

    -- * Constructing optics
    prism,
  )
where

import Control.Optics.Linear.Internal
