-- | This module provides linear prisms
--
-- A @Prism s t a b@ is equivalent to @(s \#-> Either a t, b \#-> t)@ for some
-- /sum type/ @s@. In the non-polymorphic version, this is a @(s \#-> Either a
-- s, a \#-> s)@ which represents taking one case of a sum type and a way to
-- build the sum-type given that one case. A prism is a traversal focusing on
-- one branch or case that a sum type could be.
--
-- = Example
--
-- @
-- {-# LANGUAGE LinearTypes #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GADTs #-}
--
-- import Prelude hiding ((++))
-- import Data.List.Linear ((++))
-- import Prelude.Linear ((&))
-- import qualified Data.Functor.Linear as Data
--
-- -- We can use a prism to do operations on one branch of a sum-type
-- formatLiscenceName :: PersonId %1-> PersonId
-- formatLiscenceName personId =
--   Data.fmap modLisc (match pIdLiscPrism personId) & \case
--     Left personId' -> personId'
--     Right lisc -> build pIdLiscPrism lisc
--   where
--     modLisc :: Liscence %1-> Liscence
--     modLisc (Liscence nm x) = Liscence (nm ++ "\n") x
--
-- data PersonId where
--   IdLiscence :: Liscence %1-> PersonId
--   SSN :: Int %1-> PersonId
--   BirthCertif :: String %1-> PersonId
--   StateVoterId :: Int %1-> PersonId
--   CensusId :: Int %1-> PersonId
--
-- -- A Liscence is a name and number
-- data Liscence = Liscence String Int
--
-- pIdLiscPrism :: Prism' PersonId Liscence
-- pIdLiscPrism = prism IdLiscence decompose where
--   decompose :: PersonId %1-> Either PersonId Liscence
--   decompose (IdLiscence l) = Right l
--   decompose x = Left x
-- @
--
module Control.Optics.Linear.Prism
  ( -- * Types
    Prism, Prism'
    -- * Composing optics
  , (.>)
    -- * Common optics
  , _Left, _Right
  , _Just, _Nothing
    -- * Using optics
  , match, build
  , withPrism
    -- * Constructing optics
  , prism
  )
  where

import Control.Optics.Linear.Internal
