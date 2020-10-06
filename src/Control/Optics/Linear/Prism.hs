-- | This module provides linear prisms
--
-- A @Prism s t a b@ is equivalent to @(s \#-> Either a t, b \#-> t)@ for some
-- /sum type/ @s@. In the non-polymorphic version, this is a
-- @(s \#-> Either a s, a \#-> s)@ which represents taking one case of a sum type
-- and a way to build the sum-type given that one case. A prism is a
-- traversal focusing on one branch or case that a sum type could be.
--
-- = Example
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
