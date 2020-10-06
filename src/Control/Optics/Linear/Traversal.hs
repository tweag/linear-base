-- | This module provides linear traversals
--
--  Traversals provides a means of accessing several @a@s organized in some
--  structural way in an @s@, and a means of changing them to @b@s to create a
--  @t@. In very ordinary language, it's like walking or traversing the data
--  structure, going across cases and inside definitions. In more imaganitive
--  language, it's like selecting some specific @a@s by looking at each
--  constructor of a data definition and recursing on each non-base type.
--
-- = Example
--
module Control.Optics.Linear.Traversal
  ( -- * Types
    Traversal, Traversal'
    -- * Composing optics
  , (.>)
    -- * Common optics
  , traversed
    -- * Using optics
  , over, overU
  , traverseOf, traverseOfU
    -- * Constructing optics
  , traversal
  )
  where

import Control.Optics.Linear.Internal
