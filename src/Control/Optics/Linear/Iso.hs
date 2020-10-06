-- | This module provides linear isomorphisms
--
-- An @Iso a b s t@ is equivalent to a @(s \#-> a, b \#-> t)@.  In the simple
-- case of an @Iso' a s@, this is equivalent to inverse functions
-- @(s \#-> a, a \#-> s)@.  In the general case an @Iso a b s t@ means if you
-- have the isomorphisms @(a \#-> b, b \#-> a)@ and @(s \#-> t, t \#-> s)@, then
-- you can form isomorphisms between @s@, @t@, @a@ and @b@.
--
-- = Example
--
module Control.Optics.Linear.Iso
  ( -- * Types
    Iso, Iso'
    -- * Composing optics
  , (.>)
    -- * Common optics
  , swap, assoc
    -- * Using optics
  , withIso
    -- * Constructing optics
  , iso
  )
  where

import Control.Optics.Linear.Internal
