-- | This module provides linear lenses
--
-- A @Lens s t a b@ is equivalent to a @(s \#-> (a,b \#-> t)@.  It is a way to
-- cut up an instance of a /product type/ @s@ into an @a@ and a way to take a
-- @b@ to fill the place of the @a@ in @s@ which yields a @t@. When @a=b@ and
-- @s=t@, this type is much more intuitive: @(s \#-> (a,a \#-> s))@.  This is a
-- traversal on exactly one @a@ in a @s@.
--
-- = Example
--
module Control.Optics.Linear.Lens
  ( -- * Types
    Lens, Lens'
    -- * Composing lens
  , (.>)
    -- * Common optics
  , _1, _2
    -- * Using optics
  , get, set, gets, setSwap
  , over, overU
  , reifyLens, withLens
    -- * Constructing optics
  , lens
  )
where

import Control.Optics.Linear.Internal
