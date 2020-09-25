module Control.Optics.Linear.Traversal
  ( -- * Types
    Traversal, Traversal'
    -- * Composing optics
  , (.>)
    -- * Common optics
  , traversed
    -- * Using optics
  , over, over'
  , traverseOf, traverseOf'
    -- * Constructing optics
  )
  where

import Control.Optics.Linear.Internal
