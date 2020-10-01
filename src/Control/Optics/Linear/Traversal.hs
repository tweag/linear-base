module Control.Optics.Linear.Traversal
  ( -- * Types
    Traversal, Traversal', TraversalU, TraversalU'
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
