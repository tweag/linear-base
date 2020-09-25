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
