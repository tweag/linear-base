module Control.Optics.Linear.Lens
  ( -- * Types
    Lens, Lens'
    -- * Composing lens
  , (.>)
    -- * Common optics
  , _1, _2
    -- * Using optics
  , get, set, gets, setSwap
  , over, over'
  , withLens
    -- * Constructing optics
  , lens
  )
where

import Control.Optics.Linear.Internal
