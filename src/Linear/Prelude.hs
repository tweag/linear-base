{-# LANGUAGE NoImplicitPrelude #-}

module Linear.Prelude
    ( -- * Standard 'Prelude' function with linear types
      -- $linearized-prelude
      ($)
    , const
    , swap
      -- * Re-exports from the standard 'Prelude' for convenience
    , module Prelude
    ) where

-- Hides all the functions that we want to redefine
import Prelude hiding
  ( const
  , ($) -- XXX: Temporary as `($)` should get its typing rule directly from the
        -- type inference mechanism
  , const
  , swap )

-- $linearized-prelude

-- A note on implementation: to avoid silly mistakes, very easy functions are
-- simply reimplemented here. For harder function, we reuse the Prelude
-- definition and make an unsafe cast.

-- | Beware: '($)' is not compatible with the standard one because it is
-- higher-order and we don't have multiplicity polymorphism yet.
($) :: (a->.b) ->. a ->. b
($) f x = f x

infixr 0 $

const :: a ->. b -> a
const x _ = x

swap :: (a,b) ->. (b,a)
swap (x,y) = (y,x)
