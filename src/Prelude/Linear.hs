{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Linear
  ( -- * Standard 'Prelude' function with linear types
    -- $linearized-prelude
    ($)
  , const
  , id
  , seq
  , curry
  , uncurry
  , (.)
  , either
  , maybe
  , forget
  , Semigroup(..)
  , Monoid(..)
    -- * Unrestricted
    -- $ unrestricted
  , Unrestricted(..)
  , unUnrestricted
    -- * Typeclasses for non-linear actions
    -- $ comonoid
  , Consumable(..)
  , Dupable(..)
  , Movable(..)
  , void
  , lseq
  , dup
  , dup2
  , dup3
    -- * Re-exports from the standard 'Prelude' for convenience
  , module Prelude
  ) where

import Data.Unrestricted.Linear
import Data.Monoid.Linear
import Prelude hiding
  ( ($)
  , id
  , const
  , seq
  , curry
  , uncurry
  , either
  , maybe
  , (.)
  , Functor(..)
  , Applicative(..)
  , Monad(..)
  , Traversable(..)
  , Semigroup(..)
  , Monoid(..)
  )
import Prelude.Linear.Internal.Simple

-- | Linearly typed replacement for the standard `either` function, to allow
-- linear consumption of an @Either@.
either :: (a ->. c) -> (b ->. c) -> Either a b ->. c
either f _ (Left x) = f x
either _ g (Right y) = g y

-- | Linearly typed replacement for the standard `maybe` function, to allow
-- linear consumption of a @Maybe@.
maybe :: b -> (a ->. b) -> Maybe a ->. b
maybe x _ Nothing = x
maybe _ f (Just y) = f y

-- XXX: temporary: with multiplicity polymorphism functions expecting a
-- non-linear arrow would allow a linear arrow passed, so this would be
-- redundant
-- | Convenience operator when a higher-order function expects a non-linear
-- arrow but we have a linear arrow
forget :: (a ->. b) ->. a -> b
forget f x = f x
