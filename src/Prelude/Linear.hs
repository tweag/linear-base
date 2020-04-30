{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.Linear
  ( -- * Standard 'Prelude' function with linear types
    -- $linearized-prelude
    ($)
  , (<*)
  , foldr
  , const
  , id
  , seq
  , curry
  , uncurry
  , (.)
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
  , module Data.Num.Linear
  , module Data.Either.Linear
  , module Data.Bool.Linear
  , module Data.Maybe.Linear
    -- * Re-exports from the standard 'Prelude' for convenience
  , module Prelude
  ) where

import qualified Data.Functor.Linear as Data
import Data.Unrestricted.Linear
import Data.Monoid.Linear
import Data.Num.Linear
import Data.Bool.Linear
import Data.Either.Linear
import Data.Maybe.Linear
import Prelude hiding
  ( ($)
  , id
  , const
  , seq
  , curry
  , uncurry
  , flip
  , foldr
  , (.)
  , maybe
  , either
  , (||)
  , (&&)
  , not
  , Functor(..)
  , Applicative(..)
  , Monad(..)
  , Traversable(..)
  , Semigroup(..)
  , Monoid(..)
  , Num(..)
  )
import Prelude.Linear.Internal.Simple

-- XXX: temporary: with multiplicity polymorphism functions expecting a
-- non-linear arrow would allow a linear arrow passed, so this would be
-- redundant
-- | Convenience operator when a higher-order function expects a non-linear
-- arrow but we have a linear arrow
forget :: (a #-> b) #-> a -> b
forget f x = f x

-- | Replacement for the flip function with generalized multiplicities.
flip :: (a -->.(p) b -->.(q) c) -->.(r) b -->.(q) a -->.(p) c
flip f b a = f a b

-- | Linearly typed replacement for the standard '(Prelude.<*)' function.
(<*) :: (Data.Applicative f, Consumable b) => f a #-> f b #-> f a
fa <* fb = Data.fmap (flip lseq) fa Data.<*> fb
