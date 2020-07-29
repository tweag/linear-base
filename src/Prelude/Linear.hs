{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- This module defines all the facilities you need when writing linear code. As
-- an analog to the original 'Prelude', it has the basic tools any haskell
-- program using @-XLinearHaskell@ will probably need.
--
-- This module is designed to be imported unqualifed.
--
-- A simple example:
--
-- > {-# LANGUAGE LinearTypes #-}
-- > import Prelude.Linear
-- >
-- > makeInt :: Either Int Bool #-> Int
-- > makeInt = either id boolToInt
-- >
-- > boolToInt :: Bool #-> Int
-- > boolToInt False = 0
-- > boolToInt True = 1
--
--

module Prelude.Linear
  ( -- * Standard Types, Classes and Related Functions
    ($)
  , (&)
  , (<*)
  , flip
  , foldl
  , foldr
  , const
  , id
  , seq
  , ($!)
  , curry
  , uncurry
  , (.)
  , forget
  , Semigroup(..)
  , Monoid(..)
    -- * Using 'Unrestricted' values in linear Code
    -- $ unrestricted
  , Unrestricted(..)
  , unUnrestricted
    -- * Doing non-linear operations inside linear functions
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
import Data.Foldable.Linear
import Prelude hiding
  ( ($)
  , id
  , const
  , seq
  , flip
  , ($!)
  , curry
  , uncurry
  , foldl
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

-- | Linearly typed replacement for the standard '(Prelude.<*)' function.
(<*) :: (Data.Applicative f, Consumable b) => f a #-> f b #-> f a
fa <* fb = Data.fmap (flip lseq) fa Data.<*> fb
