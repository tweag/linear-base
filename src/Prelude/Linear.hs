{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides a replacement for 'Prelude' with
-- support for linear programming via linear versions of
-- standard data types, functions and type classes.
--
-- A simple example:
--
-- > {-# LANGUAGE LinearTypes #-}
-- > {-# LANGUAGE NoImplicitPrelude #-}
-- > import Prelude.Linear
-- >
-- > makeInt :: Either Int Bool #-> Int
-- > makeInt = either id boolToInt
-- >
-- > boolToInt :: Bool #-> Int
-- > boolToInt False = 0
-- > boolToInt True = 1
--
-- This module is designed to be imported unqualifed.


module Prelude.Linear
  ( -- * Standard Types, Classes and Related Functions
    ($)
  , (&)
  , (<*)
  , foldl
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
    -- * Using 'Ur' values in linear code
    -- $ unrestricted
  , Ur(..)
  , unur
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
import Data.List.Linear
import Prelude hiding
  ( ($)
  , id
  , const
  , seq
  , curry
  , uncurry
  , flip
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
import GHC.Exts (FUN)
import Prelude.Linear.Internal

-- | Replacement for the flip function with generalized multiplicities.
flip :: FUN r (FUN p a (FUN q b c)) (FUN q b (FUN p a c))
flip f b a = f a b

-- | Linearly typed replacement for the standard '(Prelude.<*)' function.
(<*) :: (Data.Applicative f, Consumable b) => f a #-> f b #-> f a
fa <* fb = Data.fmap (flip lseq) fa Data.<*> fb
