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
    -- ** Basic data types
    module Data.Bool.Linear
  , Prelude.Char
  , module Data.Maybe.Linear
  , module Data.Either.Linear
    -- * Tuples
  , Prelude.fst
  , Prelude.snd
  , curry
  , uncurry
    -- ** Basic type classes
  , module Data.Eq.Linear
  , module Data.Ord.Linear
  , Prelude.Enum (..)
  , Prelude.Bounded (..)
    -- ** Numbers
  , Prelude.Int
  , Prelude.Integer
  , Prelude.Float
  , Prelude.Double
  , Prelude.Rational
  , Prelude.Word
  , module Data.Num.Linear
  , Prelude.Real (..)
  , Prelude.Integral (..)
  , Prelude.Floating (..)
  , Prelude.Fractional (..)
  , Prelude.RealFrac (..)
  , Prelude.RealFloat (..)
    -- *** Numeric functions
  , Prelude.subtract
  , Prelude.even
  , Prelude.odd
  , Prelude.gcd
  , Prelude.lcm
  , (Prelude.^)
  , (Prelude.^^)
  , Prelude.fromIntegral
  , Prelude.realToFrac
    -- ** Monads and functors
  , (<*)
    -- ** Semigroups and monoids
  , module Data.Monoid.Linear
    -- ** Miscellaneous functions
  , id
  , const
  , (.)
  , flip
  , ($)
  , (&)
  , Prelude.until
  , asTypeOf
  , Prelude.error
  , Prelude.errorWithoutStackTrace
  , Prelude.undefined
  , seq
  , ($!)
    -- * List operations
  , module Data.List.Linear
    -- * Functions on strings
    -- TODO: Implement a linear counterpart of this
  , module Data.String
    -- * Converting to and from String
  , Prelude.ShowS
  , Prelude.Show (..)
  , Prelude.shows
  , Prelude.showChar
  , Prelude.showString
  , Prelude.showParen
  , Prelude.ReadS
  , Prelude.Read (..)
  , Prelude.reads
  , Prelude.readParen
  , Prelude.read
  , Prelude.lex
    -- * Basic input and output
  , Prelude.IO
  , Prelude.putChar
  , Prelude.putStr
  , Prelude.putStrLn
  , Prelude.print
  , Prelude.getChar
  , Prelude.getLine
  , Prelude.getContents
  , Prelude.interact
    -- ** Files
  , Prelude.FilePath
  , Prelude.readFile
  , Prelude.writeFile
  , Prelude.appendFile
  , Prelude.readIO
  , Prelude.readLn
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
  , dup3
  , forget
  ) where

import qualified Data.Functor.Linear as Data
import Data.Unrestricted.Linear
import Data.Monoid.Linear
import Data.Num.Linear
import Data.Bool.Linear
import Data.Either.Linear
import Data.Maybe.Linear
import Data.Ord.Linear
import Data.Tuple.Linear
import Data.List.Linear
import qualified Prelude
import GHC.Exts (FUN)
import Prelude.Linear.Internal
import Data.Eq.Linear
import Data.String

-- | Replacement for the flip function with generalized multiplicities.
flip :: FUN r (FUN p a (FUN q b c)) (FUN q b (FUN p a c))
flip f b a = f a b

-- | Linearly typed replacement for the standard '(Prelude.<*)' function.
(<*) :: (Data.Applicative f, Consumable b) => f a #-> f b #-> f a
fa <* fb = Data.fmap (flip lseq) fa Data.<*> fb
