{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | This module defines vectors of known length which can hold linear values.
--
-- Having a known length matters with linear types, because many common vector
-- operations (like zip) are not total with linear types.
--
-- Make these vectors by giving any finite number of arguments to 'make'
-- and use them with 'elim':
--
-- >>> :set -XLinearTypes
-- >>> :set -XTypeApplications
-- >>> :set -XTypeInType
-- >>> :set -XTypeFamilies
-- >>> import Prelude.Linear
-- >>> import qualified Data.V.Linear as V
-- >>> :{
--  doSomething :: Int %1-> Int %1-> Bool
--  doSomething x y = x + y > 0
-- :}
--
-- >>> :{
--  isTrue :: Bool
--  isTrue = V.elim (build 4 9) doSomething
--    where
--      -- GHC can't figure out this type equality, so this is needed.
--      build :: Int %1-> Int %1-> V.V 2 Int
--      build = V.make @2 @Int
-- :}
--
-- A much more expensive library of vectors of known size (including matrices
-- and tensors of all dimensions) is the [@linear@ library on
-- Hackage](https://hackage.haskell.org/package/linear) (that's /linear/ in the
-- sense of [linear algebra](https://en.wikipedia.org/wiki/Linear_algebra),
-- rather than linear types).
module Data.V.Linear
  ( V
  , FunN
  , elim
  , make
  , iterate
  -- * Type-level utilities
  , caseNat
  ) where

import Data.V.Linear.Internal.V

{- Developers Note

@V@ is used /all over the place/, so it can be a bit tricky to
avoid module import cycles. Generally speaking:

Avoid importing much into the implementation of @V@.
-}
