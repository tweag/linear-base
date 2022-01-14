-- | This module provides linear isomorphisms.
--
-- An @Iso a b s t@ is equivalent to a @(s %1-> a, b %1-> t)@.  In the simple
-- case of an @Iso' a s@, this is equivalent to inverse functions
-- @(s %1-> a, a %1-> s)@.  In the general case an @Iso a b s t@ means if you
-- have the isomorphisms @(a %1-> b, b %1-> a)@ and @(s %1-> t, t %1-> s)@, then
-- you can form isomorphisms between @s@, @t@, @a@ and @b@.
--
-- = Example
--
-- @
-- {-# LANGUAGE LinearTypes #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE GADTs #-}
--
-- import Control.Optics.Linear.Internal
-- import Prelude.Linear
-- import qualified Data.Functor.Linear as Data
--
-- -- A toy example of operating over two isomorphic linear types
-- closureFmap :: (a %1-> b) -> ClosureEither x a %1-> ClosureEither x b
-- closureFmap f = over isoEithers (Data.fmap f)
--
-- data ClosureEither a b where
--   CLeft :: x %1-> (x %1-> a) %1-> ClosureEither a b
--   CRight :: x %1-> (x %1-> b) %1-> ClosureEither a b
--
-- isoEithers ::
--   Iso (ClosureEither a b) (ClosureEither a b') (Either a b) (Either a b')
-- isoEithers = iso fromClosure fromEither
--   where
--     fromEither :: Either a b %1-> ClosureEither a b
--     fromEither (Left a) = CLeft () (\() -> a)
--     fromEither (Right b) = CRight () (\() -> b)
--
--     fromClosure :: ClosureEither a b %1-> Either a b
--     fromClosure (CLeft x f) = Left (f x)
--     fromClosure (CRight x f) = Right (f x)
-- @
module Control.Optics.Linear.Iso
  ( -- * Types
    Iso,
    Iso',

    -- * Composing optics
    (.>),

    -- * Common optics
    swap,
    assoc,

    -- * Using optics
    withIso,

    -- * Constructing optics
    iso,
  )
where

import Control.Optics.Linear.Internal
