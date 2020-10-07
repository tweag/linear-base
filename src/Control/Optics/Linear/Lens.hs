{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This module provides linear lenses.
--
-- A @Lens s t a b@ is equivalent to a @(s \#-> (a,b \#-> t)@.  It is a way to
-- cut up an instance of a /product type/ @s@ into an @a@ and a way to take a
-- @b@ to fill the place of the @a@ in @s@ which yields a @t@. When @a=b@ and
-- @s=t@, this type is much more intuitive: @(s \#-> (a,a \#-> s))@.  This is a
-- traversal on exactly one @a@ in a @s@.
--
-- = Example
--
-- @
-- {-# LANGUAGE LinearTypes #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
--
-- import Control.Optics.Linear.Internal
-- import Prelude.Linear
--
-- import Control.Optics.Linear.Internal
-- import Prelude.Linear
-- -- We can use a lens to, for instance, linearly modify a sub-piece in
-- -- a nested record
-- modPersonZip :: Person %1-> Person
-- modPersonZip = over (personLocL .> locZipL)  (\x -> x + 1)
--
-- -- A person has a name and location
-- data Person = Person String Location
--
-- -- A location is a zip code and address
-- data Location = Location Int String
--
-- personLocL :: Lens' Person Location
-- personLocL = lens (\(Person s l) -> (l, \l' -> Person s l'))
--
-- locZipL :: Lens' Location Int
-- locZipL = lens (\(Location i s) -> (i, \i' -> Location i' s))
-- @
--
module Control.Optics.Linear.Lens
  ( -- * Types
    Lens, Lens'
    -- * Composing lens
  , (.>)
    -- * Common optics
  , _1, _2
    -- * Using optics
  , get, set, gets, setSwap
  , over, overU
  , reifyLens, withLens
    -- * Constructing optics
  , lens
  )
where

import Control.Optics.Linear.Internal
