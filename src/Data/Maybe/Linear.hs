{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides linear functions on the standard 'Maybe' type.
module Data.Maybe.Linear
  ( Maybe (..)
  , maybe
  , fromMaybe
  , maybeToList
  , catMaybes
  , mapMaybe
  )
  where

import qualified Data.Functor.Linear as Linear
import Prelude (Maybe(..))

-- | @maybe b f m@ returns @(f a)@ where @a@ is in
-- @m@ if it exists and @b@ otherwise
maybe :: b -> (a %1-> b) -> Maybe a %1-> b
maybe x _ Nothing = x
maybe _ f (Just y) = f y

-- | @fromMaybe default m@ is the @a@ in
-- @m@ if it exists and the @default@ otherwise
fromMaybe :: a -> Maybe a %1-> a
fromMaybe a Nothing = a
fromMaybe _ (Just a') = a'

-- | @maybeToList m@ creates a singleton or an empty list
-- based on the @Maybe a@.
maybeToList :: Maybe a %1-> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- | @catMaybes xs@ discards the @Nothing@s in @xs@
-- and extracts the @a@s
catMaybes :: [Maybe a] %1-> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just a : xs) = a : catMaybes xs

-- | @mapMaybe f xs = catMaybes (map f xs)@
mapMaybe :: (a %1-> Maybe b) -> [a] %1-> [b]
mapMaybe f xs = catMaybes (Linear.fmap f xs)
