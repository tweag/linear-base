{-# LANGUAGE LinearTypes #-}

-- | This module contains some convenience functions on the 'Of' pair.
module Streaming.Pair
  (
    lazily
  , strictly
  , fst'
  , snd'
  , mapOf
  , _second
  ) where

import Streaming.Type
import qualified Control.Monad.Linear as Control


lazily :: Of a b #-> (a, b)
lazily (a :> b) = (a, b)

strictly :: (a, b) -> Of a b
strictly (a,b) = a :> b

fst' :: Of a b -> a
fst' (a :> _) = a

snd' :: Of a b #-> b
snd' (_ :> b) = b

mapOf :: (a -> b) -> Of a r #-> Of b r
mapOf f (a :> r) = (f a :> r)

_second :: Control.Functor f => (b -> f b') -> Of a b -> f (Of a b')
_second f (a :> b) = Control.fmap (a :>) (f b)

