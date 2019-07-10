{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}

module Data.Either.Linear where

import Prelude.Linear (id)
import qualified Data.Functor.Linear as Data

-- (Un-enriched) coproduct in the category of linear types
data Either a b where
  Left :: a ->. Either a b
  Right :: b ->. Either a b

either :: (a ->. c) -> (b ->. c) -> Either a b ->. c
either f _ (Left a)  = f a
either _ g (Right b) = g b

instance Data.Functor (Either a) where
  fmap _ (Left a)  = Left a
  fmap f (Right b) = Right (f b)

-- Initial object in the category of linear types.  (The one from Data.Void
-- doesn't work right now because absurd isn't linear).
data Void = Void (forall a. a)

absurd :: Void ->. a
absurd (Void x) = x

units :: Either a Void ->. a
units x = either id absurd x
-- has inverse Left :: a ->. Either a Void, witnesses that Void
-- is the unit of Either

kill :: (Void, a) ->. Void
kill (x, y) = killer y
  where killer :: a ->. Void
        killer = absurd x
