{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides linear functions commonly used on tuples

module Data.Tuple.Linear
  (
    fst
  , snd
  , swap
  , curry
  , uncurry
  )
  where

import Prelude.Linear.Internal
import Data.Unrestricted.Linear

fst :: Consumable b => (a,b) %1-> a
fst (a,b) = lseq b a

snd :: Consumable a => (a,b) %1-> b
snd (a,b) = lseq a b

swap :: (a,b) %1-> (b,a)
swap (a,b) = (b,a)
