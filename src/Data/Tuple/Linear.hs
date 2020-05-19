{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides linear functions commonly used on tuples

module Data.Tuple.Linear
  (
    fst
  , snd
  , swap
  )
  where

import Data.Unrestricted.Linear

fst :: Consumable b => (a,b) #-> a
fst (a,b) = lseq b a

snd :: Consumable a => (a,b) #-> b
snd (a,b) = lseq a b

swap :: (a,b) #-> (b,a)
swap (a,b) = (b,a)
