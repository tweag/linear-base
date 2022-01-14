{-# LANGUAGE LinearTypes #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Unrestricted.Internal.Consumable
  ( -- * Consumable
    Consumable (..),
    lseq,
    seqUnit,
  )
where

class Consumable a where
  consume :: a %1 -> ()

-- | Consume the unit and return the second argument.
-- This is like 'seq' but since the first argument is restricted to be of type
-- @()@ it is consumed, hence @seqUnit@ is linear in its first argument.
seqUnit :: () %1 -> b %1 -> b
seqUnit () b = b

-- | Consume the first argument and return the second argument.
-- This is like 'seq' but the first argument is restricted to be 'Consumable'.
lseq :: Consumable a => a %1 -> b %1 -> b
lseq a b = seqUnit (consume a) b
