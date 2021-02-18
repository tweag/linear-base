{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Unrestricted.Internal.Consumable
  (
  -- * Consumable
    Consumable(..)
  , lseq
  , seqUnit
  , GConsumable(..)
  )
  where

import GHC.Generics
import qualified Unsafe.Linear as Unsafe

class Consumable a where
  consume :: a %1-> ()
  default consume :: (Generic a, GConsumable (Rep a)) => a %1-> ()
  consume a = gconsume (Unsafe.toLinear from a)

-- | Consume the unit and return the second argument.
-- This is like 'seq' but since the first argument is restricted to be of type
-- @()@ it is consumed, hence @seqUnit@ is linear in its first argument.
seqUnit :: () %1-> b %1-> b
seqUnit () b = b

-- | Consume the first argument and return the second argument.
-- This is like 'seq' but the first argument is restricted to be 'Consumable'.
lseq :: Consumable a => a %1-> b %1-> b
lseq a b = seqUnit (consume a) b

class GConsumable f where
  gconsume :: f x %1-> ()
