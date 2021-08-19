{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This module contains all instances for VM
--
module Data.VM.Linear.Internal.Instances where

import Data.VM.Linear.Internal.VM
import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.LArray.Mutable.Unlifted.Linear as LArray
import Data.Unrestricted.Linear


-- # Instances of VM
-------------------------------------------------------------------------------

instance Data.Functor (VM n) where
  fmap f (VM xs) = VM (LArray.map f xs)

-- TODO: This requires an efficient 'zip', and which in turns requires
-- something similar to our Pull Arrays, but with linear elements.
--
--   instance Data.Applicative (V n)

-- TODO: This should be possible, but I could not find a way to implement
-- 'LArray.traverse'.
-- instance Data.Traversable (VM n)
--   traverse f (VM arr) =
--     (\(LArray.LArray arr') -> VM arr')
--       Data.<$> LArray.traverse f arr

instance Consumable a => Consumable (VM n a) where
  consume (VM xs) = xs `LArray.lseq` ()

-- TODO: Decide whether we should have Dupable in terms of `VM` or `V`. This
-- will require moving things around.
instance Dupable a => Dupable (VM n a) where
  -- TODO: There must be a better way to implement this using dupN.
  dup2 (VM xs) =
    (\(# x, y #) -> (VM x, VM y))
      (LArray.dup2 xs)

