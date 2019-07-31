{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Bifunctor.Linear where

import Data.Void

-- TODO: Is a bifunctor
-- TODO: associators
-- TODO: remove `swap` from Prelude

-- | Symmetric monoidal products on the category of linear types
--
-- Laws
-- * @swap . swap = id@
class SymmetricMonoidal (m :: * -> * -> *) (u :: *) | m -> u, u -> m where
  swap :: a `m` b ->. b `m` a

instance SymmetricMonoidal (,) () where
  swap (x, y) = (y, x)

instance SymmetricMonoidal Either Void where
  swap (Left x) = Right x
  swap (Right x) = Left x
