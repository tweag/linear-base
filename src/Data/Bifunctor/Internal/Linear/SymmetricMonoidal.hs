{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

module Data.Bifunctor.Internal.Linear.SymmetricMonoidal
  ( SymmetricMonoidal(..)
  ) where

import Data.Bifunctor.Internal.Linear.Bifunctor
import Prelude.Linear
import Data.Kind (Type)
import Data.Void


-- | A SymmetricMonoidal class
--
-- This allows you to shuffle around a bifunctor nested in itself and swap the
-- places of the two types held in the bifunctor. For instance, for tuples:
--
--  * You can use @lassoc :: (a,(b,c)) %1-> ((a,b),c)@ and then use 'first' to access the @a@
--  * You can use the dual, i.e., @ rassoc :: ((a,b),c) %1-> (a,(b,c))@ and then 'second'
--  * You can swap the first and second values with @swap :: (a,b) %1-> (b,a)@
--
--  == Laws
--
--  * @swap . swap = id@
--  * @rassoc . lassoc = id@
--  * @lassoc . rassoc = id@
--  * @second swap . rassoc . first swap = rassoc . swap . rassoc@
class Bifunctor m => SymmetricMonoidal (m :: Type -> Type -> Type) (u :: Type)
    | m -> u, u -> m where
  {-# MINIMAL swap, (rassoc | lassoc) #-}
  rassoc :: (a `m` b) `m` c %1-> a `m` (b `m` c)
  rassoc = swap . lassoc . swap . lassoc . swap
  lassoc :: a `m` (b `m` c) %1-> (a `m` b) `m` c
  lassoc = swap . rassoc . swap . rassoc . swap
  swap :: a `m` b %1-> b `m` a
-- XXX: should unitors be added?
-- XXX: Laws don't seem minimial


-- # Instances
-------------------------------------------------------------------------------

instance SymmetricMonoidal (,) () where
  swap (x, y) = (y, x)
  rassoc ((x,y),z) = (x,(y,z))

instance SymmetricMonoidal Either Void where
  swap = either Right Left
  rassoc (Left (Left x)) = Left x
  rassoc (Left (Right x)) = (Right :: a %1-> Either b a) (Left x)
  rassoc (Right x) = (Right :: a %1-> Either b a) (Right x)
-- XXX: the above type signatures are necessary for certain older versions of
-- the compiler, and as such are temporary

