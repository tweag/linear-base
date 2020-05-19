{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides Bifunctor and related classes.
--
-- == 'Bifunctor'
--
-- Use a bifunctor instance to map functions over data structures
-- that have two type paramaters @a@ and @b@ and could be have a
-- functor instance for either the @a@s or @b@s.
-- For instance, you might want to map a function on either the left
-- or right element of a @(Int, Bool)@:
--
-- > import Prelude.Linear
-- > import Data.Bifunctor.Linear
-- >
-- > -- Map over the second element
-- > negateRight :: (Int, Bool) #-> (Int, Bool)
-- > negateRight x = second not x
module Data.Bifunctor.Linear
  ( Bifunctor(..),
    SymmetricMonoidal(..),
  )
  where

import Prelude.Linear
import Data.Void

-- | The Bifunctor class
--
-- == Laws
--
-- If 'bimap' is supplied, then
-- @'bimap' 'id' 'id' = 'id'@
--
-- * If 'first' and 'second' are supplied, then
-- @
-- 'first' 'id' ≡ 'id'
-- 'second' 'id' ≡ 'id'
-- @
--
-- * If all are supplied, then
-- @'bimap' f g = 'first' f '.' 'second' g
class Bifunctor p where
  {-# MINIMAL bimap | (first , second) #-}
  bimap :: (a #-> b) -> (c #-> d) -> a `p` c #-> b `p` d
  bimap f g x = first f (second g x)
  {-# INLINE bimap #-}

  first :: (a #-> b) -> a `p` c #-> b `p` c
  first f = bimap f id
  {-# INLINE first #-}

  second :: (b #-> c) -> a `p` b #-> a `p` c
  second = bimap id
  {-# INLINE second #-}

instance Bifunctor (,) where
  bimap f g (x,y) = (f x, g y)
  first f (x,y) = (f x, y)
  second g (x,y) = (x, g y)

instance Bifunctor Either where
  bimap f g = either (Left . f) (Right . g)

-- | A SymmetricMonoidal class
--
-- This allows you to shuffle around a bifunctor nested in itself and swap the
-- places of the two types held in the bifunctor. For instance, for tuples:
--
--  * You can use @lassoc :: (a,(b,c)) #-> ((a,b),c)@ and then use 'first' to access the @a@
--  * You can use the dual, i.e., @ rassoc :: ((a,b),c) #-> (a,(b,c))@ and then 'second'
--  * You can swap the first and second values with @swap :: (a,b) #-> (b,a)@
--
--  == Laws
--
--  * @swap . swap = id@
--  * @rassoc . lassoc = id@
--  * @lassoc . rassoc = id@
--  * @second swap . rassoc . first swap = rassoc . swap . rassoc@
class Bifunctor m => SymmetricMonoidal (m :: * -> * -> *) (u :: *) | m -> u, u -> m where
  {-# MINIMAL swap, (rassoc | lassoc) #-}
  rassoc :: (a `m` b) `m` c #-> a `m` (b `m` c)
  rassoc = swap . lassoc . swap . lassoc . swap
  lassoc :: a `m` (b `m` c) #-> (a `m` b) `m` c
  lassoc = swap . rassoc . swap . rassoc . swap
  swap :: a `m` b #-> b `m` a
-- XXX: should unitors be added?
-- XXX: Laws don't seem minimial

instance SymmetricMonoidal (,) () where
  swap (x, y) = (y, x)
  rassoc ((x,y),z) = (x,(y,z))

instance SymmetricMonoidal Either Void where
  swap = either Right Left
  rassoc (Left (Left x)) = Left x
  rassoc (Left (Right x)) = (Right :: a #-> Either b a) (Left x)
  rassoc (Right x) = (Right :: a #-> Either b a) (Right x)
-- XXX: the above type signatures are necessary for certain older versions of
-- the compiler, and as such are temporary
