{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Bifunctor.Internal.Linear.Bifunctor
  ( Bifunctor(..)
  ) where

import Prelude.Linear


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
  bimap :: (a %1-> b) -> (c %1-> d) -> a `p` c %1-> b `p` d
  bimap f g x = first f (second g x)
  {-# INLINE bimap #-}

  first :: (a %1-> b) -> a `p` c %1-> b `p` c
  first f = bimap f id
  {-# INLINE first #-}

  second :: (b %1-> c) -> a `p` b %1-> a `p` c
  second = bimap id
  {-# INLINE second #-}


-- # Instances
-------------------------------------------------------------------------------

instance Bifunctor (,) where
  bimap f g (x,y) = (f x, g y)
  first f (x,y) = (f x, y)
  second g (x,y) = (x, g y)

instance Bifunctor Either where
  bimap f g = either (Left . f) (Right . g)

