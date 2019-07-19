{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Linear
  ( -- * Linear monad hierarchy
    -- $ monad
    Functor(..)
  , (<$>)
  , Applicative(..)
  , Monad(..)
  , MonadFail(..)
  , return
  , join
  ) where

import Prelude.Linear.Internal.Simple (id)
import Prelude (String)

-- $monad

-- TODO: explain that the category of linear function is self-enriched, and that
-- this is a hierarchy of enriched monads. In order to have some common
-- vocabulary.

-- There is also room for another type of functor where map has type `(a ->.b)
-- -> f a ->. f b`. `[]` and `Maybe` are such functors (they are regular
-- (endo)functors of the category of linear functions whereas `LFunctor` are
-- enriched functors). A Traversable hierarchy would start with non-enriched
-- functors.

-- TODO: make the laws explicit

-- | Enriched linear functors.
class Functor f where
  fmap :: (a ->. b) ->. f a ->. f b

(<$>) :: Functor f => (a ->. b) ->. f a ->. f b
(<$>) = fmap

-- | Enriched linear applicative functors
class Functor f => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  pure :: a ->. f a
  (<*>) :: f (a ->. b) ->. f a ->. f b
  (<*>) = liftA2 id
  liftA2 :: (a ->. b ->. c) ->. f a ->. f b ->. f c
  liftA2 f x y = f <$> x <*> y

-- | Enriched linear monads
class Applicative m => Monad m where
  {-# MINIMAL (>>=) #-}
  (>>=) :: m a ->. (a ->. m b) ->. m b
  (>>) :: m () ->. m a ->. m a
  m >> k = m >>= (\() -> k)

-- | Handles pattern-matching failure in do-notation. See 'Control.Monad.Fail'.
class Monad m => MonadFail m where
  fail :: String -> m a

{-# INLINE return #-}
return :: Monad m => a ->. m a
return x = pure x

join :: Monad m => m (m a) ->. m a
join action = action >>= id
