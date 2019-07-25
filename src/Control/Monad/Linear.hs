{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Linear
  ( -- * Linear monad hierarchy
    -- $ monad
    Functor(..)
  , (<$>)
  , dataFmapDefault
  , Applicative(..)
  , dataPureDefault
  , Monad(..)
  , MonadFail(..)
  , return
  , join
  , ap
  , DataFromControl(..)
  ) where

import Prelude.Linear.Internal.Simple (id)
import Prelude (String)
import qualified Data.Functor.Linear as Data

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
class Data.Functor f => Functor f where
  fmap :: (a ->. b) ->. f a ->. f b

(<$>) :: Functor f => (a ->. b) ->. f a ->. f b
(<$>) = fmap

dataFmapDefault :: Functor f => (a ->. b) -> f a ->. f b
dataFmapDefault f = fmap f

-- | Enriched linear applicative functors
class (Data.Applicative f, Functor f) => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  pure :: a ->. f a
  (<*>) :: f (a ->. b) ->. f a ->. f b
  (<*>) = liftA2 id
  liftA2 :: (a ->. b ->. c) ->. f a ->. f b ->. f c
  liftA2 f x y = f <$> x <*> y

dataPureDefault :: Applicative f => a -> f a
dataPureDefault x = pure x

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

-- | Convenience operator to define Applicative instances in terms of Monad
ap :: Monad m => m (a ->. b) ->. m a ->. m b
ap f x = f >>= (\f' -> fmap f' x)

-- | DerivingVia combinators for Data.XXX in terms of Control.XXX
newtype DataFromControl f a = DataFromControl (f a)

instance Functor f => Data.Functor (DataFromControl f) where
  fmap f (DataFromControl x) = DataFromControl (fmap f x)

instance Applicative f => Data.Applicative (DataFromControl f) where
  pure x = DataFromControl (pure x)
  DataFromControl f <*> DataFromControl x = DataFromControl (f <*> x)
