{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Linear.Internal where

import Prelude.Linear.Internal.Simple
import Prelude (String)
import Data.Functor.Identity
import qualified Data.Functor.Linear.Internal as Data

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

-- | Enriched linear applicative functors
class (Data.Applicative f, Functor f) => Applicative f where
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

---------------------------
-- Convenience operators --
---------------------------

dataFmapDefault :: Functor f => (a ->. b) -> f a ->. f b
dataFmapDefault f = fmap f

dataPureDefault :: Applicative f => a -> f a
dataPureDefault x = pure x

{-# INLINE (<$>) #-}
(<$>) :: Functor f => (a ->. b) ->. f a ->. f b
(<$>) = fmap

{-# INLINE return #-}
return :: Monad m => a ->. m a
return x = pure x

join :: Monad m => m (m a) ->. m a
join action = action >>= id

-- | Convenience operator to define Applicative instances in terms of Monad
ap :: Monad m => m (a ->. b) ->. m a ->. m b
ap f x = f >>= (\f' -> fmap f' x)

-------------------
-- Miscellaneous --
-------------------

-- | Linearly typed replacement for the standard 'foldM' function.
foldM :: forall m a b. Monad m => (b ->. a ->. m b) -> b ->. [a] ->. m b
foldM f z0 xs = foldr f' return xs z0
  where
    f' :: a ->. (b ->. m b) ->. b ->. m b
    f' x k z = f z x >>= k

-----------------------------------------------
-- Deriving Data.XXX in terms of Control.XXX --
-----------------------------------------------

newtype Data f a = Data (f a)

instance Functor f => Data.Functor (Data f) where
  fmap f (Data x) = Data (fmap f x)

instance Applicative f => Data.Applicative (Data f) where
  pure x = Data (pure x)
  Data f <*> Data x = Data (f <*> x)
