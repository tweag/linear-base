{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Linear.Internal where

import Prelude.Linear.Internal.Simple
import Prelude (String)
import Data.Functor.Identity
import qualified Data.Functor.Linear.Internal as Data
import qualified Control.Monad.Trans.Reader as NonLinear
import qualified Control.Monad.Trans.State.Strict as Strict

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

------------------------------------------------
-- Instances for nonlinear monad transformers --
------------------------------------------------

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  Identity x >>= f = f x

-- | Temporary, until newtype record projections are linear.
runIdentity' :: Identity a ->. a
runIdentity' (Identity x) = x

instance Functor m => Functor (NonLinear.ReaderT r m) where
  fmap f (NonLinear.ReaderT g) = NonLinear.ReaderT $ \r -> fmap f (g r)
instance Applicative m => Applicative (NonLinear.ReaderT r m) where
  pure x = NonLinear.ReaderT $ \_ -> pure x
  NonLinear.ReaderT f <*> NonLinear.ReaderT x = NonLinear.ReaderT $ \r -> f r <*> x r
instance Monad m => Monad (NonLinear.ReaderT r m) where
  NonLinear.ReaderT x >>= f = NonLinear.ReaderT $ \r -> x r >>= (\a -> runReaderT' (f a) r)

-- | Temporary, until newtype record projections are linear.
runReaderT' :: NonLinear.ReaderT r m a ->. r -> m a
runReaderT' (NonLinear.ReaderT f) = f

instance Functor m => Functor (Strict.StateT s m) where
  fmap f (Strict.StateT x) = Strict.StateT $ \s -> fmap (\(a, s') -> (f a, s')) $ x s
