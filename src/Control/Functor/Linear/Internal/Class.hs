{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | This module contains all the classes eventually exported by
-- "Control.Functor.Linear". Together with related operations.
module Control.Functor.Linear.Internal.Class
  (
  -- * Functors
    Functor(..)
  , dataFmapDefault
  , (<$>)
  , (<&>)
  , (<$)
  -- * Applicative Functors
  , Applicative(..)
  , dataPureDefault
  -- * Monads
  , Monad(..)
  , MonadFail(..)
  , return
  , join
  , ap
  , foldM
  ) where

import Prelude (String)
import Prelude.Linear.Internal
import qualified Control.Monad as NonLinear ()
import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Linear.Internal.Applicative as Data
import Data.Unrestricted.Internal.Consumable


-- # Control Functors
-------------------------------------------------------------------------------

-- TODO: explain that the category of linear function is self-enriched, and that
-- this is a hierarchy of enriched monads. In order to have some common
-- vocabulary.

-- There is also room for another type of functor where map has type `(a %1->b)
-- -> f a %1-> f b`. `[]` and `Maybe` are such functors (they are regular
-- (endo)functors of the category of linear functions whereas `LFunctor` are
-- control functors). A Traversable hierarchy would start with non-control
-- functors.

-- TODO: make the laws explicit

-- | Control linear functors. The functor of type
-- @f a@ holds only one value of type @a@ and represents a computation
-- producing an @a@ with an effect. All control functors are data functors,
-- but not all data functors are control functors.
class Data.Functor f => Functor f where
  -- | Map a linear function @g@ over a control functor @f a@.
  -- Note that @g@ is used linearly over the single @a@ in @f a@.
  fmap :: (a %1-> b) %1-> f a %1-> f b

-- | Apply the control @fmap@ over a data functor.
dataFmapDefault :: Functor f => (a %1-> b) -> f a %1-> f b
dataFmapDefault f = fmap f

(<$>) :: Functor f => (a %1-> b) %1-> f a %1-> f b
(<$>) = fmap
{-# INLINE (<$>) #-}

-- |  @
--    ('<&>') = 'flip' 'fmap'
--    @
(<&>) :: Functor f => f a %1-> (a %1-> b) %1-> f b
(<&>) a f = f <$> a
{-# INLINE (<&>) #-}

-- | Linearly typed replacement for the standard '(Prelude.<$)' function.
(<$) :: (Functor f, Consumable b) => a %1-> f b %1-> f a
a <$ fb = fmap (`lseq` a) fb


-- # Control Applicatives
-------------------------------------------------------------------------------

-- | Control linear applicative functors. These represent effectful
-- computations that could produce continuations that can be applied with
-- '<*>'.
class (Data.Applicative f, Functor f) => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  -- | Inject (and consume) a value into an applicative control functor.
  pure :: a %1-> f a
  -- | Apply the linear function in a control applicative functor to the value
  -- of type @a@ in another functor. This is essentialy composing two effectful
  -- computations, one that produces a function @f :: a %1-> b@ and one that
  -- produces a value of type @a@ into a single effectful computation that
  -- produces a value of type @b@.
  (<*>) :: f (a %1-> b) %1-> f a %1-> f b
  (<*>) = liftA2 id
  -- | @liftA2 g@ consumes @g@ linearly as it lifts it
  -- over two functors: @liftA2 g :: f a %1-> f b %1-> f c@.
  liftA2 :: (a %1-> b %1-> c) %1-> f a %1-> f b %1-> f c
  liftA2 f x y = f <$> x <*> y

-- | Apply the control @pure@ over a data applicative.
dataPureDefault :: Applicative f => a -> f a
dataPureDefault x = pure x


-- # Control Monads
-------------------------------------------------------------------------------

-- | Control linear monads.
-- A linear monad is one in which you sequence linear functions in a context,
-- i.e., you sequence functions of the form @a %1-> m b@.
class Applicative m => Monad m where
  {-# MINIMAL (>>=) #-}
  -- | @x >>= g@ applies a /linear/ function @g@ linearly (i.e., using it
  -- exactly once) on the value of type @a@ inside the value of type @m a@
  (>>=) :: m a %1-> (a %1-> m b) %1-> m b
  (>>) :: (Consumable a) => m a %1-> m b %1-> m b
  m >> k = m >>= ((\() -> k) . consume)

-- | This class handles pattern-matching failure in do-notation.
-- See "Control.Monad.Fail" for details.
class Monad m => MonadFail m where
  fail :: String -> m a

return :: Monad m => a %1-> m a
return x = pure x
{-# INLINE return #-}

-- | Given an effect-producing computation that produces an effect-producing computation
-- that produces an @a@, simplify it to an effect-producing
-- computation that produces an @a@.
join :: Monad m => m (m a) %1-> m a
join action = action >>= id

-- | Use this operator to define Applicative instances in terms of Monad instances.
ap :: Monad m => m (a %1-> b) %1-> m a %1-> m b
ap f x = f >>= (\f' -> fmap f' x)

-- | Fold from left to right with a linear monad.
-- This is a linear version of 'NonLinear.foldM'.
foldM :: forall m a b. Monad m => (b %1-> a %1-> m b) -> b %1-> [a] %1-> m b
foldM _ i [] = return i
foldM f i (x:xs) = f i x >>= \i' -> foldM f i' xs

