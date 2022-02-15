{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Functor.Linear.Internal.State
  ( StateT (..),
    State,
    state,
    get,
    put,
    gets,
    modify,
    replace,
    runStateT,
    runState,
    mapStateT,
    mapState,
    execStateT,
    execState,
    withStateT,
    withState,
  )
where

import Control.Functor.Linear.Internal.Class
import Control.Functor.Linear.Internal.Instances (Data (..))
import Control.Functor.Linear.Internal.MonadTrans
import qualified Control.Monad as NonLinear ()
import qualified Control.Monad.Trans.State.Strict as NonLinear
import Data.Functor.Identity
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import Data.Unrestricted.Linear.Internal.Consumable
import Data.Unrestricted.Linear.Internal.Dupable
import Prelude.Linear.Internal

-- # StateT
-------------------------------------------------------------------------------

-- | A (strict) linear state monad transformer.
newtype StateT s m a = StateT (s %1 -> m (a, s))
  deriving (Data.Applicative) via Data (StateT s m)

-- We derive Data.Applicative and not Data.Functor since Data.Functor can use
-- weaker constraints on m than Control.Functor, while
-- Data.Applicative needs a Monad instance just like Control.Applicative.

type State s = StateT s Identity

get :: (Applicative m, Dupable s) => StateT s m s
get = state dup

put :: (Applicative m, Consumable s) => s %1 -> StateT s m ()
put = Data.void . replace

gets :: (Applicative m, Dupable s) => (s %1 -> a) %1 -> StateT s m a
gets f = state ((\(s1, s2) -> (f s1, s2)) . dup)

runStateT :: StateT s m a %1 -> s %1 -> m (a, s)
runStateT (StateT f) = f

state :: Applicative m => (s %1 -> (a, s)) %1 -> StateT s m a
state f = StateT (pure . f)

runState :: State s a %1 -> s %1 -> (a, s)
runState f = runIdentity' . runStateT f

mapStateT :: (m (a, s) %1 -> n (b, s)) %1 -> StateT s m a %1 -> StateT s n b
mapStateT r (StateT f) = StateT (r . f)

withStateT :: (s %1 -> s) %1 -> StateT s m a %1 -> StateT s m a
withStateT r (StateT f) = StateT (f . r)

execStateT :: Functor m => StateT s m () %1 -> s %1 -> m s
execStateT f = fmap (\((), s) -> s) . (runStateT f)

mapState :: ((a, s) %1 -> (b, s)) %1 -> State s a %1 -> State s b
mapState f = mapStateT (Identity . f . runIdentity')

withState :: (s %1 -> s) %1 -> State s a %1 -> State s a
withState = withStateT

execState :: State s () %1 -> s %1 -> s
execState f = runIdentity' . execStateT f

modify :: Applicative m => (s %1 -> s) %1 -> StateT s m ()
modify f = state $ \s -> ((), f s)

-- TODO: add strict version of `modify`

-- | @replace s@ will replace the current state with the new given state, and
-- return the old state.
replace :: Applicative m => s %1 -> StateT s m s
replace s = state $ (\s' -> (s', s))

-- # Instances of StateT
-------------------------------------------------------------------------------

instance Functor m => Functor (NonLinear.StateT s m) where
  fmap f (NonLinear.StateT x) = NonLinear.StateT $ \s -> fmap (\(a, s') -> (f a, s')) $ x s

instance Data.Functor m => Data.Functor (StateT s m) where
  fmap f (StateT x) = StateT (\s -> Data.fmap (\(a, s') -> (f a, s')) (x s))

instance Functor m => Functor (StateT s m) where
  fmap f (StateT x) = StateT (\s -> fmap (\(a, s') -> (f a, s')) (x s))

instance Monad m => Applicative (StateT s m) where
  pure x = StateT (\s -> return (x, s))
  StateT mf <*> StateT mx = StateT $ \s -> do
    (f, s') <- mf s
    (x, s'') <- mx s'
    return (f x, s'')

instance Monad m => Monad (StateT s m) where
  StateT mx >>= f = StateT $ \s -> do
    (x, s') <- mx s
    runStateT (f x) s'

instance MonadTrans (StateT s) where
  lift x = StateT (\s -> fmap (,s) x)
