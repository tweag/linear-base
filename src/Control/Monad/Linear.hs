{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}


-- | This module defines the linear monad hierarchy and classic monad instances.
--
-- It's critical to know that the linear monad hierarchy is different from
-- the "Data.Control.Functor" hierarchy.
-- To understand the differences see this [blog post](https://www.tweag.io/posts/2020-01-16-data-vs-control.html).
--
-- == New to monads?
--
-- If you are unfamiliar with monads, we recommend [these](https://mmhaskell.com/monads/)
-- tutorials on functors, monads, reader and writer monads, state monads and
-- monad transformers for what's featured in this module. (Of course, there are
-- a plethora of resources on monads in the Haskell community; find what works
-- best for you!)
module Control.Monad.Linear
  ( -- * Linear monad hierarchy
    -- $monad
    Functor(..)
  , (<$>)
  , (<&>)
  , (<$)
  , dataFmapDefault
  , Applicative(..)
  , dataPureDefault
  , Monad(..)
  , return
  , join
  , ap
  , foldM
  , MonadFail(..)
  , Data(..)
  -- * Monad transformers
  -- ** ReaderT monad transformer
  -- $readerT
  , Reader, reader, runReader, mapReader, withReader
  , ReaderT(..), runReaderT, mapReaderT, withReaderT
  , ask, local, asks
  -- ** StateT monad
  -- $stateT
  , State, state, runState, execState, mapState, withState
  , StateT(..), runStateT, execStateT, mapStateT, withStateT
  , get, put, modify, gets
  , MonadTrans(..)
  ) where

import Control.Monad.Linear.Internal
import Data.Functor.Identity
import Data.Unrestricted.Linear
import Prelude.Linear.Internal ((.), ($))
import qualified Data.Functor.Internal.Functor as Data
import qualified Data.Functor.Internal.Applicative as Data


-- $stateT
-- This is a linear version of the standard state monad.
-- The linear arrows ensure that the state is threaded linearly through
-- functions of the form @a %1-> StateT s m a@. That is, when sequencing 
-- @f :: a %1-> StateT s m b@ and @g :: b %1-> StateT s m c@,
-- the type system enforces that state produced by $f$ is fed into @g@.
--
-- For this reason, there is only one way to define '(>>=)':
--
-- > instance Monad m => Applicative (StateT s m) where
-- > StateT mx >>= f = StateT $ \s -> do
-- >   (x, s') <- mx s
-- >   runStateT (f x) s'
--
-- To see examples and learn about all the standard state monad functions, see
-- [here](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html).
-- To learn the basics of the state monad, see
-- [here](https://mmhaskell.com/monads/state).
--


-- $readerT
-- See [here](https://mmhaskell.com/monads/reader-writer) to learn about
-- the basics of reader monads. To know about the standard reader monad
-- functions, see the documentation of the standard reader monad
-- [here](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html).

-- | A linear reader monad transformer.
-- This reader monad requires that use of the read-only state is explict.
--
-- The monad instance requires that @r@ be 'Dupable'.  This means that you
-- should use the linear reader monad just like the non-linear monad, except
-- that the type system ensures that you explicity use or discard the
-- read-only state (with the 'Consumable' instance).
newtype ReaderT r m a = ReaderT (r %1-> m a)

-- XXX: Replace with a newtype deconstructor once it can be inferred as linear.
-- | Provide an intial read-only state and run the monadic computation in 
-- a reader monad transformer
runReaderT :: ReaderT r m a %1-> r %1-> m a
runReaderT (ReaderT f) = f

instance Data.Functor m => Data.Functor (ReaderT r m) where
  fmap f = mapReaderT (Data.fmap f)

instance Functor m => Functor (ReaderT r m) where
  fmap f = mapReaderT (fmap f)

instance (Data.Applicative m, Dupable r) => Data.Applicative (ReaderT r m) where
  pure x = ReaderT $ \r -> lseq r (Data.pure x)
  ReaderT f <*> ReaderT x = ReaderT ((\(r1,r2) -> f r1 Data.<*> x r2) . dup)

instance (Applicative m, Dupable r) => Applicative (ReaderT r m) where
  pure x = ReaderT $ \r -> lseq r (pure x)
  ReaderT f <*> ReaderT x = ReaderT ((\(r1,r2) -> f r1 Data.<*> x r2) . dup)

instance (Monad m, Dupable r) => Monad (ReaderT r m) where
  ReaderT x >>= f = ReaderT ((\(r1,r2) -> x r1 >>= (\a -> runReaderT (f a) r2)) . dup)

type Reader r = ReaderT r Identity

ask :: Applicative m => ReaderT r m r
ask = ReaderT pure

withReaderT :: (r' %1-> r) %1-> ReaderT r m a %1-> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

local :: (r %1-> r) %1-> ReaderT r m a %1-> ReaderT r m a
local = withReaderT

reader :: Monad m => (r %1-> a) %1-> ReaderT r m a
reader f = ReaderT (return . f)

runReader :: Reader r a %1-> r %1-> a
runReader m = runIdentity' . runReaderT m

mapReader :: (a %1-> b) %1-> Reader r a %1-> Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity')

mapReaderT :: (m a %1-> n b) %1-> ReaderT r m a %1-> ReaderT r n b
mapReaderT f m = ReaderT (f . runReaderT m)

withReader :: (r' %1-> r) %1-> Reader r a %1-> Reader r' a
withReader = withReaderT

asks :: Monad m => (r %1-> a) %1-> ReaderT r m a
asks f = ReaderT (return . f)

instance Dupable r => MonadTrans (ReaderT r) where
  lift x = ReaderT (`lseq` x)

get :: (Applicative m, Dupable s) => StateT s m s
get = state dup

put :: (Applicative m, Consumable s) => s %1-> StateT s m ()
put = Data.void . replace

gets :: (Applicative m, Dupable s) => (s %1-> a) %1-> StateT s m a
gets f = state ((\(s1,s2) -> (f s1, s2)) . dup)

-- | Linearly typed replacement for the standard '(Prelude.<$)' function.
(<$) :: (Functor f, Consumable b) => a %1-> f b %1-> f a
a <$ fb = fmap (`lseq` a) fb
