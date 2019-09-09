{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Linear
  ( -- * Linear monad hierarchy
    -- $monad
    Functor(..)
  , (<$>)
  , (<$)
  , dataFmapDefault
  , Applicative(..)
  , dataPureDefault
  , Monad(..)
  , MonadFail(..)
  , return
  , join
  , ap
  , Data(..)
  , foldM
  -- * Monad transformers
  -- ** ReaderT monad transformer
  , Reader, reader, runReader, mapReader, withReader
  , ReaderT(..), runReaderT, mapReaderT, withReaderT
  , ask, local, asks
  -- ** StateT monad
  , State, state, runState, execState, mapState, withState
  , StateT(..), runStateT, execStateT, mapStateT, withStateT
  , get, put, modify, gets
  , MonadTrans(..)
  ) where

import Control.Monad.Linear.Internal
import Data.Functor.Identity
import Data.Unrestricted.Linear
import Prelude.Linear.Internal.Simple ((.), ($))
import qualified Data.Functor.Linear.Internal as Data

newtype ReaderT r m a = ReaderT (r ->. m a)

-- XXX: Replace with a newtype deconstructor once it can be inferred as linear.
runReaderT :: ReaderT r m a ->. r ->. m a
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

withReaderT :: (r' ->. r) ->. ReaderT r m a ->. ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

local :: (r ->. r) ->. ReaderT r m a ->. ReaderT r m a
local = withReaderT

reader :: Monad m => (r ->. a) ->. ReaderT r m a
reader f = ReaderT (return . f)

runReader :: Reader r a ->. r ->. a
runReader m = runIdentity' . runReaderT m

mapReader :: (a ->. b) ->. Reader r a ->. Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity')

mapReaderT :: (m a ->. n b) ->. ReaderT r m a ->. ReaderT r n b
mapReaderT f m = ReaderT (f . runReaderT m)

withReader :: (r' ->. r) ->. Reader r a ->. Reader r' a
withReader = withReaderT

asks :: Monad m => (r ->. a) ->. ReaderT r m a
asks f = ReaderT (return . f)

instance Dupable r => MonadTrans (ReaderT r) where
  lift x = ReaderT (`lseq` x)


get :: (Applicative m, Dupable s) => StateT s m s
get = state dup

put :: (Applicative m, Consumable s) => s ->. StateT s m ()
put = void . replace

gets :: (Applicative m, Dupable s) => (s ->. a) ->. StateT s m a
gets f = state ((\(s1,s2) -> (f s1, s2)) . dup)

-- | Linearly typed replacement for the standard '(Prelude.<$)' function.
(<$) :: (Functor f, Consumable b) => a ->. f b ->. f a
a <$ fb = fmap (`lseq` a) fb
