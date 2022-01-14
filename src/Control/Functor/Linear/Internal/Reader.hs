{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Control.Functor.Linear.Internal.Reader
  ( --  ReaderT monad transformer
    Reader,
    reader,
    runReader,
    mapReader,
    withReader,
    ReaderT (..),
    runReaderT,
    mapReaderT,
    withReaderT,
    ask,
    local,
    asks,
  )
where

import Control.Functor.Linear.Internal.Class
import Control.Functor.Linear.Internal.Instances ()
import Control.Functor.Linear.Internal.MonadTrans
import qualified Control.Monad as NonLinear ()
import qualified Control.Monad.Trans.Reader as NonLinear
import Data.Functor.Identity
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import Data.Unrestricted.Internal.Consumable
import Data.Unrestricted.Internal.Dupable
import Prelude.Linear.Internal (runIdentity', ($), (.))

-- # Linear ReaderT
-------------------------------------------------------------------------------

-- | A linear reader monad transformer.
-- This reader monad requires that use of the read-only state is explict.
--
-- The monad instance requires that @r@ be 'Dupable'.  This means that you
-- should use the linear reader monad just like the non-linear monad, except
-- that the type system ensures that you explicity use or discard the
-- read-only state (with the 'Consumable' instance).
newtype ReaderT r m a = ReaderT (r %1 -> m a)

-- XXX: Replace with a newtype deconstructor once it can be inferred as linear.

-- | Provide an intial read-only state and run the monadic computation in
-- a reader monad transformer
runReaderT :: ReaderT r m a %1 -> r %1 -> m a
runReaderT (ReaderT f) = f

instance Data.Functor m => Data.Functor (ReaderT r m) where
  fmap f = mapReaderT (Data.fmap f)

instance Functor m => Functor (ReaderT r m) where
  fmap f = mapReaderT (fmap f)

instance (Data.Applicative m, Dupable r) => Data.Applicative (ReaderT r m) where
  pure x = ReaderT $ \r -> lseq r (Data.pure x)
  ReaderT f <*> ReaderT x = ReaderT ((\(r1, r2) -> f r1 Data.<*> x r2) . dup)

instance (Applicative m, Dupable r) => Applicative (ReaderT r m) where
  pure x = ReaderT $ \r -> lseq r (pure x)
  ReaderT f <*> ReaderT x = ReaderT ((\(r1, r2) -> f r1 Data.<*> x r2) . dup)

instance (Monad m, Dupable r) => Monad (ReaderT r m) where
  ReaderT x >>= f = ReaderT ((\(r1, r2) -> x r1 >>= (\a -> runReaderT (f a) r2)) . dup)

type Reader r = ReaderT r Identity

ask :: Applicative m => ReaderT r m r
ask = ReaderT pure

withReaderT :: (r' %1 -> r) %1 -> ReaderT r m a %1 -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

local :: (r %1 -> r) %1 -> ReaderT r m a %1 -> ReaderT r m a
local = withReaderT

reader :: Monad m => (r %1 -> a) %1 -> ReaderT r m a
reader f = ReaderT (return . f)

runReader :: Reader r a %1 -> r %1 -> a
runReader m = runIdentity' . runReaderT m

mapReader :: (a %1 -> b) %1 -> Reader r a %1 -> Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity')

mapReaderT :: (m a %1 -> n b) %1 -> ReaderT r m a %1 -> ReaderT r n b
mapReaderT f m = ReaderT (f . runReaderT m)

withReader :: (r' %1 -> r) %1 -> Reader r a %1 -> Reader r' a
withReader = withReaderT

asks :: Monad m => (r %1 -> a) %1 -> ReaderT r m a
asks f = ReaderT (return . f)

instance Dupable r => MonadTrans (ReaderT r) where
  lift x = ReaderT (`lseq` x)

-- # Instances for nonlinear ReaderT
-------------------------------------------------------------------------------

instance Functor m => Functor (NonLinear.ReaderT r m) where
  fmap f (NonLinear.ReaderT g) = NonLinear.ReaderT $ \r -> fmap f (g r)

instance Applicative m => Applicative (NonLinear.ReaderT r m) where
  pure x = NonLinear.ReaderT $ \_ -> pure x
  NonLinear.ReaderT f <*> NonLinear.ReaderT x = NonLinear.ReaderT $ \r -> f r <*> x r

instance Monad m => Monad (NonLinear.ReaderT r m) where
  NonLinear.ReaderT x >>= f = NonLinear.ReaderT $ \r -> x r >>= (\a -> runReaderT' (f a) r)

-- XXX: Temporary, until newtype record projections are linear.
runReaderT' :: NonLinear.ReaderT r m a %1 -> r -> m a
runReaderT' (NonLinear.ReaderT f) = f

instance MonadTrans (NonLinear.ReaderT r) where
  lift x = NonLinear.ReaderT (\_ -> x)
