{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | = The control functor hierarchy
--
-- The functors in this module are called control functors, which
-- are different from the data functors in @Data.Functor.Linear@.
--
-- This distinction and the use-cases of each group of functors is explained in
-- [this blog post](https://tweag.io/posts/2020-01-16-data-vs-control.html).
--
module Control.Functor.Linear
  ( -- * Control functor hierarchy
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
  , module Control.Functor.Internal.Linear.Instances
  ) where

import Control.Functor.Internal.Linear.Class
import Control.Functor.Internal.Linear.Reader
import Control.Functor.Internal.Linear.State
import Control.Functor.Internal.Linear.MonadTrans
import Control.Functor.Internal.Linear.Instances

-- $readerT
-- See [here](https://mmhaskell.com/monads/reader-writer) to learn about
-- the basics of reader monads. To know about the standard reader monad
-- functions, see the documentation of the standard reader monad
-- [here](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html).

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

