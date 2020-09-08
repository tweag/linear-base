{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides all functions which produce a
-- 'Stream (Of a) m r' from some given non-stream inputs.
module Streaming.Produce
  ( -- * Constructing 'Stream's
    yield
  , each'
  , unfoldr
  , fromHandle
  , readFile
  , replicate
  , replicateM
  , untilRight
  ) where

import Streaming.Type
import Streaming.Process
import Prelude.Linear (($), (&))
import Prelude (Either(..), Read, Bool(..), FilePath,
               Num(..), Int, otherwise, Eq(..), Ord(..), (.))
import qualified Prelude
import qualified Control.Monad.Linear as Control
import Control.Monad.Linear.Builder (BuilderType(..), monadBuilder)
import Data.Unrestricted.Linear
import System.IO.Linear
import System.IO.Resource
import qualified System.IO as System
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Stack


-- # The Stream Constructors
-------------------------------------------------------------------------------

{-| A singleton stream

>>> stdoutLn $ yield "hello"
hello

>>> S.sum $ do {yield 1; yield 2; yield 3}
6 :> ()

-}
yield :: Control.Monad m => a -> Stream (Of a) m ()
yield x = Step $ x :> Return ()
{-# INLINE yield #-}

{- | Stream the elements of a pure, foldable container.

>>> S.print $ each' [1..3]
1
2
3

-}
each' :: Control.Monad m => [a] -> Stream (Of a) m ()
each' xs = Prelude.foldr (\a stream -> Step $ a :> stream) (Return ()) xs
{-# INLINABLE each' #-}

{-| Build a @Stream@ by unfolding steps starting from a seed. In particular note
    that @S.unfoldr S.next = id@.

-}
unfoldr :: Control.Monad m =>
  (s #-> m (Either r (Unrestricted a, s))) -> s #-> Stream (Of a) m r
unfoldr step s = unfoldr' step s
  where
    Builder{..} = monadBuilder
    unfoldr' :: Control.Monad m =>
      (s #-> m (Either r (Unrestricted a, s))) -> s #-> Stream (Of a) m r
    unfoldr' step s = Effect $ step s Control.>>= \case
      Left r -> return $ Return r
      Right (Unrestricted a,s') -> return $ Step $ a :> unfoldr step s'
{-# INLINABLE unfoldr #-}

-- Note: we use the RIO monad from linear base to enforce
-- the protocol of file handles and file I/O
fromHandle :: Handle #-> Stream (Of Text) RIO ()
fromHandle h = loop h
  where
    Builder{..} = monadBuilder
    loop :: Handle #-> Stream (Of Text) RIO ()
    loop h = do
      (Unrestricted isEOF, h') <- Control.lift $ hIsEOF h
      case isEOF of
        True -> do
          Control.lift $ hClose h'
          return ()
        False -> do
          (Unrestricted text, h'') <- Control.lift $ hGetLine h'
          yield text
          fromHandle h''
{-# INLINABLE fromHandle #-}

{-| Read the lines of a file given the filename.

-}
readFile :: FilePath -> Stream (Of Text) RIO ()
readFile path = do
  handle <- Control.lift $ openFile path System.ReadMode
  fromHandle handle
  where
    Builder{..} = monadBuilder

-- | Repeat an element several times.
replicate :: (HasCallStack, Control.Monad m) => Int -> a -> Stream (Of a) m ()
replicate n a
  | n < 0 = Prelude.error "Cannot replicate a stream of negative length"
  | otherwise = loop n a
    where
      loop :: Control.Monad m => Int -> a -> Stream (Of a) m ()
      loop n a
        | n == 0 = Return ()
        | otherwise = Effect $ Control.return $ Step $ a :> loop (n-1) a
{-# INLINABLE replicate #-}

{-| Repeat an action several times, streaming its results.

>>> import qualified Unsafe.Linear as Unsafe
>>> import qualified Data.Time as Time
>>> let getCurrentTime = fromSystemIO (Unsafe.coerce Time.getCurrentTime)
>>> S.print $ S.replicateM 2 getCurrentTime
2015-08-18 00:57:36.124508 UTC
2015-08-18 00:57:36.124785 UTC

-}
replicateM :: Control.Monad m =>
  Int -> m (Unrestricted a) -> Stream (Of a) m ()
replicateM n ma
  | n < 0 = Prelude.error "Cannot replicate a stream of negative length"
  | otherwise = loop n ma
    where
      loop :: Control.Monad m => Int -> m (Unrestricted a) -> Stream (Of a) m ()
      loop n ma
        | n == 0 = Return ()
        | otherwise = Effect $ do
          Unrestricted a <- ma
          return $ Step $ a :> (replicateM (n-1) ma)
          where
            Builder{..} = monadBuilder

untilRight :: forall m a r . Control.Monad m =>
  m (Either (Unrestricted a) r) -> Stream (Of a) m r
untilRight mEither = Effect loop
  where
    Builder{..} = monadBuilder
    loop :: m (Stream (Of a) m r)
    loop = do
      either <- mEither
      either & \case
        Left (Unrestricted a) ->
          return $ Step $ a :> (untilRight mEither)
        Right r -> return $ Return r
{-# INLINABLE untilRight #-}

