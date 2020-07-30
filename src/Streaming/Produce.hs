{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides all functions which produce a
-- 'Stream (Of a) m r' from some given non-stream inputs.
module Streaming.Produce
  ( yield
  , each'
  , unfoldr
  , stdinLn
  , readLn
  , fromHandle
  , readFile
  , iterate
  , iterateM
  , repeat
  , repeatM
  , replicate
  , replicateM
  , untilRight
  , cycle
  , enumFrom
  , enumFromThen
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

yield :: Control.Monad m => a -> Stream (Of a) m ()
yield x = Step $ x :> Return ()

each' :: Control.Monad m => [a] -> Stream (Of a) m ()
each' xs = Prelude.foldr (\a stream -> Step $ a :> stream) (Return ()) xs

unfoldr :: Control.Monad m =>
  (s #-> m (Either r (Unrestricted a, s))) -> s #-> Stream (Of a) m r
unfoldr step s = Effect $ step s Control.>>= \case
  Left r -> return $ Return r
  Right (Unrestricted a,s') -> return $ Step $ a :> unfoldr step s'
  where
    Builder{..} = monadBuilder

-- Note: we use System.IO.Linear.IO since our IO needs to
-- have a Control.Monad instance
stdinLn :: Stream (Of Text) IO ()
stdinLn = do
  Unrestricted line <- Control.lift $ fromSystemIOU Text.getLine
  yield line
  stdinLn
  where
    Builder{..} = monadBuilder

readLn :: Read a => Stream (Of a) IO ()
readLn = mapMaybe (readMaybe . Text.unpack) stdinLn

-- Note: we use the RIO monad from linear base to enforce
-- the protocol of file handles and file I/O
fromHandle :: Handle #-> Stream (Of Text) RIO ()
fromHandle h = do
  (Unrestricted isEOF, h') <- Control.lift $ hIsEOF h
  case isEOF of
    True -> do
      Control.lift $ hClose h'
      return ()
    False -> do
      (Unrestricted text, h'') <- Control.lift $ hGetLine h'
      yield text
      fromHandle h''
  where
    Builder{..} = monadBuilder

readFile :: FilePath -> Stream (Of Text) RIO ()
readFile path = do
  handle <- Control.lift $ openFile path System.ReadMode
  fromHandle handle
  where
    Builder{..} = monadBuilder

iterate :: Control.Monad m => (a -> a) -> a -> Stream (Of a) m r
iterate step x = Effect $ return $ Step $ x :> iterate step (step x)
  where
    Builder{..} = monadBuilder

iterateM :: Control.Monad m =>
  (a -> m (Unrestricted a)) -> m (Unrestricted a) #-> Stream (Of a) m r
iterateM stepM mx = Effect $ do
  Unrestricted x <- mx
  return $ Step $ x :> iterateM stepM (stepM x)
  where
    Builder{..} = monadBuilder

repeat :: Control.Monad m => a -> Stream (Of a) m r
repeat = iterate Prelude.id

repeatM :: Control.Monad m => m (Unrestricted a) -> Stream (Of a) m r
repeatM ma = Effect $ do
  Unrestricted a <- ma
  return $ Step $ a :> repeatM ma
  where
    Builder{..} = monadBuilder

replicate :: (HasCallStack, Control.Monad m) => Int -> a -> Stream (Of a) m ()
replicate n a
  | n < 0 = Prelude.error "Cannot replicate a stream of negative length"
  | n == 0 = Return ()
  | otherwise = Effect $ Control.return $ Step $ a :> replicate (n-1) a

replicateM :: Control.Monad m =>
  Int -> m (Unrestricted a) -> Stream (Of a) m ()
replicateM n ma
  | n < 0 = Prelude.error "Cannot replicate a stream of negative length"
  | n == 0 = Return ()
  | otherwise = Effect $ do
    Unrestricted a <- ma
    return $ Step $ a :> (replicateM (n-1) ma)
    where
      Builder{..} = monadBuilder

untilRight :: Control.Monad m =>
  m (Either (Unrestricted a) r) -> Stream (Of a) m r
untilRight mEither = Effect $ do
  either <- mEither
  either & \case
    Left (Unrestricted a) ->
      return $ Step $ a :> (untilRight mEither)
    Right r -> return $ Return r
  where
    Builder{..} = monadBuilder

-- | 'cycle' repeats a linear stream by running through it
-- from start to end indefinitely. To do so, it must use an
-- unrestricted arrow following the stream argument.
cycle :: (Control.Monad m, Control.Functor f, Consumable r) =>
  Stream f m r -> Stream f m r
cycle s = do
  r <- s
  lseq r $ cycle s
  where
  Builder{..} = monadBuilder

enumFrom :: (Control.Monad m, Prelude.Enum n) => n -> Stream (Of n) m r
enumFrom x = iterate Prelude.succ x

enumFromThen ::
  (Control.Monad m, Prelude.Enum a) => a -> a -> Stream (Of a) m r
enumFromThen first second =
  map Prelude.toEnum $ iterate (+ diff) (Prelude.fromEnum first)
  where
    diff = Prelude.fromEnum first - Prelude.fromEnum second

