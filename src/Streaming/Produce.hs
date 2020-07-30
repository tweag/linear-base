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
  , enumFrom
  , enumFromThen
  ) where

import Streaming.Type
import Streaming.Process
import Prelude.Linear (($), (&))
import Prelude (Either(..), String, Read, Bool(..), FilePath,
               Num(..), Int, otherwise, Eq(..), Ord(..))
import qualified Prelude
import qualified Control.Monad.Linear as Control
import Control.Monad.Linear.Builder (BuilderType(..), monadBuilder)
import Data.Unrestricted.Linear
import System.IO.Linear
import System.IO.Resource
import qualified System.IO as System
import Text.Read (readMaybe)
import Data.Text (unpack)
import GHC.Stack


-- # The Stream Constructors
-------------------------------------------------------------------------------

yield :: CMonad m => a -> Stream (Of a) m ()
yield x = Step $ x :> Return ()

each' :: CMonad m => [a] -> Stream (Of a) m ()
each' xs = Prelude.foldr (\a stream -> Step $ a :> stream) (Return ()) xs

unfoldr :: CMonad m =>
  (s #-> m (Either r (Unrestricted a, s))) -> s #-> Stream (Of a) m r
unfoldr step s = Effect $ step s Control.>>= \case
  Left r -> return $ Return r
  Right (Unrestricted a,s') -> return $ Step $ a :> unfoldr step s'
  where
    Builder{..} = monadBuilder

stdinLn :: Stream (Of String) IO ()
stdinLn = do
  Unrestricted line <- Control.lift $ fromSystemIOU System.getLine
  yield line
  stdinLn
  where
    Builder{..} = monadBuilder

readLn :: Read a => Stream (Of a) IO ()
readLn = mapMaybe readMaybe stdinLn

fromHandle :: Handle #-> Stream (Of String) RIO ()
fromHandle h = do
  (Unrestricted isEOF, h') <- Control.lift $ hIsEOF h
  case isEOF of
    True -> do
      Control.lift $ hClose h'
      return ()
    False -> do
      (Unrestricted text, h'') <- Control.lift $ hGetLine h'
      yield (unpack text)
      fromHandle h''
  where
    Builder{..} = monadBuilder

readFile :: FilePath -> Stream (Of String) RIO ()
readFile path = do
  handle <- Control.lift $ openFile path System.ReadMode
  fromHandle handle
  where
    Builder{..} = monadBuilder

iterate :: CMonad m => (a -> a) -> a -> Stream (Of a) m r
iterate step x = Effect $ return $ Step $ x :> iterate step (step x)
  where
    Builder{..} = monadBuilder

iterateM :: CMonad m =>
  (a -> m (Unrestricted a)) -> m (Unrestricted a) #-> Stream (Of a) m r
iterateM stepM mx = Effect $ do
  Unrestricted x <- mx
  return $ Step $ x :> iterateM stepM (stepM x)
  where
    Builder{..} = monadBuilder

repeat :: CMonad m => a -> Stream (Of a) m r
repeat = iterate Prelude.id

repeatM :: CMonad m => m (Unrestricted a) -> Stream (Of a) m r
repeatM ma = Effect $ do
  Unrestricted a <- ma
  return $ Step $ a :> repeatM ma
  where
    Builder{..} = monadBuilder

replicate :: (HasCallStack, CMonad m) => Int -> a -> Stream (Of a) m ()
replicate n a
  | n < 0 = Prelude.error "Cannot replicate a stream of negative length"
  | n == 0 = Return ()
  | otherwise = Effect $ Control.return $ Step $ a :> replicate (n-1) a

replicateM :: CMonad m => Int -> m (Unrestricted a) -> Stream (Of a) m ()
replicateM n ma
  | n < 0 = Prelude.error "Cannot replicate a stream of negative length"
  | n == 0 = Return ()
  | otherwise = Effect $ do
    Unrestricted a <- ma
    return $ Step $ a :> (replicateM (n-1) ma)
    where
      Builder{..} = monadBuilder

untilRight :: CMonad m => m (Either (Unrestricted a) r) -> Stream (Of a) m r
untilRight mEither = Effect $ do
  either <- mEither
  either & \case
    Left (Unrestricted a) ->
      return $ Step $ a :> (untilRight mEither)
    Right r -> return $ Return r
  where
    Builder{..} = monadBuilder

enumFrom :: (CMonad m, Prelude.Enum n) => n -> Stream (Of n) m r
enumFrom x = iterate Prelude.succ x

enumFromThen :: (CMonad m, Prelude.Enum a) => a -> a -> Stream (Of a) m r
enumFromThen first second =
  map Prelude.toEnum $ iterate (+ diff) (Prelude.fromEnum first)
  where
    diff = Prelude.fromEnum first - Prelude.fromEnum second

