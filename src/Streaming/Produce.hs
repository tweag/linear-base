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

