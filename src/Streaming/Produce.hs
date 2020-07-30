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
--  , readLn
--  , fromHandle
--  , readFile
--  , iterate
--  , iterateM
--  , repeat
--  , repeatM
--  , replicate
--  , untilRight
--  , cycle
--  , replicateM
--  , enumFrom
--  , enumFromThen
--  , seconds
  ) where

import Streaming.Type
import Streaming.Process
import Prelude.Linear (($), (&))
import Prelude (Either(..), String, Read)
import qualified Prelude
import qualified Control.Monad.Linear as Control
import Control.Monad.Linear.Builder (BuilderType(..), monadBuilder)
import Data.Unrestricted.Linear
import System.IO.Linear
import System.IO.Resource
import qualified System.IO as System
import Text.Read (readMaybe)


-- # The Stream constructors
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


--fromHandle :: Handle #-> Stream (Of String) IO ()
--fromHandle = undefined











