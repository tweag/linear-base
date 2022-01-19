{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Unrestricted.Internal.Replicator (RepStream (..), Replicator (..)) where

import Prelude.Linear.Internal
import Data.V.Linear.Internal.V (FunN)
import Prelude (Either(..))
import GHC.TypeLits

data RepStream a where
  RepStream :: (s %1 -> a) -> (s %1 -> (s, s)) -> (s %1 -> ()) -> s %1 -> RepStream a

data Replicator a where
  FromMoved :: a -> Replicator a
  FromStream :: RepStream a %1 -> Replicator a
