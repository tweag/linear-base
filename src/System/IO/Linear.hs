{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module defines a resource-safe linear IO monad. It provide facilities
-- to add in your own resources.
--
-- Functions in this module are meant to be qualified.

-- XXX: This would be better as a multiplicity-parametric relative monad, but
-- until we have multiplicity polymorphism, we use a linear monad.

module System.IO.Linear where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Prelude.Linear hiding (IO)
import qualified System.IO.Linear.Internal as Naked

newtype IO a = IO (ReleaseMap -> Naked.IO (a, Unrestricted ReleaseMap))
-- The implementation of resource-safe @IO@ is based on the ResourceT monad

type ReleaseMap = Map Int (Naked.IO ())

-- * Creating new types of resources

-- | The type of resources. Each safe resource is implemented as an abstract
-- newtype wrapper around @Resource R@ where @R@ is the unsafe variant of the
-- resource.
data UnsafeResource a where
  UnsafeResource :: Int -> a -> UnsafeResource a
  -- Note that both components are unrestricted.

-- XXX: long line
unsafeRelease :: UnsafeResource a -> IO ()
unsafeRelease (UnsafeResource key _) = IO $ \ releaseMap -> releaseWith key releaseMap
  where
    releaseWith key releaseMap = do
        releaser
        Naked.return ((), Unrestricted nextMap)
      where
        Naked.Builder {..} = Naked.builder -- used in the do-notation
        releaser = releaseMap Map.! key
        nextMap = Map.delete key releaseMap

-- XXX: long lines
unsafeAquire :: Naked.IO (Unrestricted a) -> (a->Naked.IO ()) -> IO (UnsafeResource a)
unsafeAquire acquire release = IO $ \ releaseMap -> do
    Unrestricted resource <- acquire
    makeRelease releaseMap resource
  where
    Naked.Builder {..} = Naked.builder -- used in the do-notation
    makeRelease releaseMap resource =
        Naked.return (UnsafeResource releaseKey resource, Unrestricted nextMap)
      where
        releaseKey =
          case Map.null releaseMap of
            True -> 0
            False -> fst (Map.findMax releaseMap) + 1
        releaseAction =
          release resource
        nextMap =
          Map.insert releaseKey releaseAction releaseMap
