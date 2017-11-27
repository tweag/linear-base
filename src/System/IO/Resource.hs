{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | This module defines a resource-aware IO monad. It provide facilities to add
-- in your own resources.
--
-- Functions in this module are meant to be qualified.

-- XXX: This would be better as a multiplicity-parametric relative monad, but
-- until we have multiplicity polymorphism, we use a linear monad.

module System.IO.Resource
  ( RIO
    -- * Creating new types of resources
    -- $new-resources
  , UnsafeResource
  , unsafeRelease
  , unsafeAquire
  ) where

import Control.Exception (SomeException, catch, throwIO)
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Prelude.Linear hiding (IO)
import qualified System.IO.Linear as Linear
import qualified System.IO as System

newtype ReleaseMap = ReleaseMap (IntMap (Linear.IO ()))

-- | The resource-aware I/O monad. This monad guarantees that acquired resources
-- are always released.
newtype RIO a = RIO {
  unRIO
    :: ReleaseMap
    -> Linear.IO (a, Unrestricted ReleaseMap)
  }
  -- TODO: should be a reader of IORef. But it was quicker to define this way.

run :: RIO (Unrestricted a) -> System.IO a
run (RIO action) =
    catch @SomeException
      (Linear.withLinearIO (dropMapIO $ action (ReleaseMap IntMap.empty)))
      (\e -> do -- XXX: should be masked
          -- release stray resources
          let (ReleaseMap releaseMap) = ReleaseMap IntMap.empty -- TODO: no release map available here. Should really be an IORef.
          forM_ (IntMap.toList releaseMap) (\(_,finaliser) ->
            Linear.withLinearIO (moveLinearIO finaliser))
          -- re-throw exception
          throwIO e)
      -- Remarks: resources are guaranteed to be released on non-exceptional
      -- return. So, contrary to a standard bracket/ResourceT implementation, we
      -- only release exceptions in the release map upon exception.
  where
    -- Should be just an application of a linear `(<$>)`.
    moveLinearIO :: Movable a => Linear.IO a ->. Linear.IO (Unrestricted a)
    moveLinearIO action = do
        result <- action
        Linear.return $ move result
      where
        Linear.Builder{..} = Linear.builder -- used in the do-notation

    -- Helpers, will be removed when the release map is changed to `IORef`
    dropMap :: (a, Unrestricted b) ->. a
    dropMap (x, Unrestricted _) = x

    -- will be removed
    dropMapIO :: Linear.IO (a, Unrestricted b) ->. Linear.IO a
    dropMapIO action = do
        result <- action
        Linear.return $ dropMap result
      where
        Linear.Builder{..} = Linear.builder -- used in the do-notation

-- $new-resources

-- | The type of resources. Each safe resource is implemented as an abstract
-- newtype wrapper around @Resource R@ where @R@ is the unsafe variant of the
-- resource.
data UnsafeResource a where
  UnsafeResource :: Int -> a -> UnsafeResource a
  -- Note that both components are unrestricted.

-- TODO: should be masked
unsafeRelease :: UnsafeResource a ->. RIO ()
unsafeRelease (UnsafeResource key _) = RIO (releaseWith key)
  where
    releaseWith key (ReleaseMap releaseMap) = do
        releaser
        Linear.return ((), Unrestricted (ReleaseMap nextMap))
      where
        Linear.Builder {..} = Linear.builder -- used in the do-notation
        releaser = releaseMap IntMap.! key
        nextMap = IntMap.delete key releaseMap

-- TODO: should be masked
-- XXX: long lines
unsafeAquire
  :: Linear.IO (Unrestricted a)
  -> (a -> Linear.IO ())
  -> RIO (UnsafeResource a)
unsafeAquire acquire release = RIO $ \releaseMap -> do
    Unrestricted resource <- acquire
    makeRelease releaseMap resource
  where
    Linear.Builder {..} = Linear.builder -- used in the do-notation
    makeRelease (ReleaseMap releaseMap) resource =
        Linear.return (UnsafeResource releaseKey resource, Unrestricted (ReleaseMap nextMap))
      where
        releaseKey =
          case IntMap.null releaseMap of
            True -> 0
            False -> fst (IntMap.findMax releaseMap) + 1
        releaseAction =
          release resource
        nextMap =
          IntMap.insert releaseKey releaseAction releaseMap
