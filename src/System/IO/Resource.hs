{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module defines a resource-aware IO monad. It provide facilities to add
-- in your own resources.
--
-- Functions in this module are meant to be qualified.

-- XXX: This would be better as a multiplicity-parametric relative monad, but
-- until we have multiplicity polymorphism, we use a linear monad.

module System.IO.Resource
  ( RIO
  , run
    -- * Creating new types of resources
    -- $new-resources
  , UnsafeResource
  , unsafeRelease
  , unsafeAcquire
  ) where

import Control.Exception (onException, mask)
import qualified Data.IORef as System
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import Prelude.Linear hiding (IO)
import qualified System.IO.Linear as Linear
import qualified System.IO as System

newtype ReleaseMap = ReleaseMap (IntMap (Linear.IO ()))

-- | The resource-aware I/O monad. This monad guarantees that acquired resources
-- are always released.
newtype RIO a = RIO {
  unRIO
    :: IORef ReleaseMap -> Linear.IO a
  }

run :: RIO (Unrestricted a) -> System.IO a
run (RIO action) = do
    rrm <- System.newIORef (ReleaseMap IntMap.empty)
    mask (\restore ->
      onException
        (restore (Linear.withLinearIO (action rrm)))
        (do -- release stray resources
           ReleaseMap releaseMap <- System.readIORef rrm
           forM_ (IntMap.toList releaseMap) (\(_,finaliser) ->
             Linear.withLinearIO (moveLinearIO finaliser))))
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
    releaseWith key rrm = do
        Unrestricted (ReleaseMap releaseMap) <- Linear.readIORef rrm
        () <- releaseMap IntMap.! key
        Linear.writeIORef rrm (ReleaseMap (IntMap.delete key releaseMap))
      where
        Linear.Builder {..} = Linear.builder -- used in the do-notation

-- XXX: long lines
unsafeAcquire
  :: Linear.IO (Unrestricted a)
  -> (a -> Linear.IO ())
  -> RIO (UnsafeResource a)
unsafeAcquire acquire release = RIO $ \rrm -> Linear.mask_ (do
    Unrestricted resource <- acquire
    Unrestricted (ReleaseMap releaseMap) <- Linear.readIORef rrm
    () <- Linear.writeIORef rrm (ReleaseMap (IntMap.insert (releaseKey releaseMap) (release resource) releaseMap))
    Linear.return $ UnsafeResource (releaseKey releaseMap) resource)
  where
    Linear.Builder {..} = Linear.builder -- used in the do-notation

    releaseKey releaseMap =
      case IntMap.null releaseMap of
        True -> 0
        False -> fst (IntMap.findMax releaseMap) + 1
