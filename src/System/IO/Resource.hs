{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
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
    -- * Monadic primitives
    -- $monad
    -- * Creating new types of resources
    -- $new-resources
  , UnsafeResource
  , unsafeRelease
  , unsafeAcquire
  ) where

import Control.Exception (onException, mask, finally)
import qualified Data.IORef as System
import Control.Monad (forM_)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (IORef)
import Data.IntMap.Strict (IntMap)
import Prelude.Linear hiding (IO, (>>=), (>>), return)
import qualified Prelude as P
import qualified System.IO.Linear as Linear
import qualified System.IO as System

newtype ReleaseMap = ReleaseMap (IntMap (Linear.IO ()))

-- | The resource-aware I/O monad. This monad guarantees that acquired resources
-- are always released.
newtype RIO a = RIO (IORef ReleaseMap -> Linear.IO a)
unRIO :: RIO a ->. IORef ReleaseMap -> Linear.IO a
unRIO (RIO action) = action

run :: RIO (Unrestricted a) -> System.IO a
run (RIO action) = do
    rrm <- System.newIORef (ReleaseMap IntMap.empty)
    mask (\restore ->
      onException
        (restore (Linear.withLinearIO (action rrm)))
        (do -- release stray resources
           ReleaseMap releaseMap <- System.readIORef rrm
           safeRelease $ fmap snd $ IntMap.toList releaseMap))
      -- Remarks: resources are guaranteed to be released on non-exceptional
      -- return. So, contrary to a standard bracket/ResourceT implementation, we
      -- only release exceptions in the release map upon exception.
  where
    -- Use regular IO binds
    (>>=) :: System.IO a -> (a -> System.IO b) -> (System.IO b)
    (>>=) = (P.>>=)
    (>>) :: System.IO a -> System.IO b -> System.IO b
    (>>) = (P.>>)

    safeRelease :: [Linear.IO ()] -> System.IO ()
    safeRelease [] = P.return ()
    safeRelease (finalizer:fs) = Linear.withLinearIO (moveLinearIO finalizer)
      `finally` safeRelease fs
    -- Should be just an application of a linear `(<$>)`.
    moveLinearIO :: Movable a => Linear.IO a ->. Linear.IO (Unrestricted a)
    moveLinearIO action = do
        result <- action
        Linear.return $ move result
      where
        Linear.Builder{..} = Linear.builder -- used in the do-notation

-- $monad


return :: a ->. RIO a
return a = RIO $ \releaseMap -> Linear.return a

-- | Type of 'Builder'
data BuilderType = Builder
  { (>>=) :: forall a b. RIO a ->. (a ->. RIO b) ->. RIO b
  , (>>) :: forall b. RIO () ->. RIO b ->. RIO b
  }

-- | A builder to be used with @-XRebindableSyntax@ in conjunction with
-- @RecordWildCards@
builder :: BuilderType
builder =
  let
    (>>=) :: forall a b. RIO a ->. (a ->. RIO b) ->. RIO b
    x >>= f = RIO $ \releaseMap -> do
        a <- unRIO x releaseMap
        unRIO (f a) releaseMap
      where
        Linear.Builder {..} = Linear.builder -- used in the do-notation

    (>>) :: forall b. RIO () ->. RIO b ->. RIO b
    x >> y = RIO $ \releaseMap -> do
        unRIO x releaseMap
        unRIO y releaseMap
      where
        Linear.Builder {..} = Linear.builder -- used in the do-notation
  in
    Builder{..}

-- $new-resources

-- | The type of resources. Each safe resource is implemented as an abstract
-- newtype wrapper around @Resource R@ where @R@ is the unsafe variant of the
-- resource.
data UnsafeResource a where
  UnsafeResource :: Int -> a -> UnsafeResource a
  -- Note that both components are unrestricted.

unsafeRelease :: UnsafeResource a ->. RIO ()
unsafeRelease (UnsafeResource key _) = RIO (\st -> Linear.mask_ (releaseWith key st))
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
