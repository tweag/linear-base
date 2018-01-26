{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- Deactivate warning because it is painful to refactor functions with two
-- rebinded-do with different bind functions. Such as in the 'run'
-- function. Which is a good argument for having support for F#-style builders.
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
  , return
  , BuilderType(..)
  , builder
    -- * Files
    -- $files
  , Handle
  , openFile
  , hClose
  , hGetChar
  , hPutChar
  , hGetLine
  , hPutStr
  , hPutStrLn
    -- * Creating new types of resources
    -- $new-resources
  , UnsafeResource
  , unsafeRelease
  , unsafeAcquire
  , unsafeFromSystemIOResource
  , unsafeFromSystemIOResource_
  ) where

import Control.Exception (onException, mask, finally)
import Data.Coerce
import qualified Data.IORef as System
import Data.IORef (IORef)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import qualified Data.Text.IO as Text
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

    safeRelease :: [Linear.IO ()] -> System.IO ()
    safeRelease [] = P.return ()
    safeRelease (finalizer:fs) = Linear.withLinearIO (moveLinearIO finalizer)
      `finally` safeRelease fs
    -- Should be just an application of a linear `(<$>)`.
    moveLinearIO :: Movable a => Linear.IO a ->. Linear.IO (Unrestricted a)
    moveLinearIO action' = do
        result <- action'
        Linear.return $ move result
      where
        Linear.Builder{..} = Linear.builder -- used in the do-notation

-- | Should not be applied to a function that acquires or releases resources.
unsafeFromSystemIO :: System.IO a ->. RIO a
unsafeFromSystemIO action = RIO $ \ _ -> Linear.fromSystemIO action

-- $monad


return :: a ->. RIO a
return a = RIO $ \_releaseMap -> Linear.return a

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


-- $files

-- Remark: Handle needs to be private otherwise `Data.Coerce.coerce` could wreak
-- Havoc on the abstraction. But we could provide a smart constructor/view to
-- unsafely convert to file handles in order for the Handle API to be
-- extensible.
newtype Handle = Handle (UnsafeResource System.Handle)

-- | See 'System.IO.openFile'
openFile :: FilePath -> System.IOMode -> RIO Handle
openFile path mode = do
    h <- unsafeAcquire
      (Linear.fromSystemIOU $ System.openFile path mode)
      (\h -> Linear.fromSystemIO $ System.hClose h)
    return $ Handle h
  where
    Builder {..} = builder -- used in the do-notation

hClose :: Handle ->. RIO ()
hClose (Handle h) = unsafeRelease h

hGetChar :: Handle ->. RIO (Unrestricted Char, Handle)
hGetChar = coerce (unsafeFromSystemIOResource System.hGetChar)

hPutChar :: Handle ->. Char -> RIO Handle
hPutChar h c = flipHPutChar c h -- needs a multiplicity polymorphic flip
  where
    flipHPutChar :: Char -> Handle ->. RIO Handle
    flipHPutChar c =
      coerce (unsafeFromSystemIOResource_ (\h' -> System.hPutChar h' c))

hGetLine :: Handle ->. RIO (Unrestricted Text, Handle)
hGetLine = coerce (unsafeFromSystemIOResource Text.hGetLine)

hPutStr :: Handle ->. Text -> RIO Handle
hPutStr h s = flipHPutStr s h -- needs a multiplicity polymorphic flip
  where
    flipHPutStr :: Text -> Handle ->. RIO Handle
    flipHPutStr s =
      coerce (unsafeFromSystemIOResource_ (\h' -> Text.hPutStr h' s))

hPutStrLn :: Handle ->. Text -> RIO Handle
hPutStrLn h s = flipHPutStrLn s h -- needs a multiplicity polymorphic flip
  where
    flipHPutStrLn :: Text -> Handle ->. RIO Handle
    flipHPutStrLn s =
      coerce (unsafeFromSystemIOResource_ (\h' -> Text.hPutStrLn h' s))

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

-- XXX: long lines
unsafeFromSystemIOResource :: (a -> System.IO b) -> UnsafeResource a ->. RIO (Unrestricted b, UnsafeResource a)
unsafeFromSystemIOResource action (UnsafeResource key resource) = unsafeFromSystemIO $ do
    c <- action resource
    P.return (Unrestricted c, UnsafeResource key resource)
  where
    (>>=) :: System.IO a -> (a -> System.IO b) -> (System.IO b)
    (>>=) = (P.>>=)

-- XXX: long lines
unsafeFromSystemIOResource_ :: (a -> System.IO ()) -> UnsafeResource a ->. RIO (UnsafeResource a)
unsafeFromSystemIOResource_ action resource = do
    (Unrestricted _, resource) <- unsafeFromSystemIOResource action resource
    return resource
  where
    Builder {..} = builder -- used in the do-notation
