{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-- Deactivate warning because it is painful to refactor functions with two
-- rebinded-do with different bind functions. Such as in the 'run'
-- function. Which is a good argument for having support for F#-style builders.
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
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
import qualified Control.Monad as Unrestricted (fmap)
import qualified Control.Monad.Builder as Unrestricted
import qualified Data.Functor.Linear as Data
import qualified Control.Monad.Linear as Control
import qualified Control.Monad.Linear.Builder as Control
import Data.Coerce
import qualified Data.IORef as System
import Data.IORef (IORef)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Prelude.Linear hiding (IO)
import qualified Prelude as P
import qualified System.IO.Linear as Linear
import qualified System.IO as System

newtype ReleaseMap = ReleaseMap (IntMap (Linear.IO ()))

-- | The resource-aware I/O monad. This monad guarantees that acquired resources
-- are always released.
newtype RIO a = RIO (IORef ReleaseMap -> Linear.IO a)
  deriving (Data.Functor, Data.Applicative) via (Control.DataFromControl RIO)
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
           safeRelease P.$ Unrestricted.fmap snd P.$ IntMap.toList releaseMap))
      -- Remarks: resources are guaranteed to be released on non-exceptional
      -- return. So, contrary to a standard bracket/ResourceT implementation, we
      -- only release exceptions in the release map upon exception.
  where
    Unrestricted.Builder{..} = Unrestricted.monadBuilder
      -- used in the do-notation

    safeRelease :: [Linear.IO ()] -> System.IO ()
    safeRelease [] = return ()
    safeRelease (finalizer:fs) = Linear.withLinearIO (moveLinearIO finalizer)
      `finally` safeRelease fs
    -- Should be just an application of a linear `(<$>)`.
    moveLinearIO :: Movable a => Linear.IO a ->. Linear.IO (Unrestricted a)
    moveLinearIO action' = do
        result <- action'
        return $ move result
      where
        Control.Builder{..} = Control.monadBuilder -- used in the do-notation

-- | Should not be applied to a function that acquires or releases resources.
unsafeFromSystemIO :: System.IO a ->. RIO a
unsafeFromSystemIO action = RIO (\ _ -> Linear.fromSystemIO action)

-- $monad

instance Control.Functor RIO where
  fmap f (RIO action) = RIO $ \releaseMap ->
    Control.fmap f (action releaseMap)

instance Control.Applicative RIO where
  pure a = RIO $ \_releaseMap -> Control.pure a
  (<*>) = Control.ap

instance Control.Monad RIO where
  x >>= f = RIO $ \releaseMap -> do
      a <- unRIO x releaseMap
      unRIO (f a) releaseMap
    where
      Control.Builder {..} = Control.monadBuilder -- used in the do-notation

  x >> y = RIO $ \releaseMap -> do
      unRIO x releaseMap
      unRIO y releaseMap
    where
      Control.Builder {..} = Control.monadBuilder -- used in the do-notation

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
      (Linear.fromSystemIOU P.$ System.openFile path mode)
      (\h -> Linear.fromSystemIO $ System.hClose h)
    return $ Handle h
  where
    Control.Builder {..} = Control.monadBuilder -- used in the do-notation

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
        Control.Builder {..} = Control.monadBuilder -- used in the do-notation

unsafeAcquire
  :: Linear.IO (Unrestricted a)
  -> (a -> Linear.IO ())
  -> RIO (UnsafeResource a)
unsafeAcquire acquire release = RIO $ \rrm -> Linear.mask_ (do
    Unrestricted resource <- acquire
    Unrestricted (ReleaseMap releaseMap) <- Linear.readIORef rrm
    () <-
      Linear.writeIORef
        rrm
        (ReleaseMap
          (IntMap.insert (releaseKey releaseMap) (release resource) releaseMap))
    return $ UnsafeResource (releaseKey releaseMap) resource)
  where
    Control.Builder {..} = Control.monadBuilder -- used in the do-notation

    releaseKey releaseMap =
      case IntMap.null releaseMap of
        True -> 0
        False -> fst (IntMap.findMax releaseMap) + 1

unsafeFromSystemIOResource
  :: (a -> System.IO b)
  -> UnsafeResource a
  ->. RIO (Unrestricted b, UnsafeResource a)
unsafeFromSystemIOResource action (UnsafeResource key resource) =
    unsafeFromSystemIO (do
      c <- action resource
      return (Unrestricted c, UnsafeResource key resource))
  where
    Unrestricted.Builder{..} = Unrestricted.monadBuilder
      -- used in the do-notation

unsafeFromSystemIOResource_
  :: (a -> System.IO ())
  -> UnsafeResource a
  ->. RIO (UnsafeResource a)
unsafeFromSystemIOResource_ action resource = do
    (Unrestricted _, resource) <- unsafeFromSystemIOResource action resource
    return resource
  where
    Control.Builder {..} = Control.monadBuilder -- used in the do-notation
