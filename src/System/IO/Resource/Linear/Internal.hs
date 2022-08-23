-- Deactivate warning because it is painful to refactor functions with two
-- rebinded-do with different bind functions. Such as in the 'run'
-- function. Which is a good argument for having support for F#-style builders.
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

module System.IO.Resource.Linear.Internal where

import Control.Exception (finally, mask, onException)
import qualified Control.Functor.Linear as Control
import qualified Control.Monad as Ur (fmap)
import qualified Data.Functor.Linear as Data
import Data.IORef (IORef)
import qualified Data.IORef as System
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Monoid (Ap (..))
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Prelude.Linear
  ( Additive ((+)),
    Bool (..),
    Char,
    FilePath,
    Int,
    Integer,
    Monoid,
    Movable (..),
    Semigroup,
    Ur (..),
    fst,
    snd,
    ($),
  )
import qualified System.IO as System
import qualified System.IO.Linear as Linear
import qualified Prelude

-- XXX: This would be better as a multiplicity-parametric relative monad, but
-- until we have multiplicity polymorphism, we use a linear monad.

newtype ReleaseMap = ReleaseMap (IntMap (Linear.IO ()))

-- | The resource-aware I/O monad. This monad guarantees that acquired resources
-- are always released.
newtype RIO a = RIO (IORef ReleaseMap -> Linear.IO a)
  deriving (Data.Functor, Data.Applicative) via (Control.Data RIO)
  deriving (Semigroup, Monoid) via (Ap RIO a)

unRIO :: RIO a %1 -> IORef ReleaseMap -> Linear.IO a
unRIO (RIO action) = action

-- | Take a @RIO@ computation with a value @a@ that is not linearly bound and
-- make it a "System.IO" computation.
run :: RIO (Ur a) -> System.IO a
run (RIO action) = do
  rrm <- System.newIORef (ReleaseMap IntMap.empty)
  mask
    ( \restore ->
        onException
          (restore (Linear.withLinearIO (action rrm)))
          ( do
              -- release stray resources
              ReleaseMap releaseMap <- System.readIORef rrm
              safeRelease $ Ur.fmap snd $ IntMap.toList releaseMap
          )
    )
  where
    -- Remarks: resources are guaranteed to be released on non-exceptional
    -- return. So, contrary to a standard bracket/ResourceT implementation, we
    -- only release exceptions in the release map upon exception.

    safeRelease :: [Linear.IO ()] -> System.IO ()
    safeRelease [] = Prelude.return ()
    safeRelease (finalizer : fs) =
      Linear.withLinearIO (moveLinearIO finalizer)
        `finally` safeRelease fs
    -- Should be just an application of a linear `(<$>)`.
    moveLinearIO :: Movable a => Linear.IO a %1 -> Linear.IO (Ur a)
    moveLinearIO action' = Control.do
      result <- action'
      Control.return $ move result

-- | Should not be applied to a function that acquires or releases resources.
unsafeFromSystemIO :: System.IO a %1 -> RIO a
unsafeFromSystemIO action = RIO (\_ -> Linear.fromSystemIO action)

-- monad

instance Control.Functor RIO where
  fmap f (RIO action) = RIO $ \releaseMap ->
    Control.fmap f (action releaseMap)

instance Control.Applicative RIO where
  pure a = RIO $ \_releaseMap -> Control.pure a
  (<*>) = Control.ap

instance Control.Monad RIO where
  x >>= f = RIO $ \releaseMap -> Control.do
    a <- unRIO x releaseMap
    unRIO (f a) releaseMap

  x >> y = RIO $ \releaseMap -> Control.do
    unRIO x releaseMap
    unRIO y releaseMap

-- files

type Handle = Resource System.Handle

-- | See @System.IO.'System.IO.openFile'@
openFile :: FilePath -> System.IOMode -> RIO Handle
openFile path mode =
  unsafeAcquire
    (Linear.fromSystemIOU $ System.openFile path mode)
    (\h -> Linear.fromSystemIO $ System.hClose h)

-- | See @System.IO.'System.IO.openBinaryFile'@
--
-- @since 0.3.0
openBinaryFile :: FilePath -> System.IOMode -> RIO Handle
openBinaryFile path mode =
  unsafeAcquire
    (Linear.fromSystemIOU $ System.openFile path mode)
    (\h -> Linear.fromSystemIO $ System.hClose h)

-- | Specialised alias for 'release'
hClose :: Handle %1 -> RIO ()
hClose = release

hIsEOF :: Handle %1 -> RIO (Ur Bool, Handle)
hIsEOF = unsafeFromSystemIOResource System.hIsEOF

hGetChar :: Handle %1 -> RIO (Ur Char, Handle)
hGetChar = unsafeFromSystemIOResource System.hGetChar

hPutChar :: Handle %1 -> Char -> RIO Handle
hPutChar h c = unsafeFromSystemIOResource_ (\h' -> System.hPutChar h' c) h

hGetLine :: Handle %1 -> RIO (Ur Text, Handle)
hGetLine = unsafeFromSystemIOResource Text.hGetLine

hPutStr :: Handle %1 -> Text -> RIO Handle
hPutStr h s = unsafeFromSystemIOResource_ (\h' -> Text.hPutStr h' s) h

hPutStrLn :: Handle %1 -> Text -> RIO Handle
hPutStrLn h s = unsafeFromSystemIOResource_ (\h' -> Text.hPutStrLn h' s) h

-- | See @System.IO.'System.IO.hSeek'@.
--
-- @since 0.3.0
hSeek :: Handle %1 -> System.SeekMode -> Integer -> RIO Handle
hSeek h mode i = unsafeFromSystemIOResource_ (\h' -> System.hSeek h' mode i) h

-- | See @System.IO.'System.IO.hTell'@.
--
-- @since 0.3.0
hTell :: Handle %1 -> RIO (Ur Integer, Handle)
hTell = unsafeFromSystemIOResource System.hTell

-- new-resources

-- | The type of system resources.  To create and use resources, you need to
-- use the API since the constructor is not released.
data Resource a where
  UnsafeResource :: Int -> a -> Resource a

-- | Deprecated alias for 'Resource'
type UnsafeResource = Resource

{-# DEPRECATED UnsafeResource "UnsafeResource has been renamed to Resource" #-}

-- Note that both components are unrestricted.

-- | @'release' r@ calls the release function provided when @r@ was acquired.
release :: Resource a %1 -> RIO ()
release (UnsafeResource key _) = RIO (\st -> Linear.mask_ (releaseWith key st))
  where
    releaseWith key rrm = Control.do
      Ur (ReleaseMap releaseMap) <- Linear.readIORef rrm
      () <- releaseMap IntMap.! key
      Linear.writeIORef rrm (ReleaseMap (IntMap.delete key releaseMap))

-- | Deprecated alias of the 'release' function
unsafeRelease :: Resource a %1 -> RIO ()
unsafeRelease = release
{-# DEPRECATED unsafeRelease "unsafeRelease has been renamed to release" #-}

-- | Given a resource in the "System.IO.Linear.IO" monad, and
-- given a function to release that resource, provides that resource in
-- the @RIO@ monad. For example, releasing a @Handle@ from "System.IO"
-- would be done with @fromSystemIO hClose@. Because this release function
-- is an input, and could be wrong, this function is unsafe.
unsafeAcquire ::
  Linear.IO (Ur a) ->
  (a -> Linear.IO ()) ->
  RIO (Resource a)
unsafeAcquire acquire release = RIO $ \rrm ->
  Linear.mask_
    ( Control.do
        Ur resource <- acquire
        Ur (ReleaseMap releaseMap) <- Linear.readIORef rrm
        () <-
          Linear.writeIORef
            rrm
            ( ReleaseMap
                (IntMap.insert (releaseKey releaseMap) (release resource) releaseMap)
            )
        Control.return $ UnsafeResource (releaseKey releaseMap) resource
    )
  where
    releaseKey releaseMap =
      case IntMap.null releaseMap of
        True -> 0
        False -> fst (IntMap.findMax releaseMap) + 1

-- | Given a "System.IO" computation on an unsafe resource,
-- lift it to @RIO@ computaton on the acquired resource.
-- That is function of type @a -> IO b@ turns into a function of type
-- @Resource a %1-> RIO (Ur b)@
-- along with threading the @Resource a@.
--
-- 'unsafeFromSystemIOResource' is only safe to use on actions which do not release
-- the resource.
--
-- Note that the result @b@ can be used non-linearly.
unsafeFromSystemIOResource ::
  (a -> System.IO b) ->
  (Resource a %1 -> RIO (Ur b, Resource a))
unsafeFromSystemIOResource action (UnsafeResource key resource) =
  unsafeFromSystemIO
    ( do
        c <- action resource
        Prelude.return (Ur c, UnsafeResource key resource)
    )

-- | Specialised variant of 'unsafeFromSystemIOResource' for actions that don't
-- return a value.
unsafeFromSystemIOResource_ ::
  (a -> System.IO ()) ->
  (Resource a %1 -> RIO (Resource a))
unsafeFromSystemIOResource_ action resource = Control.do
  (Ur _, resource) <- unsafeFromSystemIOResource action resource
  Control.return resource
