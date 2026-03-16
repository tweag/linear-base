{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module redefines 'IO' with linear types.
--
-- To use this @IO@, do the following:
--
--  * use @ main = withLinearIO $ do ...@
--  * pull in any safe non-linear 'IO' functions with
--  @fromSystemIO@ and @fromSystemIOU@
--  * for mutable IO references/pointers, file handles, or any resources, use
--  the linear APIs provided here and in other linear @System.IO@ modules
--
-- = Example
-- @
-- import qualified System.IO.Linear as Linear
--
-- main :: IO ()
-- main = Linear.withLinearIO $
--   Linear.fromSystemIOU $ putStrLn "hello world today"
-- @
--
-- = Replacing The Original @IO@ With This Module.
--
-- This module will be deprecated if the definition for 'IO' found here is
-- upstreamed in "System.IO".  When multiplicity-polymorphism is implemented,
-- this module will supercede IO by providing a seamless replacement for
-- "System.IO" that won't break non-linear code.
module System.IO.Linear
  ( IO,

    -- * Interfacing with "System.IO"
    fromSystemIO,
    fromSystemIOU,
    withLinearIO,

    -- * Using Mutable References
    -- $ioref
    newIORef,
    readIORef,
    writeIORef,

    -- * Catching and Throwing Exceptions
    -- $exceptions
    throwIO,
    catch,
    mask_,
  )
where

import Control.Exception (Exception)
import qualified Control.Exception as System (catch, mask_, throwIO)
import Data.IORef (IORef)
import GHC.Exts (RealWorld)
import qualified GHC.ST (ST (..))
import qualified GHC.IO as System (IO (..))
import qualified GHC.IORef as System (IORef (..))
import Prelude.Linear hiding (IO)
import qualified System.IO as System
import qualified Prelude
import Control.Monad.ST.Linear
import Data.Coerce

-- | This is the linear IO monad.
-- It is a newtype around a function that transitions from one
-- @State# RealWorld@ to another, producing a value of type @a@ along with it.
-- The @State# RealWorld@ is the state of the world/machine outside the program.
--
-- The only way, such a computation is run is by putting it in @Main.main@
-- somewhere.
type IO = ST RealWorld

{- Pattern synonyms do not support linear fields (GHC #18806)
pattern IO :: State# RealWorld %1 -> (# State# RealWorld, a #) -> IO a
pattern IO action  = ST action
-}

-- | Coerces a standard IO action into a linear IO action.
-- Note that the value @a@ must be used linearly in the linear IO monad.
fromSystemIO :: forall a. System.IO a %1 -> IO a
fromSystemIO = coerce (fromUrST @RealWorld @a)

-- | Coerces a standard IO action to a linear IO action, allowing you to use
-- the result of type @a@ in a non-linear manner by wrapping it inside
-- 'Ur'.
fromSystemIOU :: forall a. System.IO a -> IO (Ur a)
fromSystemIOU = coerce (fromUrSTU @RealWorld @a)

-- | Convert a linear IO action to a "System.IO" action.
toSystemIO :: forall a. IO a %1 -> System.IO a
toSystemIO = coerce (toUrST @RealWorld @a)

-- | Use at the top of @main@ function in your program to switch to the
-- linearly typed version of 'IO':
--
-- @
-- main :: IO ()
-- main = Linear.withLinearIO $ do ...
-- @
withLinearIO :: IO (Ur a) -> System.IO a
withLinearIO action = (\x -> unur x) Prelude.<$> (toSystemIO action)

-- $ioref
-- @IORef@s are mutable references to values, or pointers to values.
-- You can create, mutate and read them from running IO actions.
--
-- Note that all arrows are unrestricted.  This is because IORefs containing
-- linear values can make linear values escape their scope and be used
-- non-linearly.

newIORef :: forall a. a -> IO (Ur (IORef a))
newIORef = coerce (newSTRef @a @RealWorld)

readIORef :: forall a. IORef a -> IO (Ur a)
readIORef = coerce (readSTRef @RealWorld @a)

writeIORef :: forall a. IORef a -> a -> IO ()
writeIORef = coerce (writeSTRef @RealWorld @a)

-- $exceptions
--
-- Note that the types of @throw@ and @catch@ sport only unrestricted arrows.
-- Having any of the arrows be linear is unsound.
-- See [here](http://dev.stephendiehl.com/hask/index.html#control.exception)
-- to learn about exceptions.

throwIO :: (Exception e) => e -> IO a
throwIO e = fromSystemIO $ System.throwIO e

catch ::
  (Exception e) =>
  IO (Ur a) ->
  (e -> IO (Ur a)) ->
  IO (Ur a)
catch body handler =
  fromSystemIO $ System.catch (toSystemIO body) (\e -> toSystemIO (handler e))

mask_ :: IO a -> IO a
mask_ action = fromSystemIO (System.mask_ (toSystemIO action))
