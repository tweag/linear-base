{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
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
  ( IO (..),

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
import qualified Control.Functor.Linear as Control
import qualified Data.Functor.Linear as Data
import Data.IORef (IORef)
import qualified Data.IORef as System
import GHC.Exts (RealWorld, State#)
import qualified GHC.IO as System (IO (..))
import Prelude.Linear hiding (IO)
import qualified System.IO as System
import qualified Unsafe.Linear as Unsafe
import qualified Prelude

-- | This is the linear IO monad.
-- It is a newtype around a function that transitions from one
-- @State# RealWorld@ to another, producing a value of type @a@ along with it.
-- The @State# RealWorld@ is the state of the world/machine outside the program.
--
-- The only way, such a computation is run is by putting it in @Main.main@
-- somewhere.
--
-- Note that this is the same definition as the standard IO monad, but with a
-- linear arrow enforcing the implicit invariant that IO actions linearly
-- thread the state of the real world. Hence, we can safely release the
-- constructor to this newtype.
newtype IO a = IO (State# RealWorld %1 -> (# State# RealWorld, a #))
  deriving (Data.Functor, Data.Applicative) via (Control.Data IO)

type role IO representational

-- Defined separately because projections from newtypes are considered like
-- general projections of data types, which take an unrestricted argument.
unIO :: IO a %1 -> State# RealWorld %1 -> (# State# RealWorld, a #)
unIO (IO action) = action

-- | Coerces a standard IO action into a linear IO action.
-- Note that the value @a@ must be used linearly in the linear IO monad.
fromSystemIO :: System.IO a %1 -> IO a
-- The implementation relies on the fact that the monad abstraction for IO
-- actually enforces linear use of the @RealWorld@ token.
--
-- There are potential difficulties coming from the fact that usage differs:
-- returned value in 'System.IO' can be used unrestrictedly, which is not
-- typically possible of linear 'IO'. This means that 'System.IO' action are
-- not actually mere translations of linear 'IO' action. Still I [aspiwack]
-- think that it is safe, hence no "unsafe" in the name.
fromSystemIO = Unsafe.coerce

-- | Coerces a standard IO action to a linear IO action, allowing you to use
-- the result of type @a@ in a non-linear manner by wrapping it inside
-- 'Ur'.
fromSystemIOU :: System.IO a -> IO (Ur a)
fromSystemIOU action =
  fromSystemIO (Ur Prelude.<$> action)

-- | Convert a linear IO action to a "System.IO" action.
toSystemIO :: IO a %1 -> System.IO a
toSystemIO (IO f) = System.IO (\s -> f s)

-- | Use at the top of @main@ function in your program to switch to the
-- linearly typed version of 'IO':
--
-- @
-- main :: IO ()
-- main = Linear.withLinearIO $ do ...
-- @
withLinearIO :: IO (Ur a) -> System.IO a
withLinearIO action = (\x -> unur x) Prelude.<$> (toSystemIO action)

-- * Monadic interface

instance Control.Functor IO where
  fmap :: forall a b. (a %1 -> b) %1 -> IO a %1 -> IO b
  fmap f x = IO $ \s ->
    cont (unIO x s) f
    where
      -- XXX: long line
      cont :: (# State# RealWorld, a #) %1 -> (a %1 -> b) %1 -> (# State# RealWorld, b #)
      cont (# s', a #) f' = (# s', f' a #)

instance Control.Applicative IO where
  pure :: forall a. a %1 -> IO a
  pure a = IO $ \s -> (# s, a #)

  (<*>) :: forall a b. IO (a %1 -> b) %1 -> IO a %1 -> IO b
  (<*>) = Control.ap

instance Control.Monad IO where
  (>>=) :: forall a b. IO a %1 -> (a %1 -> IO b) %1 -> IO b
  x >>= f = IO $ \s ->
    cont (unIO x s) f
    where
      -- XXX: long line
      cont :: (# State# RealWorld, a #) %1 -> (a %1 -> IO b) %1 -> (# State# RealWorld, b #)
      cont (# s', a #) f' = unIO (f' a) s'

  (>>) :: forall b. IO () %1 -> IO b %1 -> IO b
  x >> y = IO $ \s ->
    cont (unIO x s) y
    where
      cont :: (# State# RealWorld, () #) %1 -> IO b %1 -> (# State# RealWorld, b #)
      cont (# s', () #) y' = unIO y' s'

instance (Semigroup a) => Semigroup (IO a) where
  (<>) = Control.liftA2 (<>)

instance (Monoid a) => Monoid (IO a) where
  mempty = Control.pure mempty

-- $ioref
-- @IORef@s are mutable references to values, or pointers to values.
-- You can create, mutate and read them from running IO actions.
--
-- Note that all arrows are unrestricted.  This is because IORefs containing
-- linear values can make linear values escape their scope and be used
-- non-linearly.

newIORef :: a -> IO (Ur (IORef a))
newIORef a = fromSystemIOU (System.newIORef a)

readIORef :: IORef a -> IO (Ur a)
readIORef r = fromSystemIOU (System.readIORef r)

writeIORef :: IORef a -> a -> IO ()
writeIORef r a = fromSystemIO $ System.writeIORef r a

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
