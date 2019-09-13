{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This module redefines 'IO' with linear types. It defines a drop-in
-- replacement for 'System.IO.IO' in @System.IO@. This module will be deprecated
-- if the definition for 'IO' found here is upstreamed in "System.IO".
--
-- It will be much more pleasant when multiplicity-polymorphism has been
-- implemented, in this case it will really supersede IO.

module System.IO.Linear
  ( IO(..)
  , fromSystemIO
  , fromSystemIOU
  , withLinearIO
  -- * Ref
  -- $ioref
  , newIORef
  , readIORef
  , writeIORef
  -- * Exceptions
  -- $exceptions
  , throwIO
  , catch
  , mask_
  ) where

import Data.IORef (IORef)
import qualified Data.IORef as System
import Control.Exception (Exception)
import qualified Control.Exception as System (throwIO, catch, mask_)
import qualified Control.Monad.Linear as Control
import qualified Data.Functor.Linear as Data
import GHC.Exts (State#, RealWorld)
import Prelude.Linear hiding (IO)
import Prelude ((<$>))
import qualified Unsafe.Linear as Unsafe
import qualified System.IO as System

-- | Like the standard IO monad, but as a linear state monad. Thanks to the
-- linear arrow, we can safely expose the internal representation.
newtype IO a = IO (State# RealWorld ->. (# State# RealWorld, a #))
  deriving (Data.Functor, Data.Applicative) via (Control.Data IO)
type role IO representational

unIO :: IO a ->. State# RealWorld ->. (# State# RealWorld, a #)
-- Defined separately because projections from newtypes are considered like
-- general projections of data types, which take an unrestricted argument.
unIO (IO action) = action

-- | Coerces a standard IO action into a linear IO action
fromSystemIO :: System.IO a ->. IO a
-- The implementation relies on the fact that the monad abstraction for IO
-- actually enforces linear use of the @RealWorld@ token.
--
-- There are potential difficulties coming from the fact that usage differs:
-- returned value in 'System.IO' can be used unrestrictedly, which is not
-- typically possible of linear 'IO'. This means that 'System.IO' action are
-- not actually mere translations of linear 'IO' action. Still I [aspiwack]
-- think that it is safe, hence no "unsafe" in the name.
fromSystemIO = Unsafe.coerce

fromSystemIOU :: System.IO a -> IO (Unrestricted a)
fromSystemIOU action =
  fromSystemIO (Unrestricted <$> action)

toSystemIO :: IO a ->. System.IO a
toSystemIO = Unsafe.coerce -- basically just subtyping

-- | Use at the top of @main@ function in your program to switch to the linearly
-- typed version of 'IO':
--
-- @
-- main :: IO ()
-- main = withLinearIO $ do
--     ...
-- @
withLinearIO :: IO (Unrestricted a) -> System.IO a
withLinearIO action = (\x -> unUnrestricted x) <$> (toSystemIO action)

-- * Monadic interface

instance Control.Functor IO where
  fmap :: forall a b. (a ->. b) ->. IO a ->. IO b
  fmap f x = IO $ \s ->
      cont (unIO x s) f
    where
      -- XXX: long line
      cont :: (# State# RealWorld, a #) ->. (a ->. b) ->. (# State# RealWorld, b #)
      cont (# s', a #) f' = (# s', f' a #)

instance Control.Applicative IO where
  pure :: forall a. a ->. IO a
  pure a = IO $ \s -> (# s, a #)

  (<*>) :: forall a b. IO (a ->. b) ->. IO a ->. IO b
  (<*>) = Control.ap

instance Control.Monad IO where
  (>>=) :: forall a b. IO a ->. (a ->. IO b) ->. IO b
  x >>= f = IO $ \s ->
      cont (unIO x s) f
    where
      -- XXX: long line
      cont :: (# State# RealWorld, a #) ->. (a ->. IO b) ->. (# State# RealWorld, b #)
      cont (# s', a #) f' = unIO (f' a) s'

  (>>) :: forall b. IO () ->. IO b ->. IO b
  x >> y = IO $ \s ->
      cont (unIO x s) y
    where
      cont :: (# State# RealWorld, () #) ->. IO b ->. (# State# RealWorld, b #)
      cont (# s', () #) y' = unIO y' s'

-- $ioref
--
-- All arrows are unrestricted: IORefs containing linear values can make linear
-- values escape their scope.

newIORef :: a -> IO (Unrestricted (IORef a))
newIORef a = fromSystemIOU (System.newIORef a)

readIORef :: IORef a -> IO (Unrestricted a)
readIORef r = fromSystemIOU (System.readIORef r)

writeIORef :: IORef a -> a -> IO ()
writeIORef r a = fromSystemIO $ System.writeIORef r a

-- $exceptions
--
-- Note that the types of @throw@ and @catch@ sport only unrestricted arrows.
-- Having any of the arrows be linear is unsound.

throwIO :: Exception e => e -> IO a
throwIO e = fromSystemIO $ System.throwIO e

catch
  :: Exception e
  => IO (Unrestricted a) -> (e -> IO (Unrestricted a)) -> IO (Unrestricted a)
catch body handler =
  fromSystemIO $ System.catch (toSystemIO body) (\e -> toSystemIO (handler e))

mask_ :: IO a -> IO a
mask_ action = fromSystemIO (System.mask_ (toSystemIO action))
