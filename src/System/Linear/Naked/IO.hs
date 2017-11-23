{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This module defines a naked linear IO monad. It is just a thin wrapper
-- around standard IO primitives, and doesn't enforce interesting invariants by
-- itself. It is intended as a starting point to implement IO-like monads with
-- stronger invariants.
--
-- Functions in this module are intended to be qualified
--
-- @
--     import qualified System.Linear.Naked.IO as Naked
-- @

module System.Linear.Naked.IO
  ( IO(..)
  , ofIO
  , run
  -- * Monadic primitives
  -- $monad

  -- * Exceptions
  -- $exceptions
  , throwIO
  , catch
  ) where

import Control.Exception (Exception)
import qualified Control.Exception as System (throwIO, catch)
import GHC.Exts (State#, RealWorld)
import Linear.Prelude hiding (IO)
import qualified Linear.Unsafe as Unsafe
import qualified System.IO as System (IO)

-- | Like the standard IO monad, but as a linear state monad. Thanks to the
-- linear arrow, we can safely expose the internal representation.
newtype IO a = IO (State# RealWorld ->. (# State# RealWorld, a #))
type role IO representational

-- | Coerces a standard IO action into a linear IO action
ofIO :: System.IO a ->. IO a
ofIO = Unsafe.coerce
  -- The implementation relies on the fact that the monad abstraction for IO
  -- actually enforces linear use of the @RealWorld@ token.
  --
  -- There are potential difficulties coming from the fact that usage differs:
  -- returned value in @System.IO@ can be used unrestrictedly, which is not
  -- typically possible of linear @IO@. This means that @System.IO@ action are
  -- not actually mere translations of linear @IO@ action. Still I [aspiwack]
  -- think that it is safe, hence no "unsafe" in the name.

-- TODO:
-- unrestrictedOfIO :: System.IO a -> IO (Unrestricted a)
-- Needs an unsafe cast @a ->. Unrestricted a@

runInternal :: IO (Unrestricted a) -> System.IO (Unrestricted a)
runInternal = Unsafe.coerce -- basically just subtyping

run :: IO (Unrestricted a) -> System.IO a
run action = unUnrestricted <$> (runInternal action)

-- $exceptions
--
-- Note that the types of @throw@ and @catch@ sport only unrestricted
-- arrows. Having any of the arrow be linear seems to break linearity guarantees
-- in presence of IO.

throwIO :: Exception e => e -> IO a
throwIO e = ofIO $ System.throwIO e

catch
  :: Exception e
  => IO (Unrestricted a) -> (e -> IO (Unrestricted a)) -> IO (Unrestricted a)
catch body handler =
  ofIO $ System.catch (runInternal body) (\e -> runInternal (handler e))
