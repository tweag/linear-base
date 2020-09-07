{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-|
A thin wrapper on top of "Debug.Trace", providing linear versions of
tracing functions.

It only contains minimal amount of documentation; you should consult
the original "Debug.Trace" module for more detailed information.
-}
module Debug.Trace.Linear
  ( -- * Tracing
    trace
  , traceShow
  , traceId
  , traceStack
  , traceIO
  , traceM
  , traceShowM
    -- * Eventlog tracing
  , traceEvent
  , traceEventIO
    -- * Execution phase markers
  , traceMarker
  , traceMarkerIO
  ) where

import qualified Debug.Trace as NonLinear
import qualified Unsafe.Linear as Unsafe
import System.IO.Linear
import Data.Functor.Linear
import Data.Unrestricted.Linear
import Prelude (String, Show(..))
import Prelude.Linear.Internal

-- | The 'trace' function outputs the trace message given as its first
-- argument, before returning the second argument as its result.
trace :: String #-> a #-> a
trace = Unsafe.toLinear2 NonLinear.trace

-- | Like 'trace', but uses 'show' on the argument to convert it to
-- a 'String'.
traceShow :: Show a => a -> b #-> b
traceShow a = Unsafe.toLinear (NonLinear.traceShow a)

-- | Like 'trace' but returns the message instead of a third value.
traceId :: String #-> String
traceId s = dup s & \(s', s'') -> trace s' s''

-- | Like 'trace', but additionally prints a call stack if one is
-- available.
traceStack :: String #-> a #-> a
traceStack = Unsafe.toLinear2 NonLinear.traceStack

-- | The 'traceIO' function outputs the trace message from the IO monad.
-- This sequences the output with respect to other IO actions.
traceIO :: String #-> IO ()
traceIO s = fromSystemIO (Unsafe.toLinear NonLinear.traceIO s)

-- | Like 'trace' but returning unit in an arbitrary 'Applicative'
-- context. Allows for convenient use in do-notation.
traceM :: Applicative f => String #-> f ()
traceM s = trace s $ pure ()

-- | Like 'traceM', but uses 'show' on the argument to convert it to a
-- 'String'.
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM a = traceM (show a)

-- | The 'traceEvent' function behaves like 'trace' with the difference
-- that the message is emitted to the eventlog, if eventlog profiling is
-- available and enabled at runtime.
traceEvent :: String #-> a #-> a
traceEvent = Unsafe.toLinear2 NonLinear.traceEvent

-- | The 'traceEventIO' function emits a message to the eventlog, if
-- eventlog profiling is available and enabled at runtime.
traceEventIO :: String #-> IO ()
traceEventIO s = fromSystemIO (Unsafe.toLinear NonLinear.traceEventIO s)

-- | The 'traceMarker' function emits a marker to the eventlog, if eventlog
-- profiling is available and enabled at runtime. The @String@ is the name
-- of the marker. The name is just used in the profiling tools to help you
-- keep clear which marker is which.
traceMarker :: String #-> a #-> a
traceMarker = Unsafe.toLinear2 NonLinear.traceMarker

-- | The 'traceMarkerIO' function emits a marker to the eventlog, if
-- eventlog profiling is available and enabled at runtime.
traceMarkerIO :: String #-> IO ()
traceMarkerIO s = fromSystemIO (Unsafe.toLinear NonLinear.traceMarkerIO s)

