-- | This module defines an IO monad for linearly working with system resources
-- like files. It provides tools to take resources that are currently
-- unsafely accessible from "System.IO" and use them in this monad.
--
-- Import this module qualified to avoid name clashes.
--
-- To use this RIO monad, create some @RIO@ computation,
-- run it to get a "System.IO" computation.
--
-- = A simple example
-- >>> :set -XLinearTypes
-- >>> :set -XQualifiedDo
-- >>> :set -XNoImplicitPrelude
-- >>> import qualified System.IO.Resource.Linear as Linear
-- >>> import qualified Control.Functor.Linear as Control
-- >>> import qualified Data.Text as Text
-- >>> import Prelude.Linear
-- >>> import qualified Prelude
-- >>> :{
--  linearWriteToFile :: IO ()
--  linearWriteToFile = Linear.run $ Control.do
--    handle1 <- Linear.openFile "/home/user/test.txt" Linear.WriteMode
--    handle2 <- Linear.hPutStrLn handle1 (Text.pack "hello there")
--    () <- Linear.hClose handle2
--    Control.return (Ur ())
-- :}
--
-- To enable do notation, `QualifiedDo` extension is used. But since QualifiedDo
-- only modifies the desugaring of binds, we still need to qualify `Control.return`.
module System.IO.Resource.Linear
  ( -- * The Resource I/O Monad
    RIO,
    run,

    -- * Using Resource Handles
    -- $monad
    -- $files
    Handle,

    -- ** File I/O
    openFile,
    openBinaryFile,
    System.IOMode (..),

    -- ** Working with Handles
    hClose,
    hIsEOF,
    hGetChar,
    hPutChar,
    hGetLine,
    hPutStr,
    hPutStrLn,
    hSeek,
    System.SeekMode(..),

    -- * Creating new types of resources
    -- $new-resources
    UnsafeResource,
    unsafeRelease,
    unsafeAcquire,
    unsafeFromSystemIOResource,
    unsafeFromSystemIOResource_,
  )
where

import qualified System.IO as System
import System.IO.Resource.Linear.Internal
