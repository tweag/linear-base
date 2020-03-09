{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving     #-}

module FileIO where

import Prelude
import qualified System.IO as Sys
import Control.Monad ()
import Data.Text

-- Linear Base Imports
import qualified Control.Monad.Linear.Builder as Control
import Data.Unrestricted.Linear
import System.IO.Resource


-- |  Non-linear first line printing
--------------------------------------------

-- openFile :: FilePath -> IOMode -> IO Handle
-- IOMode = Sys.ReadMode | WriteMode | AppendMode | ReadWriteMode
-- hGetLine :: Handle -> IO String
-- hPutStr :: Handle -> String -> IO ()
-- hClose :: Handle -> IO ()

printFirstLine :: Sys.FilePath -> Sys.IO ()
printFirstLine fpath = do
  fileHandle <- Sys.openFile fpath Sys.ReadMode
  firstLine <- Sys.hGetLine fileHandle
  Sys.putStrLn firstLine
  Sys.hClose fileHandle


printFirstLineNoClose :: Sys.FilePath -> Sys.IO ()
printFirstLineNoClose fpath = do
  fileHandle <- Sys.openFile fpath Sys.ReadMode
  firstLine <- Sys.hGetLine fileHandle
  Sys.putStrLn firstLine


printFirstLineAfterClose :: Sys.FilePath -> Sys.IO ()
printFirstLineAfterClose fpath = do
  fileHandle <- Sys.openFile fpath Sys.ReadMode
  Sys.hClose fileHandle
  firstLine <- Sys.hGetLine fileHandle
  Sys.putStrLn firstLine


-- | Linear first line printing
--------------------------------------------

linearGetFirstLine :: Sys.FilePath -> RIO (Unrestricted Text)
linearGetFirstLine fp = do
    handle <- openFile fp Sys.ReadMode
    (t, handle') <- hGetLine handle
    hClose handle'
    return t
  where
    Control.Builder{..} = Control.monadBuilder

linearPrintFirstLine :: Sys.FilePath -> Sys.IO ()
linearPrintFirstLine fp = do
  text <- run (linearGetFirstLine fp)
  Sys.putStrLn (unpack text)


{-
    For clarity, we show this function without do notation.

    Note that the current approach is limited.
    We have to make the continuation use the unit type.

    Enabling a more generic approach with a type index
    for the multiplicity, as descibed in the paper is a work in progress.
    This will hopefully result in using

    `(>>==) RIO 'Many a #-> (a -> RIO p b) #-> RIO p b`

    as the linear bind operation.
-}

-- | Linear and non-linear combinators
-------------------------------------------------

-- | Linear bind
-- Notice the continuation has a linear arrow,
-- i.e., (a #-> RIO b)
(>>#=) :: RIO a #-> (a #-> RIO b) #-> RIO b
(>>#=) = (>>=)
  where
    Control.Builder{..} = Control.monadBuilder

-- | Non-linear bind
-- Notice the continuation has a non-linear arrow,
-- i.e., (() -> RIO b)
(>>==) :: RIO () #-> (() -> RIO b) #-> RIO b
(>>==) ma f = ma >> (f ())
  where
    Control.Builder{..} = Control.monadBuilder

-- | Inject
-- provided just to make the type explicit
inject :: a #-> RIO a
inject = return
  where
    Control.Builder{..} = Control.monadBuilder


-- | The explicit example
-------------------------------------------------

getFirstLineExplicit :: Sys.FilePath -> RIO (Unrestricted Text)
getFirstLineExplicit path =
  (openFileForReading path) >>#=
    readOneLine >>#=
      closeAndReturnLine
  where
    openFileForReading :: Sys.FilePath -> RIO Handle
    openFileForReading fp = openFile fp Sys.ReadMode

    readOneLine :: Handle #-> RIO (Unrestricted Text, Handle)
    readOneLine = hGetLine

    closeAndReturnLine ::
      (Unrestricted Text, Handle) #-> RIO (Unrestricted Text)
    closeAndReturnLine (unrText,handle) =
      hClose handle >>== (\_ -> inject unrText)


printFirstLineExplicit :: Sys.FilePath -> Sys.IO ()
printFirstLineExplicit fp = do
  firstLine <- run $ getFirstLineExplicit fp
  putStrLn $ unpack firstLine
