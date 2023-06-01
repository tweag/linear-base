{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- |
-- Module      : FileIO
-- Description : The Linear File IO example from the Linear Haskell paper.
--
-- We implement a function that prints the first line of a file.
--
-- We do this with the normal file IO interface in base and the linear file IO
-- interface in linear-base. With the latter, the protocol for using files is
-- enforced by the linear type system. For instance, forgetting to close the file
-- will induce a type error at compile time. That is, typechecking proves that all
-- opened files are closed at some later point in execution. With the former
-- interface, we have code that type checks but will error or cause errors at
-- runtime.
module Simple.FileIO where

-- Linear Base Imports
import qualified Control.Functor.Linear as Control
import Control.Monad ()
import Data.Text
import Data.Unrestricted.Linear
import qualified System.IO as System
import qualified System.IO.Resource.Linear as Linear
import Prelude

-- *  Non-linear first line printing

--------------------------------------------

-- openFile :: FilePath -> IOMode -> IO Handle
-- IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- hGetLine :: Handle -> IO String
-- hPutStr :: Handle -> String -> IO ()
-- hClose :: Handle -> IO ()

printFirstLine :: FilePath -> System.IO ()
printFirstLine fpath = do
  fileHandle <- System.openFile fpath System.ReadMode
  firstLine <- System.hGetLine fileHandle
  System.putStrLn firstLine
  System.hClose fileHandle

-- This compiles but can cause issues!
-- The number of file handles you can have active is finite and after that
-- openFile errors. This is especially critical on mobile devices or systems
-- with limited resources.
printFirstLineNoClose :: FilePath -> System.IO ()
printFirstLineNoClose fpath = do
  fileHandle <- System.openFile fpath System.ReadMode
  firstLine <- System.hGetLine fileHandle
  System.putStrLn firstLine

-- This compiles, but will throw an error!
printFirstLineAfterClose :: FilePath -> System.IO ()
printFirstLineAfterClose fpath = do
  fileHandle <- System.openFile fpath System.ReadMode
  System.hClose fileHandle
  firstLine <- System.hGetLine fileHandle
  System.putStrLn firstLine

-- * Linear first line printing

--------------------------------------------

linearGetFirstLine :: FilePath -> RIO (Ur Text)
linearGetFirstLine fp = Control.do
  handle <- Linear.openFile fp System.ReadMode
  (t, handle') <- Linear.hGetLine handle
  Linear.hClose handle'
  Control.return t

linearPrintFirstLine :: FilePath -> System.IO ()
linearPrintFirstLine fp = do
  text <- Linear.run (linearGetFirstLine fp)
  System.putStrLn (unpack text)

{-
    For clarity, we show this function without do notation.

    Note that the current approach is limited.
    We have to make the continuation use the unit type.

    Enabling a more generic approach with a type index
    for the multiplicity, as descibed in the paper is a work in progress.
    This will hopefully result in using

    `(>>==) RIO 'Many a %1-> (a -> RIO p b) %1-> RIO p b`

    as the non-linear bind operation.
    See https://github.com/tweag/linear-base/issues/83.
-}

-- * Linear and non-linear combinators

-------------------------------------------------

-- Some type synonyms
type RIO = Linear.RIO

type LinHandle = Linear.Handle

-- | Linear bind
-- Notice the continuation has a linear arrow,
-- i.e., (a %1-> RIO b)
(>>#=) :: RIO a %1 -> (a %1 -> RIO b) %1 -> RIO b
(>>#=) = (Control.>>=)

infixl 1 >>#= -- same fixity as base.>>=

-- | Non-linear bind
-- Notice the continuation has a non-linear arrow,
-- i.e., (() -> RIO b). For simplicity, we don't use
-- a more general type, like the following:
-- (>>==) :: RIO (Ur a) %1-> (a -> RIO b) %1-> RIO b
(>>==) :: RIO () %1 -> (() -> RIO b) %1 -> RIO b
(>>==) ma f = ma Control.>>= (\() -> f ())

infixl 1 >>== -- same fixity as base.>>=

-- | Inject
-- provided just to make the type explicit
inject :: a %1 -> RIO a
inject = Control.return

-- * The explicit example

-------------------------------------------------

getFirstLineExplicit :: FilePath -> RIO (Ur Text)
getFirstLineExplicit path =
  (openFileForReading path)
    >>#= readOneLine
    >>#= closeAndReturnLine -- Internally uses (>>==)
  where
    openFileForReading :: FilePath -> RIO LinHandle
    openFileForReading fp = Linear.openFile fp System.ReadMode
    readOneLine :: LinHandle %1 -> RIO (Ur Text, LinHandle)
    readOneLine = Linear.hGetLine
    closeAndReturnLine ::
      (Ur Text, LinHandle) %1 -> RIO (Ur Text)
    closeAndReturnLine (unrText, handle) =
      Linear.hClose handle >>#= (\() -> inject unrText)

printFirstLineExplicit :: FilePath -> System.IO ()
printFirstLineExplicit fp = do
  firstLine <- Linear.run $ getFirstLineExplicit fp
  putStrLn $ unpack firstLine
