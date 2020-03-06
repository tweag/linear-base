{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module FileIO where

import Unsafe.Coerce ( unsafeCoerce )
import qualified System.IO as Sys
import Control.Monad
import Data.Text


import qualified Control.Monad.Builder as Unrestricted
import qualified Control.Monad.Linear as Control
import qualified Control.Monad.Linear.Builder as Control
import Data.Unrestricted.Linear
import System.IO.Resource
import System.IO.Linear


{- Non-linear first line printing -}
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


{- Linear first line printing -}
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





