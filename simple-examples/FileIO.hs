{-# LANGUAGE LinearTypes #-}

module FileIO where

import Unsafe.Coerce ( unsafeCoerce )
import qualified System.IO as S
import Control.Monad

import Data.Unrestricted.Linear
import System.IO.Resource


-- openFile :: FilePath -> IOMode -> IO Handle
-- IOMode = S.ReadMode | WriteMode | AppendMode | ReadWriteMode
-- hGetLine :: Handle -> IO String
-- hPutStr :: Handle -> String -> IO ()
-- hClose :: Handle -> IO ()




{- Non-linear first line printing -}
--------------------------------------------


printFirstLine :: S.FilePath -> IO ()
printFirstLine fpath = do
  fileHandle <- S.openFile fpath S.ReadMode
  firstLine <- S.hGetLine fileHandle
  putStrLn firstLine
  S.hClose fileHandle


printFirstLineNoClose :: S.FilePath -> IO ()
printFirstLineNoClose fpath = do
  fileHandle <- S.openFile fpath S.ReadMode
  firstLine <- S.hGetLine fileHandle
  putStrLn firstLine


printFirstLineAfterClose :: S.FilePath -> IO ()
printFirstLineAfterClose fpath = do
  fileHandle <- S.openFile fpath S.ReadMode
  S.hClose fileHandle
  firstLine <- S.hGetLine fileHandle
  putStrLn firstLine


{- Linear first line printing -}
--------------------------------------------

-- This fails because the RIO instance isn't imported
-- Why???

{-
doNothing :: FilePath -> RIO ()
doNothing fp = do
  handle <- openFile fp S.ReadMode
  hClose handle
-}




