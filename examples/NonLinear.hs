{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides (unsafe) examples of using streams.
-- These examples serve to give a basic idea of how streams work
-- and the ways in which they can fail.
module NonLinear where

import Streaming
import qualified Streaming.Prelude as S
import Text.Read (readMaybe)
import System.IO


-- # Developing notes
--
-- reading lines from a file
-- writing to a file a line at a time
-- reading in a stream of values
-- doing a pure computation without using too much memory
--   via say iteration (e.g., try fibonacci)
--
-- using store/copy
--
-- Generally important:
-- filter, intersperse, take, drop, concat
--
-- read, show, next, split or splitAt
--
-- fold, foldM, all, any, sum, product, length
-- minimum, maximum,
--
-- zip, zipWith, unzip, merge (..),
--

-- # Example: Sum the even numbers from input
sumEvens :: IO Int
sumEvens = fmap S.fst' $ S.sum $ S.filter ((==) 0 . (`mod` 2)) readLn'

-- # Example: Sum evens and multiply odds
sumEvensMultOdds :: IO (Int, Int)
sumEvensMultOdds = do
  let x = S.product $ S.filter odd $ S.sum $ S.filter even $ S.copy readLn'
  p :> (s :> ())  <- x
  return (p,s)

-- # Example: Take the average of a stream using copy


-- # Example: Fibonacci and nth Fibonacci

-- Assuming input > 0
-- using a stream of fibs

-- # Example: Play hangman from input

-- # Example: Find lines in a file matching a word

-- # Example: Play a "guess the number game"

--guessNumber :: Int -> IO ()

-- # Example: Something that uses store well

-- # Example: Something that merges streams in order


-- # Example: BAD: Get three or all lines from a stream of two lines

streamTwoLines :: Stream (Of String) IO ()
streamTwoLines = do
  handle <- lift $ openFile "temp.txt" ReadMode
  line1 <- lift $ hGetLine handle
  S.yield line1
  line2 <- lift $ hGetLine handle
  S.yield line2
  lift $ hClose handle

printThreeLines :: IO ()
printThreeLines = do
  Right (line1, rest1) <- S.next streamTwoLines
  putStrLn line1
  Right (line2, rest2) <- S.next rest1
  putStrLn line2
  Right (line3, rest3) <- S.next rest1
  putStrLn line3

printAllLines :: IO ()
printAllLines = do
  Right (line1, rest1) <- S.next streamTwoLines
  putStrLn line1
  Right (line2, rest2) <- S.next rest1
  putStrLn line2
  printFrom rest1
  where
    printFrom stream = do
      Right (line, _) <- S.next stream
      putStrLn line
      printFrom stream


-- # Library
-------------------------------------------------------------------------------

readLn' :: Read a => Stream (Of a) IO ()
readLn' = S.catMaybes $ S.takeWhile isJust $ S.map readMaybe S.stdinLn
  where
    isJust :: Maybe a -> Bool
    isJust Nothing = False
    isJust _ = True




