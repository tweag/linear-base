{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides (unsafe) examples of using streams.
-- These examples serve to give a basic idea of how streams work
-- and the ways in which they can fail.
module NonLinear where

import Streaming
import qualified Streaming.Prelude as S
import Data.Function ((&))
import Text.Read (readMaybe)
import Data.Functor.Identity
import System.IO
import Data.List (isInfixOf)
import Control.Monad (forever)

-- TODO
-- * Number the examples
-- * Add enough comments
-- * Create counter-examples that fail


-- Streaming stuff that seems important to me:
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



-- # Example: Sum the even numbers from input

sumEvens :: IO ()
sumEvens = do
  (evenSum :> _) <- S.sum $ S.filter even readUntilParseFails
  putStrLn ("Sum of evens:" ++ show evenSum)

-- # Example: Sum evens and multiply odds

sumEvenMultOdd :: IO ()
sumEvenMultOdd = do
  let readInt = readUntilParseFails :: Stream (Of Int) IO ()
  let partitioned = S.partition even readInt
  (oddProduct :> (evenSum :> ())) <- S.product $ S.sum $ partitioned
  putStrLn $ "Even sum: " ++ show evenSum
  putStrLn $ "Odd product: " ++ show oddProduct

-- # Example: Fibonacci and nth Fibonacci

-- Assuming input > 0
fib :: Integer -> Integer
fib n = case maybeNthFib of
  Just (_,val) -> val
  where
    intN = (fromIntegral n)
    maybeNthFib = runIdentity $ S.head_ $ S.drop (intN - 2) $ fibStream
    fibStream = S.iterate (\(fn, fnp1) -> (fnp1, fn+fnp1)) (1,1)

-- # Example: Take the average of a stream using copy

average :: Monad m => Stream (Of Double) m () -> m Double
average stream = do
  len :> (sum :> ()) <- S.length $ S.sum $ S.copy stream
  return (sum / fromIntegral len)

averageList :: [Double] -> Double
averageList = runIdentity . average . S.each

-- # Example: Find lines in a file matching a word

findMatchingLines :: String -> FilePath -> IO [Int]
findMatchingLines word filepath = do
  fileHandle <- openFile filepath ReadMode
  (xs :> ()) <- S.toList $
    S.mapMaybe (checkSubStr word) $
    S.zip (S.each [1..]) $ S.fromHandle fileHandle
  return xs

checkSubStr :: String -> (Int, String) -> Maybe Int
checkSubStr word (ix, line)
  | True <- isInfixOf word line = Just ix
  | otherwise = Nothing


-- # Example: Play a "guess the number game"

guessNumber :: Int -> IO ()
guessNumber answer = do
  _ <- S.effects $
    S.chain (\_ -> putStrLn "Bad guess. Try again.") $
    S.break (\read -> read == answer) $ S.readLn
  return ()

-- # Example: Something that uses store well

-- # Example: Something that merges streams in order

-- # Example: Play hangman


-- TODO: PUT IN ANOTHER FILE
-- # Example: A minimal example incorrectly using an outdated stream ref


readNLines :: String -> Int -> Stream (Of String) IO ()
readNLines filePath n = do
  handle <- lift $ openFile filePath ReadMode
  readNLinesWithHandle handle n

-- | Assuming the integer is a natural (so >= 0)
readNLinesWithHandle :: Handle -> Int -> Stream (Of String) IO ()
readNLinesWithHandle handle 0 = lift $ hClose handle
readNLinesWithHandle handle n = do
  line <- lift $ hGetLine handle
  S.yield line
  readNLinesWithHandle handle (n-1)




-- | Acceptable defintion
streamTwoLines :: Stream (Of String) IO ()
streamTwoLines = do
    -- open up a file handle
    handle <- lift $ openFile "temp.txt" ReadMode 
    -- Stream two lines
    str <- lift $ hGetLine handle
    S.yield str
    str' <- lift $ hGetLine handle
    S.yield str'
    -- close the file handle again
    lift $ hClose handle

-- | Minimal bad example
badRead :: IO ()
badRead = do
    Right (v1, s) <- S.next streamTwoLines
    putStrLn v1
    Right (v1, s') <- S.next s
    putStrLn v1
    Right (v1, s'') <- S.next s -- non linear use of s
    putStrLn v1


-- | Even worse bad example
infRead :: IO ()
infRead = do
    Right (v1, s) <- S.next streamTwoLines
    putStrLn v1
    let infUseOfS = do
        Right (secret, _) <- S.next s
        putStrLn secret
        infUseOfS
    infUseOfS






-- # Example: many more examples that go wrong with non-linear streams



-- # Library
-------------------------------------------------------------------------------


readUntilParseFails :: Read a => Stream (Of a) IO ()
readUntilParseFails = fmap (\_ -> ()) $
  S.catMaybes $ S.break isNothing $ S.map readMaybe S.stdinLn
  where
    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing _ = False


