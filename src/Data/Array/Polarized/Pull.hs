{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides pull arrays.
--
-- Use pull arrays to force fusion (i.e., force GHC to not allocate arrays in
-- the garbage collector memory) in a computation that only uses lists
-- as /intermediate/ results without operations that \"write\" to a list.
-- You should think about pull arrays as arrays you can \"pull\" or read from,
-- zip with and map over. These are arrays that work nicely as arguments to
-- functions and poorly as arrays to write to.
--
-- Pull arrays fit within a larger framework for controlling when
-- garbage collector memory is allocated; please see "Data.Array.Polarized".
--
-- Import this module qualified for clarity and to avoid
-- name clashes.
--
-- == Example
--
-- > import qualified Data.Array.Polarized.Pull as Pull
-- > import Data.Vector (Vector, (!), fromList)
-- > import qualified Prelude as P
-- >
-- > type Pull a = Pull.Array a
-- >
-- > -- | compute the norm of summing three vectors
-- > -- for vectors x,y,z, this computes || x + y + z ||
-- > pullArrExample :: IO ()
-- > pullArrExample = do
-- >   x <- inputVectorX
-- >   y <- inputVectorY
-- >   z <- inputVectorZ
-- >   let x_pull = Pull.fromVector x
-- >   let y_pull = Pull.fromVector y
-- >   let z_pull = Pull.fromVector z
-- >   let nsum = normSumExample x_pull y_pull z_pull
-- >   putStrLn P.$ "norm sum is " ++ show nsum
-- >
-- > normSumExample :: Pull Int -> Pull Int -> Pull Int -> Double
-- > normSumExample x y z = norm (sumV (sumV x y) z)
-- >
-- > sumV :: Pull Int %1-> Pull Int %1-> Pull Int
-- > sumV = Pull.zipWith (Linear.+)
-- >
-- > norm :: Pull Int -> Double
-- > norm arr = sqrt P.$ fromIntegral P.$
-- >   Pull.foldr (Linear.+) 0 (Linear.fmap square arr)
-- >
-- > square :: Int %1-> Int
-- > square i = fromDup (dup2 i)
-- >   where
-- >     fromDup :: (Int, Int) %1-> Int
-- >     fromDup (i,j) = i Linear.* j
-- >
-- > -- | Query from environment
-- > inputVectorX :: IO (Vector Int)
-- > inputVectorX = return (fromList [1..100])
-- >
-- > -- | Query from environment
-- > inputVectorY :: IO (Vector Int)
-- > inputVectorY = return (fromList (map (\x -> (7 * (x+3)) `div` 11) [1..100]))
-- >
-- > -- | Query from environment
-- > inputVectorZ :: IO (Vector Int)
-- > inputVectorZ = return (fromList [negate i | i <- [1..100]])
module Data.Array.Polarized.Pull
  ( Array
    -- * Construction
  , fromFunction
  , fromVector
  , make
  , singleton
    -- * Consumption
  , toVector
  , asList
    -- * Operations
  , zip, zipWith
  , append
  , foldr
  , foldMap
  , findLength
  , split
  , reverse
  )
  where

import Data.Array.Polarized.Pull.Internal
-- XXX: the data constructor Pull.Array could be used unsafely, so we don't
-- export it, instead exporting a collection of functions to manipulate
-- PullArrays
-- (eg one could use an element multiple times, if the constructor was
-- available)
-- TODO: the current collection is almost certainly not complete: it would be
-- nice if there was one (or a small number) of functions which characterise
-- PullArrays, but I'm not sure what they are
-- In particular, PullArrays are incredibly unfriendly in returned-value
-- position at the moment, moreso than they should be
import qualified Data.Functor.Linear as Data
import Prelude.Linear hiding (zip, zipWith, foldr, foldMap, reverse)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Unsafe.Linear as Unsafe

-- | Convert a pull array into a list.
asList :: Array a %1-> [a]
asList = foldr (\x xs -> x:xs) []

-- | @zipWith f [x1,x2,...] [y1,y2,...] == [f x1 y1, f x2 y2, ...]@
-- __Partial:__ Only works if both arrays have the same length.
zipWith :: (a %1-> b %1-> c) -> Array a %1-> Array b %1-> Array c
zipWith f x y = Data.fmap (uncurry f) (zip x y)

-- | Fold a pull array using a monoid.
foldMap :: Monoid m => (a %1-> m) -> Array a %1-> m
foldMap f = foldr ((<>) . f) mempty

-- I'm fairly sure this can be used safely
-- | Convert a Vector to a pull array.
fromVector :: Vector a %1-> Array a
fromVector = Unsafe.toLinear $ \v -> fromFunction (v Vector.!) (Vector.length v)
