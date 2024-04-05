{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Array.Polarized.Pull.Internal where

import qualified Data.Functor.Linear as Data
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Prelude.Linear
import qualified Unsafe.Linear as Unsafe
import qualified Prelude

-- | A pull array is an array from which it is easy to extract elements, and
-- this can be done in any order. The linear consumption of a pull array means
-- each element is consumed exactly once, but the length can be accessed
-- freely.
data Array a where
  Array :: (Int -> a) -> Int -> Array a
  deriving (Prelude.Semigroup) via NonLinear (Array a)

-- In the linear consumption of a pull array f n, (f i) should be consumed
-- linearly for every 0 <= i < n. The exported functions (from non-internal
-- modules) should enforce this invariant, but the current type of PullArray
-- does not.

instance Data.Functor Array where
  fmap f (Array g n) = fromFunction (\x -> f (g x)) n

-- XXX: This should be well-typed without the unsafe, but it isn't accepted:
-- the pull array type probably isn't the ideal choice (making Array linear in
-- (Int -> a) would mean only one value could be taken out of the Array (which
-- is interesting in and of itself: I think this is like an n-ary With), and
-- changing the other arrows makes no difference)

-- | Create an empty pull array
empty :: Array a
empty = fromFunction (\_ -> error "Data.Array.Polarized.Pull.Internal.empty: this should never be called") 0

-- | Produce a pull array of lenght 1 consisting of solely the given element.
singleton :: a %1 -> Array a
singleton = Unsafe.toLinear (\x -> fromFunction (\_ -> x) 1)

-- | @zip [x1, ..., xn] [y1, ..., yn] = [(x1,y1), ..., (xn,yn)]@
-- __Partial:__ `zip [x1,x2,...,xn] [y1,y2,...,yp]` is an error if @n ≠ p@.
zip :: Array a %1 -> Array b %1 -> Array (a, b)
zip (Array g n) (Array h m)
  | n /= m = error "Polarized.zip: size mismatch"
  | otherwise = fromFunction (\k -> (g k, h k)) n

-- | Concatenate two pull arrays.
append :: Array a %1 -> Array a %1 -> Array a
append (Array f m) (Array g n) = Array h (m + n)
  where
    h k =
      if k < m
        then f k
        else g (k - m)

-- | Creates a pull array of given size, filled with the given element.
make :: a -> Int -> Array a
make x n = fromFunction (const x) n

instance Semigroup (Array a) where
  (<>) = append

-- | A right-fold of a pull array.
foldr :: (a %1 -> b %1 -> b) -> b %1 -> Array a %1 -> b
foldr f z (Array g n) = go f z g n
  where
    go :: (_) => (_ %1 -> _ %1 -> _) -> _ %1 -> _ -> _ -> _
    go _ z' _ 0 = z'
    go f' z' g' k = go f' (f' (g' (k - 1)) z') g' (k - 1)

-- go is strict in its last argument

-- | Extract the length of an array, and give back the original array.
findLength :: Array a %1 -> (Int, Array a)
findLength (Array f n) = (n, Array f n)

-- | @'fromFunction' arrIndexer len@ constructs a pull array given a function
-- @arrIndexer@ that goes from an array index to array values and a specified
-- length @len@.
fromFunction :: (Int -> a) -> Int -> Array a
fromFunction f n = Array f' n
  where
    f' k
      | k < 0 = error "Pull.Array: negative index"
      | k >= n = error "Pull.Array: index too large"
      | otherwise = f k

-- XXX: this is used internally to ensure out of bounds errors occur, but
-- is unnecessary if the input function can be assumed to already have bounded
-- domain, for instance in `append`.

-- | This is a convenience function for @alloc . transfer@
toVector :: Array a %1 -> Vector a
toVector (Array f n) = Vector.generate n f

-- TODO: A test to make sure alloc . transfer == toVector

-- | @'split' n v = (vl, vr)@ such that @vl@ has length @n@.
--
-- 'split' is total: if @n@ is larger than the length of @v@,
-- then @vr@ is empty.
split :: Int -> Array a %1 -> (Array a, Array a)
split k (Array f n) = (fromFunction f (min k n), fromFunction (\x -> f (x + k)) (max (n - k) 0))

-- | Reverse a pull array.
reverse :: Array a %1 -> Array a
reverse (Array f n) = Array (\x -> f (n + 1 - x)) n

-- | Decompose an array into its head and tail, returns @Nothing@ if the array is empty.
uncons :: Array a %1 -> Maybe (a, Array a)
uncons (Array _ 0) = Nothing
uncons (Array f n) = Just (f 0, fromFunction (\x -> f (x + 1)) (n - 1))
