{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Data.Array.Polarized.Pull.Internal where

import qualified Data.Functor.Linear as Data
import Prelude.Linear
import qualified Prelude
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Monoid.Linear

import qualified Unsafe.Linear as Unsafe

-- | A pull array represents arrays which are easy to extract elements from.
-- They provide an indexing function, and a length. If the indexing function
-- is called at a point out of range, the result is an error.
data Array a where
  Array :: (Int -> a) -> Int -> Array a
  deriving Prelude.Semigroup via NonLinear (Array a)
-- Int is movable, so this is the same as Array :: (Int ->. a) -> ...

instance Data.Functor Array where
  fmap f (Array g n) = fromFunction (\x -> f (g x)) n

-- XXX: This should be well-typed without the unsafe, but it isn't accepted: the
-- PullArray type probably isn't the ideal choice
-- (making Array linear in (Int -> a) would mean only one value could be
-- taken out of the Array (which is interesting in and of itself: I think this
-- is like an n-ary With), and changing the other arrows makes no difference)
singleton :: a ->. Array a
singleton = Unsafe.toLinear (\x -> fromFunction (\_ -> x) 1)

-- | /!\ Partial! Only works if both arrays have the same length.
zip :: Array a ->. Array b ->. Array (a,b)
zip (Array g n) (Array h m)
  | n /= m    = error "Polarized.zip: size mismatch"
  | otherwise = fromFunction (\k -> (g k, h k)) n

append :: Array a ->. Array a ->. Array a
append (Array f m) (Array g n) = Array (\k -> if k < m then f k else g (k-m)) (m + n)

-- | Creates a pull array of given size, filled with the given element
make :: a -> Int -> Array a
make x n = fromFunction (const x) n

instance Semigroup (Array a) where
  (<>) = append

foldr :: (a ->. b ->. b) -> b ->. Array a ->. b
foldr f z (Array g n) = go f z g n
  where go :: (_ ->. _ ->. _) -> _ ->. _ -> _ -> _
        go _ z' _ 0 = z'
        go f' z' g' k = go f' (f' (g' (k-1)) z') g' (k-1)
        -- go is strict in its last argument

-- | Extracting the length of an array doesn't count as consuming it.
findLength :: Array a ->. (Int, Array a)
findLength (Array f n) = (n, Array f n)

-- | A smart constructor for PullArrays
fromFunction :: (Int -> a) -> Int -> Array a
fromFunction f n = Array f' n
  where f' k
          | k < 0 = error "Pull.Array: negative index"
          | k >= n = error "Pull.Array: index too large"
          | otherwise = f k
-- XXX: this is used internally to ensure out of bounds errors occur, but
-- is unnecessary if the input function can be assumed to already have bounded
-- domain

-- | This is a shortcut function, which does the same thing as
-- `alloc` . `transfer`
toVector :: Array a ->. Vector a
toVector (Array f n) = Vector.generate n f
-- TODO: A test to make sure alloc . transfer == toVector

-- | @'split' n v = (vl, vr)@ such as @vl@ has length @n@.
--
-- 'split' is total: if @n@ is larger than the length of @v@, then @vr@ is
-- empty.
split :: Int -> Array a ->. (Array a, Array a)
split k (Array f n) = (fromFunction f (min k n), fromFunction (\x -> f (x+k)) (max (n-k) 0))

reverse :: Array a ->. Array a
reverse (Array f n) = Array (\x -> f (n+1-x)) n
