{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
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

import qualified Unsafe.Linear as Unsafe

-- | A pull array represents arrays which are easy to extract elements from.
-- They provide an indexing function, and a length. If the indexing function
-- is called at a point out of range, the result is unspecified.
data Array a where
  Array :: (Int -> a) -> Int -> Array a
-- Int is movable, so this is the same as Array :: (Int -> a) -> ...

instance Data.Functor Array where
  fmap f (Array g n) = Array (\x -> f (g x)) n

-- XXX: This doesn't match how PullArrays should work, but it is accepted
-- ap :: Array (x ->. y) ->. Array x ->. Array y
-- ap (Array g n) (Array h m) = Array (\k -> let (q,r) = k `quotRem` n in g q (h r)) (n*m)

-- XXX: This should be well-typed without the unsafe, but it isn't accepted: the
-- PullArray type probably isn't the ideal choice
-- (making Array linear in (Int -> a) would mean only one value could be
-- taken out of the Array (which is interesting in and of itself: I think this
-- is like an n-ary With), and changing the other arrows makes no difference)
singleton :: a ->. Array a
singleton = Unsafe.toLinear (\x -> Array (\_ -> x) 1)

-- | /!\ Partial! Only works if both arrays have the same length.
zip :: Array a ->. Array b ->. Array (a,b)
zip (Array g n) (Array h m)
  | n /= m    = error "Polarized.zip: size mismatch"
  | otherwise = Array (\k -> (g k, h k)) n

append :: Array a ->. Array a ->. Array a
append (Array f m) (Array g n) = Array (\k -> if k < m then f k else g k) (m + n)

-- | Creates a pull array of given size, filled with the given element
make :: a -> Int -> Array a
make x n = Array (const x) n

-- TODO: Use a derivingvia combinator to omit this
instance Prelude.Semigroup (Array a) where
  a <> b = append a b
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

fromFunction :: (Int -> a) -> Int -> Array a
fromFunction = Array

toVector :: Array a ->. Vector a
toVector (Array f n) = Vector.generate n f
