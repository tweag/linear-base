{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-|
Linear versions of 'Data.List' functions.

This module only contains minimal amount of documentation; consult the
original "Data.List" module for more detailed information.
-}
module Data.List.Linear
  ( -- * Basic functions
    map
  , filter
  , uncons
  , reverse
  , length
  , splitAt
  , span
  , intersperse
  , intercalate
  , transpose
  -- * Folds
  , foldr
  , foldl
  , foldl'
  , foldMap
  , foldMap'
  -- * Special folds
  , concat
  , concatMap
  , and
  , or
  , any
  , all
  -- * Building lists
  , scanl
  , replicate
  , unfoldr
  -- * Zipping lists
  , zip
  , zip'
  , zip3
  , zipWith
  , zipWith'
  , zipWith3
  ) where

import qualified Unsafe.Linear as Unsafe
import qualified Prelude as Prelude
import Prelude (Maybe(..), Either(..), Int, otherwise)
import Data.Num.Linear
import Prelude.Linear.Internal
import Data.Bool.Linear
import Data.Unrestricted.Linear
import Data.Functor.Linear
import Data.Monoid.Linear
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List as NonLinear

-- # Basic functions
--------------------------------------------------

map :: (a #-> b) -> [a] #-> [b]
map = fmap

-- | @filter p xs@ returns a list with elements satisfying the predicate.
--
-- See 'Data.Maybe.Linear.mapMaybe' if you do not want the 'Dupable' constraint.
filter :: Dupable a => (a #-> Bool) -> [a] #-> [a]
filter _ [] = []
filter p (x:xs) =
  dup x & \case
    (x', x'') ->
      if p x'
      then x'' : filter p xs
      else x'' `lseq` filter p xs

uncons :: [a] #-> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

reverse :: [a] #-> [a]
reverse = Unsafe.toLinear NonLinear.reverse

-- | Return the length of the given list alongside with the list itself.
length :: [a] #-> ([a], Ur Int)
length = Unsafe.toLinear $ \xs ->
  (xs, Ur (NonLinear.length xs))
-- We can only do this because of the fact that 'NonLinear.length'
-- does not inspect the elements.

--  'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list.
splitAt :: Int -> [a] #-> ([a], [a])
splitAt i = Unsafe.toLinear (Prelude.splitAt i)

-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list.
span :: Dupable a => (a #-> Bool) -> [a] #-> ([a], [a])
span _ [] = ([], [])
span f (x:xs) = dup x & \case
  (x', x'') ->
    if f x'
    then span f xs & \case (ts, fs) -> (x'':ts, fs)
    else ([x''], xs)

-- | The intersperse function takes an element and a list and
-- `intersperses' that element between the elements of the list.
intersperse :: a -> [a] #-> [a]
intersperse sep = Unsafe.toLinear (NonLinear.intersperse sep)

-- | @intercalate xs xss@ is equivalent to @(concat (intersperse xs
-- xss))@. It inserts the list xs in between the lists in xss and
-- concatenates the result.
intercalate :: [a] -> [[a]] #-> [a]
intercalate sep = Unsafe.toLinear (NonLinear.intercalate sep)

-- | The transpose function transposes the rows and columns of its argument.
transpose :: [[a]] #-> [[a]]
transpose = Unsafe.toLinear NonLinear.transpose

-- # Folds
--------------------------------------------------

foldr :: (a #-> b #-> b) -> b #-> [a] #-> b
foldr f = Unsafe.toLinear2 (NonLinear.foldr (\a b -> f a b))

foldl :: (b #-> a #-> b) -> b #-> [a] #-> b
foldl f = Unsafe.toLinear2 (NonLinear.foldl (\b a -> f b a))

foldl' :: (b #-> a #-> b) -> b #-> [a] #-> b
foldl' f = Unsafe.toLinear2 (NonLinear.foldl' (\b a -> f b a))

-- | Map each element of the structure to a monoid,
-- and combine the results.
foldMap :: Monoid m => (a #-> m) -> [a] #-> m
foldMap f = foldr ((<>) . f) mempty

-- | A variant of 'foldMap' that is strict in the accumulator.
foldMap' :: Monoid m => (a #-> m) ->  [a] #-> m
foldMap' f = foldl' (\acc a -> acc <> f a) mempty

concat :: [[a]] #-> [a]
concat = Unsafe.toLinear NonLinear.concat

concatMap :: (a #-> [b]) -> [a] #-> [b]
concatMap f = Unsafe.toLinear (NonLinear.concatMap (forget f))

-- | __NOTE:__ This does not short-circuit, and always consumes the
-- entire container.
any :: (a #-> Bool) -> [a] #-> Bool
any p = foldl' (\b a -> b || p a) False

-- | __NOTE:__ This does not short-circuit, and always consumes the
-- entire container.
all :: (a #-> Bool) -> [a] #-> Bool
all p = foldl' (\b a -> b && p a) True

-- | __NOTE:__ This does not short-circuit, and always consumes the
-- entire container.
and :: [Bool] #-> Bool
and = foldl' (&&) True

-- | __NOTE:__ This does not short-circuit, and always consumes the
-- entire container.
or :: [Bool] #-> Bool
or = foldl' (||) False

-- # Building Lists
--------------------------------------------------

scanl :: Dupable b => (b #-> a #-> b) -> b #-> [a] #-> [b]
scanl _ b [] = [b]
scanl f b (x:xs) = dup2 b & \(b', b'') -> b' : scanl f (f b'' x) xs

replicate :: Dupable a => Int -> a #-> [a]
replicate i a
  | i Prelude.< 1 = a `lseq` []
  | i Prelude.== 1 = [a]
  | otherwise  = dup2 a & \(a', a'') -> a' : replicate (i-1) a''

unfoldr :: (b #-> Maybe (a, b)) -> b #-> [a]
unfoldr f = Unsafe.toLinear (NonLinear.unfoldr (forget f))

-- # Zipping Lists
--------------------------------------------------

zip :: (Consumable a, Consumable b) => [a] #-> [b] #-> [(a, b)]
zip = zipWith (,)

-- | Same as 'zip', but returns the leftovers instead of consuming them.
zip' :: [a] #-> [b] #-> ([(a, b)], Maybe (Either (NonEmpty a) (NonEmpty b)))
zip' = zipWith' (,)

zip3 :: (Consumable a, Consumable b, Consumable c) => [a] #-> [b] #-> [c] #-> [(a, b, c)]
zip3 = zipWith3 (,,)

zipWith :: (Consumable a, Consumable b) => (a#->b#->c) -> [a] #-> [b] #-> [c]
zipWith f xs ys =
  zipWith' f xs ys & \(ret, leftovers) ->
    leftovers `lseq` ret

-- | Same as 'zipWith', but returns the leftovers instead of consuming them.
zipWith' :: (a#->b#->c) -> [a] #-> [b] #-> ([c], Maybe (Either (NonEmpty a) (NonEmpty b)))
zipWith' _ [] [] = ([], Nothing)
zipWith' _ (a:as) [] = ([], Just (Left (a :| as)))
zipWith' _ [] (b:bs) = ([], Just (Right (b :| bs)))
zipWith' f (a:as) (b:bs) = zipWith' f as bs & \case
  (cs, rest) -> (f a b : cs, rest)

zipWith3 :: forall a b c d. (Consumable a, Consumable b, Consumable c) => (a#->b#->c#->d) -> [a] #-> [b] #-> [c] #-> [d]
zipWith3 _ [] ys zs = (ys, zs) `lseq` []
zipWith3 _ xs [] zs = (xs, zs) `lseq` []
zipWith3 _ xs ys [] = (xs, ys) `lseq` []
zipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 f xs ys zs
