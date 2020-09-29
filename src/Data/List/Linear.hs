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
    (++)
  , map
  , filter
  , NonLinear.head
  , uncons
  , NonLinear.tail
  , NonLinear.last
  , NonLinear.init
  , reverse
  , NonLinear.lookup
  , length
  , NonLinear.null
    -- * Extracting sublists
  , take
  , drop
  , splitAt
  , span
  , partition
  , takeWhile
  , dropWhile
  , NonLinear.find
  , intersperse
  , intercalate
  , transpose
  -- * Folds
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  , foldr
  , foldr1
  , foldMap
  , foldMap'
  -- * Special folds
  , concat
  , concatMap
  , and
  , or
  , any
  , all
  , sum
  , product
  -- * Building lists
  , scanl
  , scanl1
  , scanr
  , scanr1
  , repeat
  , replicate
  , cycle
  , iterate
  , unfoldr
  -- * Ordered lists
  , NonLinear.sort
  , NonLinear.sortOn
  , NonLinear.insert
  -- * Zipping lists
  , zip
  , zip'
  , zip3
  , zipWith
  , zipWith'
  , zipWith3
  , unzip
  , unzip3
  ) where

import qualified Unsafe.Linear as Unsafe
import qualified Prelude as Prelude
import Prelude (Maybe(..), Either(..), Int)
import Prelude.Linear.Internal
import Data.Bool.Linear
import Data.Unrestricted.Linear
import Data.Functor.Linear
import Data.Monoid.Linear
import Data.Num.Linear
import Data.List.NonEmpty (NonEmpty ((:|)))
import GHC.Stack
import qualified Data.List as NonLinear

-- # Basic functions
--------------------------------------------------

(++) :: [a] #-> [a] #-> [a]
(++) = Unsafe.toLinear2 (NonLinear.++)

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
length :: [a] #-> (Ur Int, [a])
length = Unsafe.toLinear $ \xs ->
  (Ur (NonLinear.length xs), xs)
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

-- The partition function takes a predicate a list and returns the
-- pair of lists of elements which do and do not satisfy the predicate,
-- respectively.
partition :: Dupable a => (a #-> Bool) -> [a] #-> ([a], [a])
partition p (xs :: [a]) = foldr select ([], []) xs
 where
  select :: a #-> ([a], [a]) #-> ([a], [a])
  select x (ts, fs) =
    dup2 x & \(x', x'') ->
      if p x'
      then (x'':ts, fs)
      else (ts, x'':fs)

-- | __NOTE__: This does not short-circuit and always traverses the
-- entire list to consume the rest of the elements.
takeWhile :: Dupable a => (a #-> Bool) -> [a] #-> [a]
takeWhile _ [] = []
takeWhile p (x:xs) =
  dup2 x & \(x', x'') ->
    if p x'
    then x'' : takeWhile p xs
    else (x'', xs) `lseq` []

dropWhile :: Dupable a => (a #-> Bool) -> [a] #-> [a]
dropWhile _ [] = []
dropWhile p (x:xs) =
  dup2 x & \(x', x'') ->
    if p x'
    then x'' `lseq` dropWhile p xs
    else x'' : xs

-- | __NOTE__: This does not short-circuit and always traverses the
-- entire list to consume the rest of the elements.
take :: Consumable a => Int -> [a] #-> [a]
take _ [] = []
take i (x:xs)
  | i Prelude.< 0 = (x, xs) `lseq` []
  | otherwise = x : take (i-1) xs

drop :: Consumable a => Int -> [a] #-> [a]
drop _ [] = []
drop i (x:xs)
  | i Prelude.< 0 = x:xs
  | otherwise = x `lseq` drop (i-1) xs


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

foldr1 :: HasCallStack => (a #-> a #-> a) -> [a] #-> a
foldr1 f = Unsafe.toLinear (NonLinear.foldr1 (\a b -> f a b))

foldl :: (b #-> a #-> b) -> b #-> [a] #-> b
foldl f = Unsafe.toLinear2 (NonLinear.foldl (\b a -> f b a))

foldl' :: (b #-> a #-> b) -> b #-> [a] #-> b
foldl' f = Unsafe.toLinear2 (NonLinear.foldl' (\b a -> f b a))

foldl1 :: HasCallStack => (a #-> a #-> a) -> [a] #-> a
foldl1 f = Unsafe.toLinear (NonLinear.foldl1 (\a b -> f a b))

foldl1' :: HasCallStack => (a #-> a #-> a) -> [a] #-> a
foldl1' f = Unsafe.toLinear (NonLinear.foldl1' (\a b -> f a b))

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

sum :: AddIdentity a => [a] #-> a
sum = foldl' (+) zero

product :: MultIdentity a => [a] #-> a
product = foldl' (*) one

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

iterate :: Dupable a => (a #-> a) -> a #-> [a]
iterate f a = dup2 a & \(a', a'') ->
  a' : iterate f (f a'')

repeat :: Dupable a => a #-> [a]
repeat = iterate id

cycle :: (HasCallStack, Dupable a) => [a] #-> [a]
cycle [] = Prelude.error "cycle: empty list"
cycle xs = dup2 xs & \(xs', xs'') -> xs' ++ cycle xs''

scanl :: Dupable b => (b #-> a #-> b) -> b #-> [a] #-> [b]
scanl _ b [] = [b]
scanl f b (x:xs) = dup2 b & \(b', b'') -> b' : scanl f (f b'' x) xs

scanl1 :: Dupable a => (a #-> a #-> a) -> [a] #-> [a]
scanl1 _ [] = []
scanl1 f (x:xs) = scanl f x xs

scanr :: Dupable b => (a #-> b #-> b) -> b #-> [a] #-> [b]
scanr _ b [] =  [b]
scanr f b (a:as) =
  scanr f b as & \(b':bs') ->
    dup2 b' & \(b'', b''') ->
      f a b'' : b''' : bs'

scanr1 :: Dupable a => (a #-> a #-> a) -> [a] #-> [a]
scanr1 _ [] =  []
scanr1 _ [a] =  [a]
scanr1 f (a:as) =
  scanr1 f as & \(a':as') ->
    dup2 a' & \(a'', a''') ->
      f a a'' : a''' : as'

replicate :: Dupable a => Int -> a #-> [a]
replicate i a
  | i Prelude.< 1 = a `lseq` []
  | i Prelude.== 1 = [a]
  | otherwise  = dup2 a & \(a', a'') -> a' : replicate (i-1) a''

unfoldr :: (b #-> Maybe (a, b)) -> b #-> [a]
unfoldr f = Unsafe.toLinear (NonLinear.unfoldr (forget f))

-- # Zipping and unzipping lists
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

unzip :: [(a, b)] #-> ([a], [b])
unzip = Unsafe.toLinear NonLinear.unzip

unzip3 :: [(a, b, c)] #-> ([a], [b], [c])
unzip3 = Unsafe.toLinear NonLinear.unzip3
