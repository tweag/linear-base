{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.List.Linear
  ( -- * Basic functions
    map
  , filter
  , uncons
  , reverse
  , length
  , splitAt
  , span
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
import Prelude (Bool, Maybe(..), Either(..), Int)
import Data.Num.Linear
import Prelude.Linear.Internal.Simple
import Data.Unrestricted.Linear
import Data.Functor.Linear
import Data.List.NonEmpty (NonEmpty ((:|)))

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
reverse l = rev l []
  where
    rev :: [a] #-> [a] #-> [a]
    rev [] a = a
    rev (x:xs) a = rev xs (x:a)

-- | Return a new list with the same elements alongside with
-- its length.
length :: [a] #-> (Int, [a])
length [] = (0, [])
length (x:xs) = length xs & \case
  (l, xs') -> (l+1, x:xs')

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
zipWith _ [] ys = ys `lseq` []
zipWith _ xs [] = xs `lseq` []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

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

