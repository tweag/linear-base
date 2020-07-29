{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.List.Linear
  where

import qualified Unsafe.Linear as Unsafe
import qualified Prelude as Prelude
import Prelude (Bool, Maybe(..), Either(..), Int)
import Data.Num.Linear
import Prelude.Linear.Internal.Simple
import Data.Unrestricted.Linear
import Data.Functor.Linear
import Data.List.NonEmpty (NonEmpty ((:|)))

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

zip :: forall a b c. (Consumable a, Consumable b) => [a] #-> [b] #-> [(a, b)]
zip = zipWith (,)

-- | Same as 'zip', but returns the leftovers instead of consuming them.
zip' :: forall a b c. [a] #-> [b] #-> ([(a, b)], Maybe (Either (NonEmpty a) (NonEmpty b)))
zip' = zipWith' (,)

zip3 :: forall a b c. (Consumable a, Consumable b, Consumable c) => [a] #-> [b] #-> [c] #-> [(a, b, c)]
zip3 = zipWith3 (,,)

zipWith :: forall a b c. (Consumable a, Consumable b) => (a#->b#->c) -> [a] #-> [b] #-> [c]
zipWith f [] ys = ys `lseq` []
zipWith f xs [] = xs `lseq` []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- | Same as 'zipWith', but returns the leftovers instead of consuming them.
zipWith' :: (a#->b#->c) -> [a] #-> [b] #-> ([c], Maybe (Either (NonEmpty a) (NonEmpty b)))
zipWith' f [] [] = ([], Nothing)
zipWith' f (a:as) [] = ([], Just (Left (a :| as)))
zipWith' f [] (b:bs) = ([], Just (Right (b :| bs)))
zipWith' f (a:as) (b:bs) = zipWith' f as bs & \case
  (cs, rest) -> (f a b : cs, rest)

zipWith3 :: forall a b c d. (Consumable a, Consumable b, Consumable c) => (a#->b#->c#->d) -> [a] #-> [b] #-> [c] #-> [d]
zipWith3 f [] ys zs = (ys, zs) `lseq` []
zipWith3 f xs [] zs = (xs, zs) `lseq` []
zipWith3 f xs ys [] = (xs, ys) `lseq` []
zipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 f xs ys zs

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

splitAt :: Int -> [a] #-> ([a], [a])
splitAt i = Unsafe.toLinear (Prelude.splitAt i)
