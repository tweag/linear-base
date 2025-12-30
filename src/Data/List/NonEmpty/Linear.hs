{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Linear versions of 'NonEmpty' functions.
--
-- This module only contains minimal amount of documentation; consult the
-- original "Data.List.NonEmpty" module for more detailed information.
module Data.List.NonEmpty.Linear
  ( -- * Non-empty stream transformations
    NonEmpty (..),
    map,
    intersperse,
    scanl,
    scanr,
    scanl1,
    scanr1,
    transpose,
    NonLinear.sortBy,
    NonLinear.sortWith,

    -- * Basic functions
    length,
    NonLinear.head,
    NonLinear.tail,
    NonLinear.last,
    NonLinear.init,
    singleton,
    (<|),
    cons,
    uncons,
    unfoldr,
    NonLinear.sort,
    NonLinear.sortOn,
    reverse,
    append,
    appendList,
    prependList,

    -- * Extracting sublists
    take,
    drop,
    splitAt,
    takeWhile,
    dropWhile,
    span,
    break,
    filter,
    partition,

    -- * Zipping and unzipping streams
    zip,
    zipWith,
    zip',
    zipWith',
    unzip,
    unzip3,

    -- * Converting to and from a list
    fromList,
    toList,
    nonEmpty,
    xor,
  )
where

import qualified Data.List.Linear as List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonLinear
import Data.Vector.Internal.Check (HasCallStack)
import Prelude.Linear hiding (drop, dropWhile, filter, intersperse, length, map, partition, reverse, scanl, scanl1, scanr, scanr1, span, splitAt, take, takeWhile, transpose, uncons, unfoldr, unzip, unzip3, zip, zip', zipWith, zipWith')
import qualified Unsafe.Linear as Unsafe
import qualified Prelude as Prelude

map :: (a %1 -> b) -> NonEmpty a %1 -> NonEmpty b
map f (x :| xs) = f x :| List.map f xs

intersperse :: a -> NonEmpty a %1 -> NonEmpty a
intersperse a = Unsafe.toLinear (NonLinear.intersperse a)

reverse :: NonEmpty a %1 -> NonEmpty a
reverse = Unsafe.toLinear NonLinear.reverse

scanl :: (Dupable b) => (b %1 -> a %1 -> b) -> b %1 -> NonEmpty a %1 -> NonEmpty b
scanl f z = fromList . List.scanl f z . toList

scanr :: (Dupable b) => (a %1 -> b %1 -> b) -> b %1 -> NonEmpty a %1 -> NonEmpty b
scanr f z = fromList . List.scanr f z . toList

scanl1 :: (Dupable a) => (a %1 -> a %1 -> a) -> NonEmpty a %1 -> NonEmpty a
scanl1 f (x :| xs) = fromList $ List.scanl f x xs

scanr1 :: (Dupable a) => (a %1 -> a %1 -> a) -> NonEmpty a %1 -> NonEmpty a
scanr1 f (x :| xs) = fromList $ List.scanr1 f (x : xs)

transpose :: NonEmpty (NonEmpty a) %1 -> NonEmpty (NonEmpty a)
transpose = Unsafe.toLinear NonLinear.transpose

singleton :: a %1 -> NonEmpty a
singleton = (:| [])

infixr 5 <|

(<|) :: a %1 -> NonEmpty a %1 -> NonEmpty a
a <| bs = a :| toList bs

cons :: a %1 -> NonEmpty a %1 -> NonEmpty a
cons = (<|)

uncons :: NonEmpty a %1 -> (a, Maybe (NonEmpty a))
uncons (x :| xs) = (x, nonEmpty xs)

unfoldr :: (a %1 -> (b, Maybe a)) -> a %1 -> NonEmpty b
unfoldr f a = case f a of
  (b, mc) -> b :| maybe [] go mc
  where
    go c = case f c of
      (d, me) -> d : maybe [] go me

append :: NonEmpty a %1 -> NonEmpty a %1 -> NonEmpty a
append = (<>)

appendList :: NonEmpty a %1 -> [a] %1 -> NonEmpty a
appendList (x :| xs) ys = x :| (xs <> ys)

prependList :: [a] %1 -> NonEmpty a %1 -> NonEmpty a
prependList ls ne = case ls of
  [] -> ne
  (y : ys) -> y :| (ys <> toList ne)

-- | __NOTE__: This does not short-circuit and always traverses the
-- entire list to consume the rest of the elements.
take :: (Consumable a) => Int -> NonEmpty a %1 -> [a]
take n = List.take n . toList

drop :: (Consumable a) => Int -> NonEmpty a %1 -> [a]
drop n = List.drop n . toList

splitAt :: (Consumable a) => Int -> NonEmpty a %1 -> ([a], [a])
splitAt n = List.splitAt n . toList

-- | __NOTE__: This does not short-circuit and always traverses the
-- entire list to consume the rest of the elements.
takeWhile :: (Dupable a) => (a %1 -> Bool) -> NonEmpty a %1 -> [a]
takeWhile p = List.takeWhile p . toList

dropWhile :: (Dupable a) => (a %1 -> Bool) -> NonEmpty a %1 -> [a]
dropWhile p = List.dropWhile p . toList

span :: (Dupable a) => (a %1 -> Bool) -> NonEmpty a %1 -> ([a], [a])
span p = List.span p . toList

break :: (Dupable a) => (a %1 -> Bool) -> NonEmpty a %1 -> ([a], [a])
break p = span (not . p)

filter :: (Dupable a) => (a %1 -> Bool) -> NonEmpty a %1 -> [a]
filter p = List.filter p . toList

partition :: (Dupable a) => (a %1 -> Bool) -> NonEmpty a %1 -> ([a], [a])
partition p = List.partition p . toList

-- | Return the length of the given list alongside with the list itself.
length :: NonEmpty a %1 -> (Ur Int, NonEmpty a)
length = Unsafe.toLinear $ \xs ->
  (Ur (NonLinear.length xs), xs)

fromList :: (HasCallStack) => [a] %1 -> (NonEmpty a)
fromList (x : xs) = x :| xs
fromList [] = Prelude.error "NonEmpty.Linear.fromList: empty list"

toList :: NonEmpty a %1 -> [a]
toList (x :| xs) = x : xs

nonEmpty :: [a] %1 -> Maybe (NonEmpty a)
nonEmpty (x : xs) = Just (x :| xs)
nonEmpty [] = Nothing

xor :: NonEmpty Bool %1 -> Bool
xor = Unsafe.toLinear NonLinear.xor

zip :: (Consumable a, Consumable b) => NonEmpty a %1 -> NonEmpty b %1 -> NonEmpty (a, b)
zip = zipWith (,)

zipWith :: (Consumable a, Consumable b) => (a %1 -> b %1 -> c) -> NonEmpty a %1 -> NonEmpty b %1 -> NonEmpty c
zipWith f (a :| as) (b :| bs) = f a b :| List.zipWith f as bs

-- | Same as 'zipWith', but returns the leftovers instead of consuming them.
-- Because the leftovers are returned at toplevel, @zipWith'@ is pretty strict:
-- forcing the second cons cell of the returned list forces all the recursive
-- calls.
zipWith' :: (a %1 -> b %1 -> c) -> NonEmpty a %1 -> NonEmpty b %1 -> (NonEmpty c, Maybe (Either (NonEmpty a) (NonEmpty b)))
zipWith' f (a :| as) (b :| bs) =
  case List.zipWith' f as bs of
    (cs, may) -> (f a b :| cs, may)

-- | Same as 'zip', but returns the leftovers instead of consuming them.
zip' :: NonEmpty a %1 -> NonEmpty b %1 -> (NonEmpty (a, b), Maybe (Either (NonEmpty a) (NonEmpty b)))
zip' = zipWith' (,)

unzip :: NonEmpty (a, b) %1 -> (NonEmpty a, NonEmpty b)
unzip ((a, b) :| asbs) =
  List.unzip asbs & \(as, bs) ->
    (a :| as, b :| bs)

unzip3 :: NonEmpty (a, b, c) %1 -> (NonEmpty a, NonEmpty b, NonEmpty c)
unzip3 ((a, b, c) :| abs) =
  List.unzip3 abs & \(as, bs, cs) ->
    (a :| as, b :| bs, c :| cs)
