{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Foldable.Linear
  ( Foldable (..)
  , all
  , and
  , or
  , traverse_
  , sequence_
  -- * Helpers for creating instances
  , foldMapDefault
  , ConsumableViaFoldable (..)
  ) where

import qualified Prelude as Prelude
import Prelude (Maybe(..), Either(..), Int, Ordering (..))
import Prelude.Linear.Internal.Simple
import Data.Unrestricted.Linear
import Data.Eq.Linear
import Data.Ord.Linear
import Data.Bool.Linear
import Data.Num.Linear
import Data.Maybe.Linear
import Data.List.NonEmpty (NonEmpty (..))
import Data.Functor.Linear
import Data.Monoid.Linear

class Foldable t where
    {-# MINIMAL foldMap | foldr #-}

    fold :: Monoid m => t m #-> m
    fold = foldMap id

    foldMap' :: Monoid m => (a #-> m) -> t a #-> m
    foldMap' f = foldl' (\acc a -> acc <> f a) mempty

    foldMap :: Monoid m => (a #-> m) -> t a #-> m
    {-# INLINE foldMap #-}
    foldMap f = foldr ((<>) . f) mempty

    foldr :: (a #-> b #-> b) -> b #-> t a #-> b
    foldr f z t = appEndo (foldMap (Endo . f) t) z

    foldr' :: forall a b. (a #-> b #-> b) -> b #-> t a -> b
    foldr' f z0 xs = foldl f' id xs z0
      where
        f' :: (b #-> b) #-> a #-> b #-> b
        f' k x z = k $! f x z

    foldl :: (b #-> a #-> b) -> b #-> t a #-> b
    foldl f z t = appEndo (getDual' (foldMap (Dual . Endo . flip f) t)) z
      where
        getDual' :: Dual a #-> a
        getDual' (Dual a) = a

    foldl' :: forall a b. (b #-> a #-> b) -> b #-> t a #-> b
    foldl' f z0 xs = foldr f' id xs z0
       where
         f' :: a #-> (b #-> b) #-> b #-> b
         f' x k z = k $! f z x

    foldr1 :: forall a. (a #-> a #-> a) -> t a #-> a
    foldr1 f xs = fromMaybe (Prelude.errorWithoutStackTrace "foldr1: empty structure")
                    (foldr mf Nothing xs)
      where
        mf :: a #-> Maybe a #-> Maybe a
        mf x Nothing = Just x
        mf x (Just y) = Just (f x y)

    foldl1 :: forall a. (a #-> a #-> a) -> t a #-> a
    foldl1 f xs = fromMaybe (Prelude.errorWithoutStackTrace "foldl1: empty structure")
                    (foldl mf Nothing xs)
      where
        mf :: Maybe a #-> a #-> Maybe a
        mf Nothing y = Just y
        mf (Just x) y = Just (f x y)

    toList :: t a #-> [a]
    toList = foldr (:) []

    -- | This is likely not very useful and only here to preserve the symmetry
    -- between 'Prelude.Foldable'.
    null :: Consumable a => t a #-> Bool
    null = foldr (\x a -> (x, a) `lseq` False) True

    length :: Consumable a => t a #-> Int
    length = foldl' (\c a -> a `lseq` c + 1) 0

    -- | This is likely not very useful and only here to preserve the symmetry
    -- between 'Prelude.Foldable'.
    elem :: Prelude.Eq a => a -> t a #-> Bool
    elem x = any (== x)

    minimum :: (Dupable a, Prelude.Ord a) => t a #-> a
    minimum = minimumBy compare

    maximum :: (Dupable a, Prelude.Ord a) => t a #-> a
    maximum = maximumBy compare

-- | A wrapper which implements a 'Consumable' instance given a 'Foldable'.
-- Suitable to use with @-XDerivingVia@.
newtype ConsumableViaFoldable a = ConsumableViaFoldable a
instance (Foldable t, Consumable a) => Consumable (ConsumableViaFoldable (t a)) where
  consume (ConsumableViaFoldable xs) = foldl' (flip lseq) () xs

instance Foldable [] where
  foldr f z = \case
    [] -> z
    x:xs -> f x (foldr f z xs)

instance Foldable Maybe where
  foldMap _ Nothing = mempty
  foldMap f (Just s) = f s

instance Foldable NonEmpty where
  foldMap f (x:|xs) = f x <> foldMap f xs

instance Consumable a => Foldable (Either a) where
  foldMap _ (Left l) = l `lseq` mempty
  foldMap f (Right r) = f r

maximumBy :: (Foldable t, Dupable a) => (a #-> a #-> Ordering) -> t a #-> a
maximumBy cmp = foldl1 (\x y ->
  dup (x, y) & \case
    ((x1, y1), (x2, y2)) -> cmp x1 y1 & \case
      GT -> y2 `lseq` x2
      EQ -> x2 `lseq` y2
      LT -> x2 `lseq` y2
  )

minimumBy :: (Foldable t, Dupable a) => (a #-> a #-> Ordering) -> t a #-> a
minimumBy cmp = maximumBy (\x y -> cmp x y & \case
    GT -> LT
    EQ -> EQ
    LT -> GT
  )

-- | __NOTE:__ This does not short-circuit, and always consumes the
-- entire container.
any :: Foldable t => (a #-> Bool) -> t a #-> Bool
any p = foldl' (\b a -> b || p a) False

-- | __NOTE:__ This does not short-circuit, and always consumes the
-- entire container.
all :: Foldable t => (a #-> Bool) -> t a #-> Bool
all p = foldl' (\b a -> b && p a) True

-- | __NOTE:__ This does not short-circuit, and always consumes the
-- entire container.
and :: Foldable t => t Bool #-> Bool
and = foldl' (&&) True

-- | __NOTE:__ This does not short-circuit, and always consumes the
-- entire container.
or :: Foldable t => t Bool #-> Bool
or = foldl' (||) False

traverse_ :: (Foldable t, Applicative f, Consumable b)
          => (a #-> f b) -> t a #-> f ()
traverse_ f = foldr (\x k -> f x *> k) (pure ())

sequence_ :: (Foldable t, Applicative f, Consumable a)
          => t (f a) #-> f ()
sequence_ = traverse_ id


-- | This function can be used as a value for 'foldMap' given a 'Traversable'
-- and a 'Consumable' instance.
--
-- Note: Unlike their base counterparts, 'Data.Foldable.Linear' is not a
-- superclass of 'Data.Traversable.Linear'; since there are 'Traversable's
-- which are not 'Foldable', for example `(a,)` or `Const a`. This type
-- signature also witnesses that by requring an extra `Consumable (t ())`
-- constraint.
foldMapDefault :: (Traversable t, Monoid m, Consumable (t ())) => (a #-> m) -> t a #-> m
foldMapDefault f xs = sequence ((\a -> (f a, ())) <$> xs) & \case
  (m, t) -> t `lseq` m
