{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Control.Optics.Linear.Internal where

import qualified Data.Profunctor.Linear as Monoidal
import Data.Profunctor.Linear hiding (swap)
import Data.Void
import Prelude.Linear
import qualified Control.Monad.Linear as Control
import qualified Data.Functor.Linear as Data
import qualified Prelude as P

type Optic_ arr a b s t = a `arr` b -> s `arr` t

type Optic c a b s t =
  forall arr. c arr => Optic_ arr a b s t

type Iso a b s t = Optic Profunctor a b s t
type Iso' a s = Iso a a s s
type Lens a b s t = Optic (Strong (,) ()) a b s t
type Lens' a s = Lens a a s s
type WithLens a b s t = Optic (Strong (&) Top) a b s t
type WithLens' a s = WithLens a a s s
type Prism a b s t = Optic (Strong Either Void) a b s t
type Prism' a s = Prism a a s s

-- | Because it is inconvenient to pair up parametric constraints, 'Traversal'
-- is defined in terms of 'Optic_' rather than 'Optic'.
type Traversal a b s t =
  forall arr.
  ( Strong (,) () arr , Strong Either Void arr , Monoidal (,) () arr)
  => Optic_ arr a b s t
type Traversal' a s = Traversal a a s s

-- TODO: I'm sure most of these type can be monomorphic optics into polymorphic
-- ones.
swap :: SymmetricMonoidal m u => Iso' (a `m` b) (b `m` a)
swap = dimap Monoidal.swap Monoidal.swap

_1 :: Lens a b (a,c) (b,c)
_1 = first

_2 :: Lens a b (c,a) (c,b)
_2 = second

_WithL :: WithLens a b (a & c) (b & c)
_WithL = first

_Left :: Prism a b (Either a c) (Either b c)
_Left = first

_Right :: Prism a b (Either c a) (Either c b)
_Right = second

-- eitherMaybe :: Iso (Either () a) (Either () b) (Maybe a) (Maybe b)
-- eitherMaybe = dimap (maybe (Left ()) Right) (either (\() -> Nothing) Just)

-- the :: Prism a b (Maybe a) (Maybe b)
-- the = eitherMaybe P.. _Right

flipIso :: Iso a b s t -> Iso t s b a
flipIso i = dimap (to i) (from i)

from :: Iso a b s t -> s ->. a
from = get'

to :: Iso a b s t -> b ->. t
to = unknown

unknown :: Prism a b s t -> b ->. t
unknown p = getFrom (p (From id))

newtype From x a b = From { getFrom :: x ->. b }
instance Profunctor (From x) where
  dimap _ g (From h) = From (g . h)
instance Strong Either Void (From x) where
  first (From f) = From $ Left . f

over :: Lens a b s t -> (a ->. b) -> s ->. t
over l f = getArrow (l (LA f))

get :: Lens a b s t -> s -> a
get l = getTo (l (To P.id))

get' :: WithLens a b s t -> s ->. a
get' l = getTo' (l (To' id))

over' :: WithLens a b s t -> (a ->. b) -> s ->. t
over' l f = getArrow (l (LA f))

-- over'' :: Optic (Strong Either Void) a b s t -> (a ->. b) -> (s ->. t)
-- over'' l f = getArrow (l (LA f))

set'' :: WithLens a b s t -> s & b ->. t
set'' l = getTing (l (Ting sndWith))

newtype Ting b s t = Ting { getTing :: s & b ->. t }
instance Strong (&) Top (Ting b) where
  first (Ting k) = Ting (\(With s b x) -> With (k . With (fstWith . s) b) (sndWith . s) x)
instance Profunctor (Ting b) where
  dimap f g (Ting k) = Ting (\(With s b x) -> g (k (With (f . s) b x)))

set :: Lens a b s t -> s -> b ->. t
set l = getExtra (l (Extra (const id)))

set' :: Consumable a => Lens a b s t -> s ->. b ->. t
set' l = getExtra' (l (Extra' lseq))

set3 :: Consumable a => Lens a b s t -> s ->. b -> t
set3 l = getIdea (l (Idea (\x y -> x `lseq` y)))

set3' :: Lens a b s t -> s ->. b ->. (a, t)
set3' l = getRemember (l (Remember (,)))

-- get3 :: Lens a b s t -> s ->. a
-- get3 l = getTo' (l (To' id))

newtype Idea x a b = Idea { getIdea :: a ->. x -> b }
instance Profunctor (Idea a) where
  dimap f g (Idea h) = Idea $ \s a -> g (h (f s) a)
instance Strong (,) () (Idea a) where
  first (Idea f) = Idea $ \(a1, c) a -> (f a1 a, c)

newtype Remembering x y z w = Remember { getRemember :: z ->. x ->. (y, w) }
instance Profunctor (Remembering x y) where
  dimap f g (Remember h) = Remember (\s x -> run g (h (f s) x))
    where run :: (b ->. t) -> (y,b) ->. (y,t)
          run k (y,b) = (y,k b)
instance Strong (,) () (Remembering x y) where
  first (Remember f) = Remember (\(a,c) x -> run (f a x) c)
    where run :: (y,b) ->. c ->. (y,(b,c))
          run (y,b) c = (y,(b,c))

newtype LinearArrow a b = LA { getArrow :: a ->. b }

instance Profunctor LinearArrow where
  dimap f g (LA h) = LA (g . h . f)

instance Strong (,) () LinearArrow where
  first (LA f) = LA (\(a, c) -> (f a, c))
  second (LA f) = LA (\(a, b) -> (a, f b))

-- instance Strong Either Void LinearArrow where
--   first (LA f) = LA (either (Left . f) Right)
--   second (LA f) = LA (either Left (Right . f))

instance Strong (&) Top LinearArrow where
  first (LA f) = LA $ With (f . fstWith) sndWith

-- instance Profunctor (->) where
--   dimap f g h x = g (h (f x))
--
-- instance Strong (,) () (->) where
--   first f = (\(a, c) -> (f a, c))
--   second f = (\(a, b) -> (a, f b))
--
-- instance Strong Either Void (->) where
--   first f = P.either (Left P.. f) Right
--   second f = P.either Left (Right P.. f)

data a & b where
  With :: (x ->. a) -> (x ->. b) -> x ->. a & b
fstWith :: a & b ->. a
fstWith (With f _ x) = f x
sndWith :: a & b ->. b
sndWith (With _ g x) = g x
data Top where
  Top :: x ->. Top

instance SymmetricMonoidal (&) Top where
  swap (With f g x) = With g f x

newtype To x a b = To { getTo :: a -> x }
instance Profunctor (To x) where
  dimap f _ (To h) = To (\s -> h (f s))
instance Strong (,) () (To x) where
  first (To f) = To (f P.. fst)

newtype To' x a b = To' { getTo' :: a ->. x }
instance Profunctor (To' x) where
  dimap f _ (To' h) = To' (\s -> h (f s))
instance Strong (&) Top (To' x) where
  first (To' f) = To' (f . fstWith)

newtype Extra x a b = Extra { getExtra :: a -> x ->. b }
instance Profunctor (Extra x) where
  dimap f g (Extra h) = Extra $ \s x -> g (h (f s) x)
instance Strong (,) () (Extra x) where
  first (Extra f) = Extra $ \(a,c) x -> (f a x, c)
instance Consumable x => Strong Either Void (Extra x) where
  first (Extra f) = Extra $ P.either (\a -> Left . f a) (\c x -> x `lseq` Right c)

newtype Extra' x a b = Extra' { getExtra' :: a ->. x ->. b }
instance Profunctor (Extra' x) where
  dimap f g (Extra' h) = Extra' $ \s x -> g (h (f s) x)
instance Strong (,) () (Extra' x) where
  first (Extra' f) = Extra' $ \(a,c) x -> (f a x, c)


newtype ProApp f a b = PA { getPA :: a ->. f b }
instance Data.Functor f => Profunctor (ProApp f) where
  dimap f g (PA h) = PA $ \s -> Data.fmap g (h (f s))
instance Control.Functor f => Strong (,) () (ProApp f) where
  first (PA f) = PA $ \(a,c) -> Control.fmap (,c) (f a)
-- instance Control.Applicative f => Strong Either Void (ProApp f) where
--   first (PA f) = PA $ either (Data.fmap Left . f) (Control.pure . Right)
instance Control.Applicative f => Monoidal (,) () (ProApp f) where
  PA f *** PA g = PA $ \(a,x) -> (,) Data.<$> f a Data.<*> g x
  unit = PA Control.pure
  -- XXX: make these all Control

-- traversed :: Traversable t => Traversal a b (t a) (t b)
-- traversed p = _
