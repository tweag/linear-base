{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Control.Optics.Linear.Internal
  ( -- * Types
    Optic_(..)
  , Optic
  , Iso, Iso'
  , Lens, Lens'
  , Prism, Prism'
  , PTraversal, PTraversal'
  , DTraversal, DTraversal'
    -- * Composing optics
  , (.>)
    -- * Common optics
  , swap, assoc
  , _1, _2
  , _Left, _Right
  , _Just, _Nothing
  , ptraversed, dtraversed, btraversed
  , both, both', both''
    -- * Using optics
  , get, set, gets
  , get', gets'
  , set', set''
  , match, build
  , preview
  , over, over'
  , traverseOf, traverseOf'
  , lengthOf
  , withIso, withLens, withPrism
  , toListOf
    -- * Constructing optics
  , iso, prism, lens
  )
  where

import qualified Control.Arrow as NonLinear
import qualified Control.Monad.Linear as Control
import qualified Data.Bifunctor.Linear as Bifunctor
import Data.Functor.Linear.Internal.Traversable
import Data.Bifunctor.Linear (SymmetricMonoidal)
import Data.Functor.Const
import Data.Functor.Linear
import Data.Monoid.Linear
import qualified Data.Profunctor.Kleisli.Linear as Linear
import Data.Profunctor.Linear
import Data.Void
import Prelude.Linear
import qualified Prelude as P

-- TODO: documentation in this module
-- Put the functions in some sensible order: possibly split into separate
-- Lens/Prism/Traversal/Iso modules
newtype Optic_ arr a b s t = Optical (a `arr` b -> s `arr` t)

type Optic c a b s t =
  forall arr. c arr => Optic_ arr a b s t

type Iso a b s t = Optic Profunctor a b s t
type Iso' a s = Iso a a s s
type Lens a b s t = Optic (Strong (,) ()) a b s t
type Lens' a s = Lens a a s s
type Prism a b s t = Optic (Strong Either Void) a b s t
type Prism' a s = Prism a a s s
type PTraversal a b s t = Optic PWandering a b s t
type PTraversal' a s = PTraversal a a s s
type DTraversal a b s t = Optic DWandering a b s t
type DTraversal' a s = DTraversal a a s s
-- XXX: these will unify into
-- type Traversal (p :: Multiplicity) a b s t = Optic (Wandering p) a b s t

type BTraversal a b s t = Optic Traversing a b s t

swap :: SymmetricMonoidal m u => Iso (a `m` b) (c `m` d) (b `m` a) (d `m` c)
swap = iso Bifunctor.swap Bifunctor.swap

assoc :: SymmetricMonoidal m u => Iso ((a `m` b) `m` c) ((d `m` e) `m` f) (a `m` (b `m` c)) (d `m` (e `m` f))
assoc = iso Bifunctor.lassoc Bifunctor.rassoc

(.>) :: Optic_ arr a b s t -> Optic_ arr x y a b -> Optic_ arr x y s t
Optical f .> Optical g = Optical (f P.. g)

lens :: (s ->. (a, b ->. t)) -> Lens a b s t
lens k = Optical $ \f -> dimap k (\(x,g) -> g $ x) (first f)

withLens :: Optic_ (Linear.Kleisli (OtherFunctor a b)) a b s t -> s ->. (a, b ->. t)
withLens (Optical l) s = runOtherFunctor (Linear.runKleisli (l (Linear.Kleisli (\a -> OtherFunctor (a, id)))) s)

prism :: (b ->. t) -> (s ->. Either t a) -> Prism a b s t
prism b s = Optical $ \f -> dimap s (either id id) (second (rmap b f))

_1 :: Lens a b (a,c) (b,c)
_1 = Optical first

_2 :: Lens a b (c,a) (c,b)
_2 = Optical second

-- XXX: these will unify to
-- > both :: forall (p :: Multiplicity). Traversal p a b (a,a) (b,b)
both' :: PTraversal a b (a,a) (b,b)
both' = _Pairing .> ptraversed

both :: DTraversal a b (a,a) (b,b)
both = _Pairing .> dtraversed

both'' :: BTraversal a b (a,a) (b,b)
both'' = _Pairing .> btraversed

-- XXX: these are a special case of Bitraversable, but just the simple case
-- is included here for now
_Pairing :: Iso (Pair a) (Pair b) (a,a) (b,b)
_Pairing = iso Paired unpair

newtype Pair a = Paired (a,a)
unpair :: Pair a ->. (a,a)
unpair (Paired x) = x

instance P.Functor Pair where
  fmap f (Paired (x,y)) = Paired (f x, f y)
instance Functor Pair where
  fmap f (Paired (x,y)) = Paired (f x, f y)
instance Foldable Pair where
  foldMap f (Paired (x,y)) = f x P.<> f y
instance P.Traversable Pair where
  traverse f (Paired (x,y)) = Paired P.<$> ((,) P.<$> f x P.<*> f y)
instance Traversable Pair where
  traverse f (Paired (x,y)) = Paired <$> ((,) <$> f x <*> f y)

toListOf :: Optic_ (NonLinear.Kleisli (Const [a])) a b s t -> s -> [a]
toListOf l = gets l (\a -> [a])

_Left :: Prism a b (Either a c) (Either b c)
_Left = Optical first

_Right :: Prism a b (Either c a) (Either c b)
_Right = Optical second

_Just :: Prism a b (Maybe a) (Maybe b)
_Just = prism Just (maybe (Left Nothing) Right)

_Nothing :: Prism' () (Maybe a)
_Nothing = prism (\() -> Nothing) Left

ptraversed :: P.Traversable t => PTraversal a b (t a) (t b)
ptraversed = Optical pwander

dtraversed :: Traversable t => DTraversal a b (t a) (t b)
dtraversed = Optical dwander

over :: Optic_ LinearArrow a b s t -> (a ->. b) -> s ->. t
over (Optical l) f = getLA (l (LA f))

traverseOf :: Optic_ (Linear.Kleisli f) a b s t -> (a ->. f b) -> s ->. f t
traverseOf (Optical l) f = Linear.runKleisli (l (Linear.Kleisli f))

get :: Optic_ (NonLinear.Kleisli (Const a)) a b s t -> s -> a
get l = gets l P.id

gets :: Optic_ (NonLinear.Kleisli (Const r)) a b s t -> (a -> r) -> s -> r
gets (Optical l) f s = getConst' (NonLinear.runKleisli (l (NonLinear.Kleisli (Const P.. f))) s)

preview :: Optic_ (NonLinear.Kleisli (Const (Maybe (First a)))) a b s t -> s -> Maybe a
preview l s = P.fmap getFirst (gets l (\a -> Just (First a)) s)

get' :: Optic_ (Linear.Kleisli (Const (Top, a))) a b s t -> s ->. (Top, a)
get' l = gets' l id

gets' :: Optic_ (Linear.Kleisli (Const (Top, r))) a b s t -> (a ->. r) -> s ->. (Top, r)
gets' (Optical l) f s = getConst' (Linear.runKleisli (l (Linear.Kleisli (\a -> Const (mempty, f a)))) s)

set' :: Optic_ (Linear.Kleisli (MyFunctor a b)) a b s t -> s ->. b ->. (a, t)
set' (Optical l) s = runMyFunctor (Linear.runKleisli (l (Linear.Kleisli (\a -> MyFunctor (\b -> (a,b))))) s)

set'' :: Optic_ (NonLinear.Kleisli (Control.Reader b)) a b s t -> b ->. s -> t
set'' (Optical l) b s = Control.runReader (NonLinear.runKleisli (l (NonLinear.Kleisli (const (Control.reader id)))) s) b

set :: Optic_ (->) a b s t -> b -> s -> t
set (Optical l) x = l (const x)

match :: Optic_ (Market a b) a b s t -> s ->. Either t a
match (Optical l) = snd (runMarket (l (Market id Right)))

build :: Optic_ (Linear.CoKleisli (Const b)) a b s t -> b ->. t
build (Optical l) x = Linear.runCoKleisli (l (Linear.CoKleisli getConst')) (Const x)

-- XXX: move this to Prelude
-- | Linearly typed patch for the newtype deconstructor. (Temporary until
-- inference will get this from the newtype declaration.)
getConst' :: Const a b ->. a
getConst' (Const x) = x

lengthOf :: MultIdentity r => Optic_ (NonLinear.Kleisli (Const (Adding r))) a b s t -> s -> r
lengthOf l s = getAdded (gets l (const (Adding one)) s)

-- XXX: the below two functions will be made redundant with multiplicity
-- polymorphism on over and traverseOf'
over' :: Optic_ (->) a b s t -> (a -> b) -> s -> t
over' (Optical l) f = l f

traverseOf' :: Optic_ (NonLinear.Kleisli f) a b s t -> (a -> f b) -> s -> f t
traverseOf' (Optical l) f = NonLinear.runKleisli (l (NonLinear.Kleisli f))

iso :: (s ->. a) -> (b ->. t) -> Iso a b s t
iso f g = Optical (dimap f g)

withIso :: Optic_ (Exchange a b) a b s t -> ((s ->. a) -> (b ->. t) -> r) -> r
withIso (Optical l) f = f fro to
  where Exchange fro to = l (Exchange id id)

withPrism :: Optic_ (Market a b) a b s t -> ((b ->. t) -> (s ->. Either t a) -> r) -> r
withPrism (Optical l) f = f b m
  where Market b m = l (Market id Right)

traversal :: (s ->. Batch a b t) -> BTraversal a b s t
traversal h = Optical (\k -> dimap h fuse (traverse' k))

traverse' :: (Strong Either Void arr, Monoidal (,) () arr) => a `arr` b -> Batch a c t `arr` Batch b c t
traverse' k = dimap out inn (second (k *** (traverse' k)))

out :: Batch a b t ->. Either t (a, Batch a b (b ->. t))
out (P t) = Left t
out (x :*: l) = Right (l,x)

inn :: Either t (a, Batch a b (b ->. t)) ->. Batch a b t
inn (Left t) = P t
inn (Right (l,x)) = x :*: l

thing :: Traversable t => t a ->. Batch a b (t b)
thing = traverse batch

btraversed :: Traversable t => BTraversal a b (t a) (t b)
btraversed = traversal thing
