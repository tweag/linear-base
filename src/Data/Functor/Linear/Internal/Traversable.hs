{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Data.Functor.Linear.Internal.Traversable
  ( -- * Linear traversable hierarchy
    -- $
    Traversable (..),
    genericTraverse,
    GTraversable,
    mapM,
    sequenceA,
    for,
    forM,
    mapAccumL,
    mapAccumR,
  )
where

import qualified Control.Functor.Linear.Internal.Class as Control
import qualified Control.Functor.Linear.Internal.Instances as Control
import Control.Functor.Linear.Internal.Kan
import qualified Control.Functor.Linear.Internal.State as Control
import Data.Functor.Const
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import GHC.Types (Multiplicity (..))
import Generics.Linear
import Prelude.Linear.Internal
import Prelude (Either (..), Maybe (..))

-- traversable

-- TODO: write the laws
-- TODO: maybe add a Foldable class between Functor and Traversable as well

-- | A linear data traversible is a functor of type @t a@ where you can apply a
-- linear effectful action of type @a %1-> f b@ on each value of type @a@ and
-- compose this to perform an action on the whole functor, resulting in a value
-- of type @f (t b)@.
--
-- To learn more about 'Traversable', see here:
--
--  * \"Applicative Programming with Effects\",
--    by Conor McBride and Ross Paterson,
--    /Journal of Functional Programming/ 18:1 (2008) 1-13, online at
--    <http://www.soi.city.ac.uk/~ross/papers/Applicative.html>.
--
--  * \"The Essence of the Iterator Pattern\",
--    by Jeremy Gibbons and Bruno Oliveira,
--    in /Mathematically-Structured Functional Programming/, 2006, online at
--    <http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/#iterator>.
--
--  * \"An Investigation of the Laws of Traversals\",
--    by Mauro Jaskelioff and Ondrej Rypacek,
--    in /Mathematically-Structured Functional Programming/, 2012, online at
--    <http://arxiv.org/pdf/1202.2919>.
class Data.Functor t => Traversable t where
  {-# MINIMAL traverse | sequence #-}

  traverse :: Control.Applicative f => (a %1 -> f b) -> t a %1 -> f (t b)
  {-# INLINE traverse #-}
  traverse f x = sequence (Data.fmap f x)

  sequence :: Control.Applicative f => t (f a) %1 -> f (t a)
  {-# INLINE sequence #-}
  sequence = traverse id

mapM :: (Traversable t, Control.Monad m) => (a %1 -> m b) -> t a %1 -> m (t b)
mapM = traverse
{-# INLINE mapM #-}

sequenceA :: (Traversable t, Control.Applicative f) => t (f a) %1 -> f (t a)
sequenceA = sequence
{-# INLINE sequenceA #-}

for :: (Traversable t, Control.Applicative f) => t a %1 -> (a %1 -> f b) -> f (t b)
for t f = traverse f t
{-# INLINE for #-}

forM :: (Traversable t, Control.Monad m) => t a %1 -> (a %1 -> m b) -> m (t b)
forM = for
{-# INLINE forM #-}

mapAccumL :: Traversable t => (a %1 -> b %1 -> (a, c)) -> a %1 -> t b %1 -> (a, t c)
mapAccumL f s t = swap $ Control.runState (traverse (\b -> Control.state $ \i -> swap $ f i b) t) s

mapAccumR :: Traversable t => (a %1 -> b %1 -> (a, c)) -> a %1 -> t b %1 -> (a, t c)
mapAccumR f s t = swap $ runStateR (traverse (\b -> StateR $ \i -> swap $ f i b) t) s

swap :: (a, b) %1 -> (b, a)
swap (x, y) = (y, x)

-- | A right-to-left state transformer
newtype StateR s a = StateR (s %1 -> (a, s))
  deriving (Data.Functor, Data.Applicative) via Control.Data (StateR s)

runStateR :: StateR s a %1 -> s %1 -> (a, s)
runStateR (StateR f) = f

instance Control.Functor (StateR s) where
  fmap f (StateR x) = StateR $ (\(a, s') -> (f a, s')) . x

instance Control.Applicative (StateR s) where
  pure x = StateR $ \s -> (x, s)
  StateR f <*> StateR x = StateR (go . Control.fmap f . x)
    where
      go :: (a, (a %1 -> b, s)) %1 -> (b, s)
      go (a, (h, s'')) = (h a, s'')

------------------------
-- Generic derived instances --
------------------------

instance Traversable [] where
  -- We define traverse explicitly both to allow specialization
  -- to the appropriate Applicative and to allow specialization to
  -- the passed function. The generic definition allows neither, sadly.
  traverse f = go
    where
      go [] = Control.pure []
      go (x : xs) = Control.liftA2 (:) (f x) (go xs)

instance Traversable ((,) a) where
  traverse = genericTraverse

instance Traversable ((,,) a b) where
  traverse = genericTraverse

instance Traversable ((,,,) a b c) where
  traverse = genericTraverse

instance Traversable ((,,,,) a b c d) where
  traverse = genericTraverse

instance Traversable Maybe where
  traverse = genericTraverse

instance Traversable (Const a) where
  traverse = genericTraverse

instance Traversable (Either a) where
  traverse = genericTraverse

instance Traversable U1 where
  traverse = genericTraverse

instance Traversable V1 where
  traverse = genericTraverse

instance (Traversable f, Traversable g) => Traversable (f :*: g) where
  traverse = genericTraverse

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  traverse = genericTraverse

instance Traversable f => Traversable (M1 i c f) where
  traverse = genericTraverse

instance Traversable Par1 where
  traverse = genericTraverse

instance (Traversable f, Traversable g) => Traversable (f :.: g) where
  traverse = genericTraverse

instance Traversable (K1 i v) where
  traverse = genericTraverse

instance Traversable UAddr where
  traverse = genericTraverse

instance Traversable UChar where
  traverse = genericTraverse

instance Traversable UDouble where
  traverse = genericTraverse

instance Traversable UFloat where
  traverse = genericTraverse

instance Traversable UInt where
  traverse = genericTraverse

instance Traversable UWord where
  traverse = genericTraverse

-- | This type class derives the definition of 'genericTraverse' by induction on
-- the generic representation of a type.
class GTraversable t where
  -- gtraverse :: Applicative f => (a %1 -> f b) -> t a %1 -> forall r. (forall k. ((a %1 -> r) %1 -> k) %1 -> f k) %1 -> forall k. (t b %1 -> k) %1 -> f k
  --
  -- TODO: developer documentation on why we use this type rather than the more
  -- straightforward type of `traverse`. Used, for instance, in the
  -- generic-deriving package.
  gtraverse :: Control.Applicative f => (a %1 -> f b) -> t a %1 -> Curried (Yoneda f) (Yoneda f) (t b)

instance GTraversable t => GTraversable (M1 i c t) where
  gtraverse f (M1 x) = M1 Control.<$> gtraverse f x
  {-# INLINE gtraverse #-}

-- Can m be polymorphic? I'm not optimistic.
instance (m ~ 'One, GTraversable t) => GTraversable (MP1 m t) where
  gtraverse f (MP1 x) = Control.fmap MP1 (gtraverse f x)
  {-# INLINE gtraverse #-}

instance GTraversable Par1 where
  gtraverse f (Par1 x) = Par1 Control.<$> liftCurriedYonedaC (f x)
  {-# INLINE gtraverse #-}

instance (GTraversable f, Traversable g) => GTraversable (f :.: g) where
  gtraverse f (Comp1 x) = Comp1 Control.<$> gtraverse (traverse f) x
  {-# INLINE gtraverse #-}

instance (GTraversable f, GTraversable g) => GTraversable (f :+: g) where
  gtraverse f (L1 x) = L1 Control.<$> gtraverse f x
  gtraverse f (R1 x) = R1 Control.<$> gtraverse f x
  {-# INLINE gtraverse #-}

instance (GTraversable f, GTraversable g) => GTraversable (f :*: g) where
  gtraverse f (x :*: y) = Control.liftA2 (:*:) (gtraverse f x) (gtraverse f y)
  {-# INLINE gtraverse #-}

instance GTraversable (K1 i c) where
  gtraverse _ (K1 c) = Control.pure (K1 c)
  {-# INLINE gtraverse #-}

instance GTraversable U1 where
  gtraverse _ U1 = Control.pure U1
  {-# INLINE gtraverse #-}

instance GTraversable V1 where
  gtraverse _ v = Control.pure ((\case {}) v)

instance GTraversable UAddr where
  gtraverse _ (UAddr x) = Control.pure (UAddr x)
  {-# INLINE gtraverse #-}

instance GTraversable UChar where
  gtraverse _ (UChar x) = Control.pure (UChar x)
  {-# INLINE gtraverse #-}

instance GTraversable UDouble where
  gtraverse _ (UDouble x) = Control.pure (UDouble x)
  {-# INLINE gtraverse #-}

instance GTraversable UFloat where
  gtraverse _ (UFloat x) = Control.pure (UFloat x)
  {-# INLINE gtraverse #-}

instance GTraversable UInt where
  gtraverse _ (UInt x) = Control.pure (UInt x)
  {-# INLINE gtraverse #-}

instance GTraversable UWord where
  gtraverse _ (UWord x) = Control.pure (UWord x)
  {-# INLINE gtraverse #-}

-- | Implementation of 'Data.Functor.Linear.traverse' for types which derive
-- (linear) 'Generics.Linear.Generic1'.
--
-- ### Performance note
--
-- At present, this function does not perform well for recursive types like lists;
-- it will not specialize to either
--
-- ### Example
--
-- > data T
-- > $(deriveGeneric1 ''T)
-- >
-- > instance Traversable T where
-- >   traverse = genericTraverse
--
-- Note that, contrary to many other classes in linear-base, we can't define
-- `Traversable T` using deriving via, because the
-- [role](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/roles.html)
-- of `t`, in the type of 'Data.Functor.Linear.traverse', is nominal.
genericTraverse ::
  (Generic1 t, GTraversable (Rep1 t), Control.Applicative f) =>
  (a %1 -> f b) ->
  t a %1 ->
  f (t b)
genericTraverse f = lowerYoneda . lowerCurriedC . Control.fmap to1 . gtraverse f . from1
{-# INLINE genericTraverse #-}
