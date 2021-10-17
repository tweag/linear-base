{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Functor.Linear.Internal.Traversable
  ( -- * Linear traversable hierarchy
    -- $ traversable
    Traversable(..)
  , mapM, sequenceA, for, forM
  , mapAccumL, mapAccumR
  , genericTraverse
  ) where

import qualified Control.Functor.Linear.Internal.Class as Control
import Control.Functor.Linear.Yoneda
import qualified Control.Functor.Linear.Internal.State as Control
import qualified Control.Functor.Linear.Internal.Instances as Control
import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Linear.Internal.Applicative as Data
import Data.Functor.Const
import Prelude.Linear.Internal
import Prelude (Maybe(..), Either(..))
import Generics.Linear
import Data.V.Linear.Internal.V (V (..))
import qualified Data.V.Linear.Internal.V as V
import qualified Data.Vector as Vector
import GHC.TypeNats (KnownNat)
import qualified Unsafe.Linear as Unsafe
import GHC.Types (Multiplicity (..))

-- $traversable

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
--
class Data.Functor t => Traversable t where
  {-# MINIMAL traverse | sequence #-}

  traverse :: Control.Applicative f => (a %1-> f b) -> t a %1-> f (t b)
  {-# INLINE traverse #-}
  traverse f x = sequence (Data.fmap f x)

  sequence :: Control.Applicative f => t (f a) %1-> f (t a)
  {-# INLINE sequence #-}
  sequence = traverse id

mapM :: (Traversable t, Control.Monad m) => (a %1-> m b) -> t a %1-> m (t b)
mapM = traverse
{-# INLINE mapM #-}

sequenceA :: (Traversable t, Control.Applicative f) => t (f a) %1-> f (t a)
sequenceA = sequence
{-# INLINE sequenceA #-}

for :: (Traversable t, Control.Applicative f) => t a %1-> (a %1-> f b) -> f (t b)
for t f = traverse f t
{-# INLINE for #-}

forM :: (Traversable t, Control.Monad m) => t a %1-> (a %1-> m b) -> m (t b)
forM = for
{-# INLINE forM #-}

mapAccumL :: Traversable t => (a %1-> b %1-> (a,c)) -> a %1-> t b %1-> (a, t c)
mapAccumL f s t = swap $ Control.runState (traverse (\b -> Control.state $ \i -> swap $ f i b) t) s

mapAccumR :: Traversable t => (a %1-> b %1-> (a,c)) -> a %1-> t b %1-> (a, t c)
mapAccumR f s t = swap $ runStateR (traverse (\b -> StateR $ \i -> swap $ f i b) t) s

swap :: (a,b) %1-> (b,a)
swap (x,y) = (y,x)

-- | A right-to-left state transformer
newtype StateR s a = StateR (s %1-> (a, s))
  deriving (Data.Functor, Data.Applicative) via Control.Data (StateR s)

runStateR :: StateR s a %1-> s %1-> (a, s)
runStateR (StateR f) = f

instance Control.Functor (StateR s) where
  fmap f (StateR x) = StateR $ (\(a, s') -> (f a, s')) . x

instance Control.Applicative (StateR s) where
  pure x = StateR $ \s -> (x,s)
  StateR f <*> StateR x = StateR (go . Control.fmap f . x)
    where go :: (a, (a %1-> b, s)) %1-> (b, s)
          go (a, (h, s'')) = (h a, s'')

------------------------
-- Standard instances --
------------------------

instance KnownNat n => Traversable (V n) where
  traverse f (V xs) =
    (V . Unsafe.toLinear (Vector.fromListN (V.theLength @n))) Data.<$>
    traverse f (Unsafe.toLinear Vector.toList xs)

----------------------------
-- Generic derived instances
----------------------------

instance Traversable [] where
  traverse = genericTraverse
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

-- For the Data.Applicative variant we'd have
-- instance Moveable v => Traversable (K1 i v) where
--   traverse _ (K1 c) = move c & \case (Ur u) -> Data.pure (K1 u)

----------------------
-- Generic deriving --
----------------------

-- | Generically derive a suitable definition of 'traverse'.
--
-- === Caution
--
-- GHC's optimizer often fails to inline away the generic representations of
-- traversals for sum types. To the best of my knowledge, no one has found a
-- good workaround for this problem.
genericTraverse
  :: (Generic1 t, GTraversable (Rep1 t), Control.Applicative f)
  => (a %1-> f b) -> t a %1-> f (t b)
genericTraverse f = lowerCY . Control.fmap to1 . gtraverse f . from1
{-# INLINABLE genericTraverse #-}

class GTraversable t where
  gtraverse :: Control.Applicative f => (a %1-> f b) -> t a %1-> CY f (t b)
instance GTraversable U1 where
  gtraverse _ U1 = Data.pure U1
  {-# INLINE gtraverse #-}
instance (GTraversable f, GTraversable g) => GTraversable (f :*: g) where
  gtraverse f (l :*: r) = Data.liftA2 (:*:) (gtraverse f l) (gtraverse f r)
  {-# INLINE gtraverse #-}
instance (GTraversable f, GTraversable g) => GTraversable (f :+: g) where
  gtraverse f (L1 a) = L1 Data.<$> gtraverse f a
  gtraverse f (R1 a) = R1 Data.<$> gtraverse f a
  {-# INLINE gtraverse #-}

instance GTraversable f => GTraversable (M1 i c f) where
  gtraverse f (M1 a) = M1 Data.<$> gtraverse f a
  {-# INLINE gtraverse #-}
instance (GTraversable f, m ~ 'One) => GTraversable (MP1 m f) where
  gtraverse f (MP1 a) = MP1 Data.<$> gtraverse f a
  {-# INLINE gtraverse #-}
instance GTraversable Par1 where
  gtraverse f (Par1 a) = Par1 Data.<$> liftCY (f a)
  {-# INLINE gtraverse #-}
instance (GTraversable f, Traversable g) => GTraversable (f :.: g) where
  gtraverse f (Comp1 a) = Comp1 Data.<$> gtraverse (traverse f) a
  {-# INLINE gtraverse #-}
-- These are the only instances where we need @Control.Applicative@.
instance GTraversable V1 where
  -- This somewhat lazy instance is typical for the Traversable instances
  -- in base, for better or for worse.
  gtraverse _ x = Control.pure ((\case) x)
instance GTraversable (K1 i v) where
  gtraverse _ (K1 c) = Control.pure (K1 c)
  {-# INLINE gtraverse #-}
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

-- CY f is a slight simplification, by Sjoerd Visscher, of the
-- Curried (Yoneda f) (Yoneda f) type used to implement
-- Control.Lens.Traversal.confusing. Specifically,
--
--   CY f ~ Curried (Yoneda f) f
--
-- This removes one layer of continuation passing, at the expense
-- of a bit more implementation complexity in <*>.
--
-- Note that what we're doing here is *not* equivalent to applying
-- confusing itself to a plain traversal of the generics, and gives
-- much cleaner code than that would.
newtype CY f a = CY
  { _runCY :: forall r. Yoneda f (a %1-> r) %1-> f r }
instance Data.Functor (CY f) where
  fmap f (CY q) = CY (\p -> q (Control.fmap @(Yoneda f) (. f) p))
  {-# INLINE fmap #-}
instance Control.Functor (CY f) where
  fmap f (CY q) = CY (\p -> q (Control.fmap @(Yoneda f) (. f) p))
  {-# INLINE fmap #-}
instance Data.Applicative (CY f) where
  pure a = Control.pure a
  (<*>) = (Control.<*>)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
instance Control.Applicative (CY f) where
  pure a = CY (\(Yoneda y) -> y ($ a))
  CY p <*> CY q = CY (\w -> p (Yoneda (\zig -> q (Control.fmap @(Yoneda f) (\br a -> zig (\ab -> br (ab a))) w))))
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

lowerCY :: Control.Applicative f => CY f a %1-> f a
lowerCY (CY m) = m (Control.pure id)
{-# INLINE lowerCY #-}

liftCY :: Control.Applicative f => f a %1-> CY f a
liftCY fa = CY (\p -> lowerYoneda (yap p fa))
{-# INLINE liftCY #-}

-- Alternative implementations of CY. None of the implementations seem
-- to produce good code for nontrivial sum types in general. *sigh*

{-
newtype CY f a = CY (Curried (Coyoneda f) (Coyoneda f) a)
--  deriving newtype (Data.Functor, Control.Functor, Data.Applicative, Control.Applicative)
instance Data.Functor (CY f) where
  fmap f (CY x) = CY (Data.fmap f x)
  {-# INLINE fmap #-}
instance Control.Functor (CY f) where
  fmap f (CY x) = CY (Control.fmap f x)
  {-# INLINE fmap #-}
instance Data.Applicative (CY f) where
  pure a = CY (Data.pure a)
  CY xs <*> CY ys = CY (xs Data.<*> ys)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
instance Control.Applicative (CY f) where
  pure a = CY (Control.pure a)
  CY xs <*> CY ys = CY (xs Control.<*> ys)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
mapCoerceCY :: forall f a b. Coercible a b => CY f a %1-> CY f b
mapCoerceCY = coerce (id :: CY f a %1-> CY f a)
{-# INLINE mapCoerceCY #-}

lowerCY :: Control.Applicative f => CY f a %1-> f a
lowerCY (CY q) = lowerCoyoneda (lowerCurried q)
{-# INLINE lowerCY #-}

liftCY :: Control.Applicative f => f a %1-> CY f a
liftCY fa = CY (Curried (`coyap` fa))
{-# INLINE liftCY #-}
-}

{-
newtype CY f a = CY (Curried (Yoneda f) (Yoneda f) a)
--  deriving newtype (Data.Functor, Control.Functor, Data.Applicative, Control.Applicative)
instance Data.Functor (CY f) where
  fmap f (CY x) = CY (Data.fmap f x)
  {-# INLINE fmap #-}
instance Control.Functor (CY f) where
  fmap f (CY x) = CY (Control.fmap f x)
  {-# INLINE fmap #-}
instance Data.Applicative (CY f) where
  pure a = CY (Data.pure a)
  CY xs <*> CY ys = CY (xs Data.<*> ys)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
instance Control.Applicative (CY f) where
  pure a = CY (Control.pure a)
  CY xs <*> CY ys = CY (xs Control.<*> ys)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

lowerCY :: Control.Applicative f => CY f a %1-> f a
lowerCY (CY q) = lowerYoneda (lowerCurried q)
{-# INLINE lowerCY #-}

liftCY :: Control.Applicative f => f a %1-> CY f a
liftCY fa = CY (Curried (`yap` fa))
{-# INLINE liftCY #-}
-}
