{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module contains all the classes eventually exported by
-- "Control.Functor.Linear". Together with related operations.
module Control.Functor.Linear.Internal.Class
  ( -- * Functors
    Functor (..),
    dataFmapDefault,
    (<$>),
    (<&>),
    (<$),
    void,

    -- * Applicative Functors
    Applicative (..),
    dataPureDefault,

    -- * Monads
    Monad (..),
    MonadFail (..),
    return,
    join,
    ap,
    foldM,
  )
where

import qualified Control.Monad as NonLinear ()
import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Linear.Internal.Applicative as Data
import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Sum as Sum
import Data.Monoid.Linear
import Data.Type.Bool
import Data.Unrestricted.Linear.Internal.Consumable
import GHC.TypeLits
import GHC.Types (Type)
import Generics.Linear
import Prelude.Linear.Generically
import Prelude.Linear.Internal
import Prelude.Linear.Unsatisfiable (Unsatisfiable, unsatisfiable)
import Prelude (Bool (..), String)

-- # Control Functors
-------------------------------------------------------------------------------

-- TODO: explain that the category of linear function is self-enriched, and that
-- this is a hierarchy of enriched monads. In order to have some common
-- vocabulary.

-- There is also room for another type of functor where map has type `(a %1->b)
-- -> f a %1-> f b`. `[]` and `Maybe` are such functors (they are regular
-- (endo)functors of the category of linear functions whereas `LFunctor` are
-- control functors). A Traversable hierarchy would start with non-control
-- functors.

-- TODO: make the laws explicit

-- | Control linear functors. The functor of type
-- @f a@ holds only one value of type @a@ and represents a computation
-- producing an @a@ with an effect. All control functors are data functors,
-- but not all data functors are control functors.
class Data.Functor f => Functor f where
  -- | Map a linear function @g@ over a control functor @f a@.
  -- Note that @g@ is used linearly over the single @a@ in @f a@.
  fmap :: (a %1 -> b) %1 -> f a %1 -> f b

-- | Apply the control @fmap@ over a data functor.
dataFmapDefault :: Functor f => (a %1 -> b) -> f a %1 -> f b
dataFmapDefault f = fmap f

(<$>) :: Functor f => (a %1 -> b) %1 -> f a %1 -> f b
(<$>) = fmap
{-# INLINE (<$>) #-}

infixl 4 <$> -- same fixity as base.<$>

-- |  @
--    ('<&>') = 'flip' 'fmap'
--    @
(<&>) :: Functor f => f a %1 -> (a %1 -> b) %1 -> f b
(<&>) a f = f <$> a
{-# INLINE (<&>) #-}

infixl 1 <&> -- same fixity as base.<&>

-- | Linearly typed replacement for the standard '(Prelude.<$)' function.
(<$) :: (Functor f, Consumable b) => a %1 -> f b %1 -> f a
a <$ fb = fmap (`lseq` a) fb

infixl 4 <$ -- same fixity as base.<$

-- | Discard a consumable value stored in a control functor.
void :: (Functor f, Consumable a) => f a %1 -> f ()
void = fmap consume

-- # Control Applicatives
-------------------------------------------------------------------------------

-- | Control linear applicative functors. These represent effectful
-- computations that could produce continuations that can be applied with
-- '<*>'.
class (Data.Applicative f, Functor f) => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}

  -- | Inject (and consume) a value into an applicative control functor.
  pure :: a %1 -> f a

  -- | Apply the linear function in a control applicative functor to the value
  -- of type @a@ in another functor. This is essentialy composing two effectful
  -- computations, one that produces a function @f :: a %1-> b@ and one that
  -- produces a value of type @a@ into a single effectful computation that
  -- produces a value of type @b@.
  (<*>) :: f (a %1 -> b) %1 -> f a %1 -> f b
  (<*>) = liftA2 id

  infixl 4 <*> -- same fixity as base.<*>

  -- | @liftA2 g@ consumes @g@ linearly as it lifts it
  -- over two functors: @liftA2 g :: f a %1-> f b %1-> f c@.
  liftA2 :: (a %1 -> b %1 -> c) %1 -> f a %1 -> f b %1 -> f c
  liftA2 f x y = f <$> x <*> y

-- | Apply the control @pure@ over a data applicative.
dataPureDefault :: Applicative f => a -> f a
dataPureDefault x = pure x

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (a, f) <*> (b, x) = (a <> b, f x)

instance (Monoid a, Monoid b) => Applicative ((,,) a b) where
  pure x = (mempty, mempty, x)
  (a1, a2, f) <*> (b1, b2, x) = (a1 <> b1, a2 <> b2, f x)

instance (Monoid a, Monoid b, Monoid c) => Applicative ((,,,) a b c) where
  pure x = (mempty, mempty, mempty, x)
  (a1, a2, a3, f) <*> (b1, b2, b3, x) = (a1 <> b1, a2 <> b2, a3 <> b3, f x)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  Identity x >>= f = f x

-- # Control Monads
-------------------------------------------------------------------------------

-- | Control linear monads.
-- A linear monad is one in which you sequence linear functions in a context,
-- i.e., you sequence functions of the form @a %1-> m b@.
class Applicative m => Monad m where
  {-# MINIMAL (>>=) #-}

  -- | @x >>= g@ applies a /linear/ function @g@ linearly (i.e., using it
  -- exactly once) on the value of type @a@ inside the value of type @m a@
  (>>=) :: m a %1 -> (a %1 -> m b) %1 -> m b

  infixl 1 >>= -- same fixity as base.>>=

  (>>) :: m () %1 -> m a %1 -> m a
  m >> k = m >>= (\() -> k)
  infixl 1 >> -- same fixity as base.>>

-- | This class handles pattern-matching failure in do-notation.
-- See "Control.Monad.Fail" for details.
class Monad m => MonadFail m where
  fail :: String -> m a

return :: Monad m => a %1 -> m a
return x = pure x
{-# INLINE return #-}

-- | Given an effect-producing computation that produces an effect-producing computation
-- that produces an @a@, simplify it to an effect-producing
-- computation that produces an @a@.
join :: Monad m => m (m a) %1 -> m a
join action = action >>= id

-- | Use this operator to define Applicative instances in terms of Monad instances.
ap :: Monad m => m (a %1 -> b) %1 -> m a %1 -> m b
ap f x = f >>= (\f' -> fmap f' x)

-- | Fold from left to right with a linear monad.
-- This is a linear version of 'NonLinear.foldM'.
foldM :: forall m a b. Monad m => (b %1 -> a %1 -> m b) -> b %1 -> [a] %1 -> m b
foldM _ i [] = return i
foldM f i (x : xs) = f i x >>= \i' -> foldM f i' xs

---------------
-- Instances --
---------------

deriving via
  Generically1 ((,) a)
  instance
    Functor ((,) a)

deriving via
  Generically1 ((,,) a b)
  instance
    Functor ((,,) a b)

deriving via
  Generically1 ((,,,) a b c)
  instance
    Functor ((,,,) a b c)

deriving via
  Generically1 ((,,,,) a b c d)
  instance
    Functor ((,,,,) a b c d)

instance Monoid a => Monad ((,) a) where
  (a, x) >>= f = go a (f x)
    where
      go :: a %1 -> (a, b) %1 -> (a, b)
      go b1 (b2, y) = (b1 <> b2, y)

deriving via
  Generically1 (Sum.Sum f g)
  instance
    (Functor f, Functor g) => Functor (Sum.Sum f g)

deriving via
  Generically1 (Compose f g)
  instance
    (Functor f, Functor g) => Functor (Compose f g)

------------------------
-- Generics instances --
------------------------

instance (Generic1 f, Functor (Rep1 f)) => Functor (Generically1 f) where
  fmap f = Generically1 . to1 . fmap f . from1 . unGenerically1

-- True if the generic type does not contain 'Par1', i.e. it does not use its parameter.
type family NoPar1 (f :: Type -> Type) :: Bool where
  NoPar1 U1 = 'True
  NoPar1 (K1 i v) = 'True
  NoPar1 (l :*: r) = NoPar1 l && NoPar1 r
  NoPar1 (l :+: r) = NoPar1 l && NoPar1 r
  NoPar1 (l :.: r) = NoPar1 l || NoPar1 r
  NoPar1 (M1 i c f) = NoPar1 f
  NoPar1 Par1 = 'False

-- If the generic type does not use its parameter, we can linearly coerce the parameter to any other type.
class NoPar1 f ~ 'True => Unused f where
  unused :: f a %1 -> f b

instance Unused U1 where
  unused U1 = U1

instance Unused (K1 i v) where
  unused (K1 c) = K1 c

instance (Unused l, Unused r) => Unused (l :*: r) where
  unused (l :*: r) = unused l :*: unused r

instance (Unused l, Unused r) => Unused (l :+: r) where
  unused (L1 l) = L1 (unused l)
  unused (R1 r) = R1 (unused r)

instance Unused f => Unused (M1 i c f) where
  unused (M1 a) = M1 (unused a)

instance (Unused' (NoPar1 l) l r, (NoPar1 l || NoPar1 r) ~ 'True) => Unused (l :.: r) where
  unused (Comp1 a) = Comp1 (unused' @(NoPar1 l) a)

class Unused' (left_unused :: Bool) l r where
  unused' :: l (r a) %1 -> l (r b)

instance Unused l => Unused' 'True l r where
  unused' = unused

instance (Functor l, Unused r) => Unused' 'False l r where
  unused' = fmap unused

-- A linear map on a pair is only possible if only one side uses its parameter.
-- To get the right type, the other side can then be coerced (instead of mapped) using `unused`.
class (noPar1l ~ NoPar1 l, noPar1r ~ NoPar1 r) => EitherNoPar1 (noPar1l :: Bool) (noPar1r :: Bool) l r where
  eitherNoPar1Map :: (a %1 -> b) %1 -> (l :*: r) a %1 -> (l :*: r) b

instance (Unused l, Functor r, NoPar1 r ~ 'False) => EitherNoPar1 'True 'False l r where
  eitherNoPar1Map f (l :*: r) = unused l :*: fmap f r

instance (Unused r, Functor l, NoPar1 l ~ 'False) => EitherNoPar1 'False 'True l r where
  eitherNoPar1Map f (l :*: r) = fmap f l :*: unused r

type MessageMany =
  'Text "Can't derive an instance of Functor. One of the constructors"
    ':$$: 'Text "of your datatype uses the type parameter more than once."

instance ('False ~ NoPar1 l, 'False ~ NoPar1 r, Unsatisfiable MessageMany) => EitherNoPar1 'False 'False l r where
  eitherNoPar1Map = unsatisfiable

type MessageZero =
  'Text "Can't derive an instance of Functor. One of the constructors"
    ':$$: 'Text "of your datatype does not use the type parameter."

instance ('True ~ NoPar1 l, 'True ~ NoPar1 r, Unsatisfiable MessageZero) => EitherNoPar1 'True 'True l r where
  eitherNoPar1Map = unsatisfiable

instance (Functor l, Functor r) => Functor (l :+: r) where
  fmap f (L1 a) = L1 (fmap f a)
  fmap f (R1 a) = R1 (fmap f a)

instance Functor f => Functor (M1 j c f) where
  fmap f (M1 a) = M1 (fmap f a)

instance Functor Par1 where
  fmap f (Par1 a) = Par1 (f a)

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp1 fga) = Comp1 (fmap (fmap f) fga)

instance (Data.Functor l, Data.Functor r, EitherNoPar1 b1 b2 l r) => Functor (l :*: r) where
  fmap = eitherNoPar1Map
