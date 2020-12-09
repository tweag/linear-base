{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Linear.Internal where

import Prelude.Linear.Internal
import Prelude (String)
import qualified Control.Monad as NonLinear ()
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Functor.Compose
import Data.Monoid.Linear hiding (Sum)
import qualified Data.Functor.Internal.Linear.Functor as Data
import qualified Data.Functor.Internal.Linear.Applicative as Data
import qualified Control.Monad.Trans.Reader as NonLinear
import qualified Control.Monad.Trans.State.Strict as NonLinear

-- $monad

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
  fmap :: (a %1-> b) %1-> f a %1-> f b

-- | Control linear applicative functors. These represent effectful
-- computations that could produce continuations that can be applied with
-- '<*>'.
class (Data.Applicative f, Functor f) => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  -- | Inject (and consume) a value into an applicative control functor.
  pure :: a %1-> f a
  -- | Apply the linear function in a control applicative functor to the value
  -- of type @a@ in another functor. This is essentialy composing two effectful
  -- computations, one that produces a function @f :: a %1-> b@ and one that
  -- produces a value of type @a@ into a single effectful computation that
  -- produces a value of type @b@.
  (<*>) :: f (a %1-> b) %1-> f a %1-> f b
  (<*>) = liftA2 id
  -- | @liftA2 g@ consumes @g@ linearly as it lifts it
  -- over two functors: @liftA2 g :: f a %1-> f b %1-> f c@.
  liftA2 :: (a %1-> b %1-> c) %1-> f a %1-> f b %1-> f c
  liftA2 f x y = f <$> x <*> y

-- | Control linear monads.
-- A linear monad is one in which you sequence linear functions in a context,
-- i.e., you sequence functions of the form @a %1-> m b@.
class Applicative m => Monad m where
  {-# MINIMAL (>>=) #-}
  -- | @x >>= g@ applies a /linear/ function @g@ linearly (i.e., using it
  -- exactly once) on the value of type @a@ inside the value of type @m a@
  (>>=) :: m a %1-> (a %1-> m b) %1-> m b
  (>>) :: m () %1-> m a %1-> m a
  m >> k = m >>= (\() -> k)

-- | This class handles pattern-matching failure in do-notation.
-- See "Control.Monad.Fail" for details.
class Monad m => MonadFail m where
  fail :: String -> m a

---------------------------
-- Convenience operators --
---------------------------

-- | Apply the control @fmap@ over a data functor.
dataFmapDefault :: Functor f => (a %1-> b) -> f a %1-> f b
dataFmapDefault f = fmap f

-- | Apply the control @pure@ over a data applicative.
dataPureDefault :: Applicative f => a -> f a
dataPureDefault x = pure x

{-# INLINE (<$>) #-}
(<$>) :: Functor f => (a %1-> b) %1-> f a %1-> f b
(<$>) = fmap

-- |  @
--    ('<&>') = 'flip' 'fmap'
--    @
(<&>) :: Functor f => f a %1-> (a %1-> b) %1-> f b
(<&>) a f = f <$> a
{-# INLINE (<&>) #-}

{-# INLINE return #-}
return :: Monad m => a %1-> m a
return x = pure x

-- | Given an effect-producing computation that produces an effect-producing computation
-- that produces an @a@, simplify it to an effect-producing
-- computation that produces an @a@.
join :: Monad m => m (m a) %1-> m a
join action = action >>= id

-- | Use this operator to define Applicative instances in terms of Monad instances.
ap :: Monad m => m (a %1-> b) %1-> m a %1-> m b
ap f x = f >>= (\f' -> fmap f' x)

-------------------
-- Miscellaneous --
-------------------

-- | Fold from left to right with a linear monad.
-- This is a linear version of 'NonLinear.foldM'.
foldM :: forall m a b. Monad m => (b %1-> a %1-> m b) -> b %1-> [a] %1-> m b
foldM _ i [] = return i
foldM f i (x:xs) = f i x >>= \i' -> foldM f i' xs

-----------------------------------------------
-- Deriving Data.XXX in terms of Control.XXX --
-----------------------------------------------

-- | This is a newtype for deriving Data.XXX classes from
-- Control.XXX classes.
newtype Data f a = Data (f a)

instance Functor f => Data.Functor (Data f) where
  fmap f (Data x) = Data (fmap f x)

instance Applicative f => Data.Applicative (Data f) where
  pure x = Data (pure x)
  Data f <*> Data x = Data (f <*> x)

---------------------
-- Basic instances --
---------------------

instance Functor ((,) a) where
  fmap f (a, x) = (a, f x)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (a, f) <*> (b, x) = (a <> b, f x)

instance Monoid a => Monad ((,) a) where
  (a, x) >>= f = go a (f x)
    where go :: a %1-> (a,b) %1-> (a,b)
          go b1 (b2, y) = (b1 <> b2, y)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  Identity x >>= f = f x

-- XXX: Temporary, until newtype record projections are linear.
runIdentity' :: Identity a %1-> a
runIdentity' (Identity x) = x

instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (InL fa) = InL (fmap f fa)
  fmap f (InR ga) = InR (fmap f ga)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ fmap (fmap f) fga

------------------------------------------------
-- Instances for nonlinear monad transformers --
------------------------------------------------

instance Functor m => Functor (NonLinear.ReaderT r m) where
  fmap f (NonLinear.ReaderT g) = NonLinear.ReaderT $ \r -> fmap f (g r)
instance Applicative m => Applicative (NonLinear.ReaderT r m) where
  pure x = NonLinear.ReaderT $ \_ -> pure x
  NonLinear.ReaderT f <*> NonLinear.ReaderT x = NonLinear.ReaderT $ \r -> f r <*> x r
instance Monad m => Monad (NonLinear.ReaderT r m) where
  NonLinear.ReaderT x >>= f = NonLinear.ReaderT $ \r -> x r >>= (\a -> runReaderT' (f a) r)

-- XXX: Temporary, until newtype record projections are linear.
runReaderT' :: NonLinear.ReaderT r m a %1-> r -> m a
runReaderT' (NonLinear.ReaderT f) = f

instance Functor m => Functor (NonLinear.StateT s m) where
  fmap f (NonLinear.StateT x) = NonLinear.StateT $ \s -> fmap (\(a, s') -> (f a, s')) $ x s

----------------------------
-- New monad transformers --
----------------------------

-- | A (strict) linear state monad transformer.
newtype StateT s m a = StateT (s %1-> m (a, s))
  deriving Data.Applicative via Data (StateT s m)
  -- We derive Data.Applicative and not Data.Functor since Data.Functor can use
  -- weaker constraints on m than Control.Functor, while
  -- Data.Applicative needs a Monad instance just like Control.Applicative.

type State s = StateT s Identity

runStateT :: StateT s m a %1-> s %1-> m (a, s)
runStateT (StateT f) = f

instance Data.Functor m => Data.Functor (StateT s m) where
  fmap f (StateT x) = StateT (\s -> Data.fmap (\(a, s') -> (f a, s')) (x s))

instance Functor m => Functor (StateT s m) where
  fmap f (StateT x) = StateT (\s -> fmap (\(a, s') -> (f a, s')) (x s))

instance Monad m => Applicative (StateT s m) where
  pure x = StateT (\s -> return (x,s))
  StateT mf <*> StateT mx = StateT $ \s -> do
    (f, s') <- mf s
    (x, s'') <- mx s'
    return (f x, s'')

instance Monad m => Monad (StateT s m) where
  StateT mx >>= f = StateT $ \s -> do
    (x, s') <- mx s
    runStateT (f x) s'

state :: Applicative m => (s %1-> (a,s)) %1-> StateT s m a
state f = StateT (pure . f)

runState :: State s a %1-> s %1-> (a, s)
runState f = runIdentity' . runStateT f

mapStateT :: (m (a, s) %1-> n (b, s)) %1-> StateT s m a %1-> StateT s n b
mapStateT r (StateT f) = StateT (r . f)

withStateT :: (s %1-> s) %1-> StateT s m a %1-> StateT s m a
withStateT r (StateT f) = StateT (f . r)

execStateT :: Functor m => StateT s m () %1-> s %1-> m s
execStateT f = fmap (\((), s) -> s) . (runStateT f)

mapState :: ((a,s) %1-> (b,s)) %1-> State s a %1-> State s b
mapState f = mapStateT (Identity . f . runIdentity')

withState :: (s %1-> s) %1-> State s a %1-> State s a
withState = withStateT

execState :: State s () %1-> s %1-> s
execState f = runIdentity' . execStateT f

modify :: Applicative m => (s %1-> s) %1-> StateT s m ()
modify f = state $ \s -> ((), f s)
-- TODO: add strict version of `modify`

-- | @replace s@ will replace the current state with the new given state, and
-- return the old state.
replace :: Applicative m => s %1-> StateT s m s
replace s = state $ (\s' -> (s', s))

-----------------------------
-- Monad transformer class --
-----------------------------

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a %1-> t m a

instance MonadTrans (NonLinear.ReaderT r) where
  lift x = NonLinear.ReaderT (\_ -> x)

instance MonadTrans (StateT s) where
  lift x = StateT (\s -> fmap (,s) x)
