{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Linear.Internal where

import Prelude.Linear.Internal.Simple
import Prelude (String)
import qualified Control.Monad as NonLinear ()
import Data.Functor.Identity
import Data.Monoid.Linear
import qualified Data.Functor.Linear.Internal as Data
import qualified Control.Monad.Trans.Reader as NonLinear
import qualified Control.Monad.Trans.State.Strict as NonLinear

-- $monad

-- TODO: explain that the category of linear function is self-enriched, and that
-- this is a hierarchy of enriched monads. In order to have some common
-- vocabulary.

-- There is also room for another type of functor where map has type `(a #->b)
-- -> f a #-> f b`. `[]` and `Maybe` are such functors (they are regular
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
  fmap :: (a #-> b) #-> f a #-> f b

-- | Control linear applicative functors. These represent effectful
-- computations that could produce continuations that can be applied with
-- '<*>'.
class (Data.Applicative f, Functor f) => Applicative f where
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  -- | Inject (and consume) a value into an applicative control functor.
  pure :: a #-> f a
  -- | Apply the linear function in a control applicative functor to the value
  -- of type @a@ in another functor. This is essentialy composing two effectful
  -- computations, one that produces a function @f :: a #-> b@ and one that
  -- produces a value of type @a@ into a single effectful computation that
  -- produces a value of type @b@.
  (<*>) :: f (a #-> b) #-> f a #-> f b
  (<*>) = liftA2 id
  -- | @liftA2 g@ consumes @g@ linearly as it lifts it
  -- over two functors: @liftA2 g :: f a #-> f b #-> f c@.
  liftA2 :: (a #-> b #-> c) #-> f a #-> f b #-> f c
  liftA2 f x y = f <$> x <*> y

-- | Control linear monads.
-- A linear monad is one in which you sequence linear functions in a context,
-- i.e., you sequence functions of the form @a #-> m b@.
class Applicative m => Monad m where
  {-# MINIMAL (>>=) #-}
  -- | @x >>= g@ applies a /linear/ function @g@ linearly (i.e., using it
  -- exactly once) on the value of type @a@ inside the value of type @m a@
  (>>=) :: m a #-> (a #-> m b) #-> m b
  (>>) :: m () #-> m a #-> m a
  m >> k = m >>= (\() -> k)

-- | This class handles pattern-matching failure in do-notation.
-- See "Control.Monad.Fail" for details.
class Monad m => MonadFail m where
  fail :: String -> m a

---------------------------
-- Convenience operators --
---------------------------

-- | Apply the control @fmap@ over a data functor.
dataFmapDefault :: Functor f => (a #-> b) -> f a #-> f b
dataFmapDefault f = fmap f

-- | Apply the control @pure@ over a data applicative.
dataPureDefault :: Applicative f => a -> f a
dataPureDefault x = pure x

{-# INLINE (<$>) #-}
(<$>) :: Functor f => (a #-> b) #-> f a #-> f b
(<$>) = fmap

{-# INLINE return #-}
return :: Monad m => a #-> m a
return x = pure x

-- | Given an effect-producing computation that produces an effect-producing computation
-- that produces an @a@, simplify it to an effect-producing
-- computation that produces an @a@.
join :: Monad m => m (m a) #-> m a
join action = action >>= id

-- | Use this operator to define Applicative instances in terms of Monad instances.
ap :: Monad m => m (a #-> b) #-> m a #-> m b
ap f x = f >>= (\f' -> fmap f' x)

-------------------
-- Miscellaneous --
-------------------

-- | Fold from left to right with a linear monad.
-- This is a linear version of 'NonLinear.foldM'.
foldM :: forall m a b. Monad m => (b #-> a #-> m b) -> b #-> [a] #-> m b
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
    where go :: a #-> (a,b) #-> (a,b)
          go b1 (b2, y) = (b1 <> b2, y)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  Identity x >>= f = f x

-- XXX: Temporary, until newtype record projections are linear.
runIdentity' :: Identity a #-> a
runIdentity' (Identity x) = x

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
runReaderT' :: NonLinear.ReaderT r m a #-> r -> m a
runReaderT' (NonLinear.ReaderT f) = f

instance Functor m => Functor (NonLinear.StateT s m) where
  fmap f (NonLinear.StateT x) = NonLinear.StateT $ \s -> fmap (\(a, s') -> (f a, s')) $ x s

----------------------------
-- New monad transformers --
----------------------------

-- | A (strict) linear state monad transformer.
newtype StateT s m a = StateT (s #-> m (a, s))
  deriving Data.Applicative via Data (StateT s m)
  -- We derive Data.Applicative and not Data.Functor since Data.Functor can use
  -- weaker constraints on m than Control.Functor, while
  -- Data.Applicative needs a Monad instance just like Control.Applicative.

type State s = StateT s Identity

runStateT :: StateT s m a #-> s #-> m (a, s)
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

state :: Applicative m => (s #-> (a,s)) #-> StateT s m a
state f = StateT (pure . f)

runState :: State s a #-> s #-> (a, s)
runState f = runIdentity' . runStateT f

mapStateT :: (m (a, s) #-> n (b, s)) #-> StateT s m a #-> StateT s n b
mapStateT r (StateT f) = StateT (r . f)

withStateT :: (s #-> s) #-> StateT s m a #-> StateT s m a
withStateT r (StateT f) = StateT (f . r)

execStateT :: Functor m => StateT s m () #-> s #-> m s
execStateT f = fmap (\((), s) -> s) . (runStateT f)

mapState :: ((a,s) #-> (b,s)) #-> State s a #-> State s b
mapState f = mapStateT (Identity . f . runIdentity')

withState :: (s #-> s) #-> State s a #-> State s a
withState = withStateT

execState :: State s () #-> s #-> s
execState f = runIdentity' . execStateT f

modify :: Applicative m => (s #-> s) #-> StateT s m ()
modify f = state $ \s -> ((), f s)
-- TODO: add strict version of `modify`

-- | @replace s@ will replace the current state with the new given state, and
-- return the old state.
replace :: Applicative m => s #-> StateT s m s
replace s = state $ (\s' -> (s', s))

-----------------------------
-- Monad transformer class --
-----------------------------

class (forall m. Monad m => Monad (t m)) => MonadTrans t where
  lift :: Monad m => m a #-> t m a

instance MonadTrans (NonLinear.ReaderT r) where
  lift x = NonLinear.ReaderT (\_ -> x)

instance MonadTrans (StateT s) where
  lift x = StateT (\s -> fmap (,s) x)
