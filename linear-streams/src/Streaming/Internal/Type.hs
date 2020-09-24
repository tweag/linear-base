{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Streaming.Internal.Type
  ( -- * The 'Stream' and 'Of' types
    -- $stream
    Stream (..)
  , Of (..)
  ) where

import qualified Data.Functor.Linear as Data
import qualified Control.Monad.Linear as Control
import qualified Prelude.Linear as Linear
import Prelude.Linear (($), (.))


-- # Data Definitions
-------------------------------------------------------------------------------


{- $stream

    The 'Stream' data type is equivalent to @FreeT@ and can represent any effectful
    succession of steps, where the form of the steps or 'commands' is
    specified by the first (functor) parameter. The effects are performed
    exactly once since the monad is a @Control.Monad@ from
    <https://github.com/tweag/linear-base linear-base>.

> data Stream f m r = Step !(f (Stream f m r)) | Effect (m (Stream f m r)) | Return r

    The /producer/ concept uses the simple functor @ (a,_) @ \- or the stricter
    @ Of a _ @. Then the news at each step or layer is just: an individual item of type @a@.
    Since @Stream (Of a) m r@ is equivalent to @Pipe.Producer a m r@, much of
    the @pipes@ @Prelude@ can easily be mirrored in a @streaming@ @Prelude@. Similarly,
    a simple @Consumer a m r@ or @Parser a m r@ concept arises when the base functor is
    @ (a -> _) @ . @Stream ((->) input) m result@ consumes @input@ until it returns a
    @result@.

    To avoid breaking reasoning principles, the constructors
    should not be used directly. A pattern-match should go by way of 'inspect' \
    \- or, in the producer case, 'Streaming.Prelude.next'
-}
data Stream f m r where
  Step :: !(f (Stream f m r)) #-> Stream f m r
  Effect :: m (Stream f m r) #-> Stream f m r
  Return :: r #-> Stream f m r

-- | A left-strict pair; the base functor for streams of individual elements.
data Of a b where
  (:>) :: !a -> b #-> Of a b

infixr 5 :>


-- # Control.Monad instance for (Stream f m)
-------------------------------------------------------------------------------

-- Note: we have maintained the weakest prerequisite constraints possible.

-- Note: to consume the 'Stream f m a' in the 'Cons' case, you
-- need 'fmap' to consume the stream. This implies at minimum
-- Data.Functor m and Data.Functor m.
instance (Data.Functor m, Data.Functor f) => Data.Functor (Stream f m) where
  fmap :: (Data.Functor m, Data.Functor f) =>
    (a #-> b) -> Stream f m a #-> Stream f m b
  fmap f s = fmap' f s
  {-# INLINABLE fmap #-}

fmap' :: (Data.Functor m, Data.Functor f) =>
  (a #-> b) -> Stream f m a #-> Stream f m b
fmap' f (Return r) = Return (f r)
fmap' f (Step fs) = Step $ Data.fmap (Data.fmap f) fs
fmap' f (Effect ms) = Effect $ Data.fmap (Data.fmap f) ms

-- Note: the 'Control.Functor f' instance is needed.
-- Weaker constraints won't do.
instance (Control.Functor m, Control.Functor f) =>
  Data.Applicative (Stream f m) where
  pure :: a -> Stream f m a
  pure = Return
  {-# INLINE pure #-}

  (<*>) :: (Control.Functor m, Control.Functor f) =>
    Stream f m (a #-> b) #-> Stream f m a #-> Stream f m b
  (<*>) s1 s2 = app s1 s2
  {-# INLINABLE (<*>) #-}

app :: (Control.Functor m, Control.Functor f) =>
  Stream f m (a #-> b) #-> Stream f m a #-> Stream f m b
app (Return f) stream = Control.fmap f stream
app (Step fs) stream = Step $ Control.fmap (Data.<*> stream) fs
app (Effect ms) stream = Effect $ Control.fmap (Data.<*> stream) ms



instance (Control.Functor m, Control.Functor f) =>
  Control.Functor (Stream f m) where
  fmap :: (Data.Functor m, Data.Functor f) =>
    (a #-> b) #-> Stream f m a #-> Stream f m b
  fmap f s = fmap'' f s
  {-# INLINABLE fmap #-}

fmap'' :: (Control.Functor m, Control.Functor f) =>
  (a #-> b) #-> Stream f m a #-> Stream f m b
fmap'' f (Return r) = Return (f r)
fmap'' f (Step fs) = Step $ Control.fmap (Control.fmap f) fs
fmap'' f (Effect ms) = Effect $ Control.fmap (Control.fmap f) ms


instance (Control.Functor m, Control.Functor f) =>
  Control.Applicative (Stream f m) where
  pure :: a #-> Stream f m a
  pure = Return
  {-# INLINE pure #-}

  (<*>) :: (Control.Functor m, Control.Functor f) =>
    Stream f m (a #-> b) #-> Stream f m a #-> Stream f m b
  (<*>) = (Data.<*>)
  {-# INLINE (<*>) #-}

instance (Control.Functor m, Control.Functor f) =>
  Control.Monad (Stream f m) where
  (>>=) :: Stream f m a #-> (a #-> Stream f m b) #-> Stream f m b
  (>>=) = bind
  {-# INLINABLE (>>=) #-}

bind :: (Control.Functor m, Control.Functor f) =>
  Stream f m a #-> (a #-> Stream f m b) #-> Stream f m b
bind (Return a) f = f a
bind (Step fs) f = Step $ Control.fmap (Control.>>= f) fs
bind (Effect ms) f = Effect $ Control.fmap (Control.>>= f) ms


-- # MonadTrans for (Stream f m)
-------------------------------------------------------------------------------

instance Control.Functor f => Control.MonadTrans (Stream f) where
  lift :: (Control.Functor m, Control.Functor f) => m a #-> Stream f m a
  lift = Effect . Control.fmap Control.return
  {-# INLINE lift #-}


-- # Control.Functor for (Of)
-------------------------------------------------------------------------------

ofFmap :: (a #-> b) #-> (Of x a) #-> (Of x b)
ofFmap f (a :> b) = a :> f b
{-# INLINE ofFmap #-}

instance Data.Functor (Of a) where
  fmap = Linear.forget ofFmap
  {-# INLINE fmap #-}

instance Control.Functor (Of a) where
  fmap = ofFmap
  {-# INLINE fmap #-}

