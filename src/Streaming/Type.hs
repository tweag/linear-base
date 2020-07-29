{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module Streaming.Type
  ( Stream (..)
  , Of (..)
  ) where

import qualified Data.Functor.Linear as Data
import qualified Control.Monad.Linear as Control
import qualified Prelude.Linear as Linear
import Prelude.Linear (($), (.))


-- # Data Definitions
-------------------------------------------------------------------------------

data Stream f m r where
  Step :: !(f (Stream f m r)) #-> Stream f m r
  Effect :: m (Stream f m r) #-> Stream f m r
  Return :: r #-> Stream f m r

data Of a b where
  (:>) :: !a -> b #-> Of a b

infixr 5 :>


-- # Control.Monad instance for (Stream f m)
-------------------------------------------------------------------------------

-- Note: we have maintained the weakest prerequisite constraints possible.

type DFunctor = Data.Functor
type DApplicative = Data.Applicative
type CFunctor = Control.Functor
type CApplicative = Control.Applicative
type CMonad = Control.Monad

-- Note: to consume the 'Stream f m a' in the 'Cons' case, you
-- need 'fmap' to consume the stream. This implies at minimum
-- Data.Functor m and Data.Functor m.
instance (DFunctor m, DFunctor f) => Data.Functor (Stream f m) where
  fmap :: (DFunctor m, DFunctor f) =>
    (a #-> b) -> Stream f m a #-> Stream f m b
  fmap f (Return r) = Return (f r)
  fmap f (Step fs) = Step $ Data.fmap (Data.fmap f) fs
  fmap f (Effect ms) = Effect $ Data.fmap (Data.fmap f) ms

-- Note: the 'CFunctor f' instance is needed. Weaker constraints won't do.
instance (CFunctor m, CFunctor f) => Data.Applicative (Stream f m) where
  pure :: a -> Stream f m a
  pure = Return

  (<*>) :: (CFunctor m, CFunctor f) =>
    Stream f m (a #-> b) #-> Stream f m a #-> Stream f m b
  (Return f) <*> stream = Control.fmap f stream
  (Step fs) <*> stream = Step $ Control.fmap (Data.<*> stream) fs
  (Effect ms) <*> stream = Effect $ Control.fmap (Data.<*> stream) ms

instance (CFunctor m, CFunctor f) => Control.Functor (Stream f m) where
  fmap :: (CFunctor m, CFunctor f) =>
    (a #-> b) #-> Stream f m a #-> Stream f m b
  fmap f (Return r) = Return (f r)
  fmap f (Step fs) = Step $ Control.fmap (Control.fmap f) fs
  fmap f (Effect ms) = Effect $ Control.fmap (Control.fmap f) ms

instance (CFunctor m, CFunctor f) => Control.Applicative (Stream f m) where
  pure :: a #-> Stream f m a
  pure = Return

  (<*>) :: (CFunctor m, CFunctor f) =>
    Stream f m (a #-> b) #-> Stream f m a #-> Stream f m b
  (<*>) = (Data.<*>)

instance (CFunctor m, CFunctor f) => Control.Monad (Stream f m) where
  (>>=) :: Stream f m a #-> (a #-> Stream f m b) #-> Stream f m b
  (Return a) >>= f = f a
  (Step fs) >>= f = Step $ Control.fmap (Control.>>= f) fs
  (Effect ms) >>= f = Effect $ Control.fmap (Control.>>= f) ms


-- # MonadTrans for (Stream f m)
-------------------------------------------------------------------------------

instance Control.Functor f => Control.MonadTrans (Stream f) where
  lift :: (CFunctor m, CFunctor f) => m a #-> Stream f m a
  lift = Effect . Control.fmap Control.return


-- # Control.Functor for (Of)
-------------------------------------------------------------------------------

ofFmap :: (a #-> b) #-> (Of x a) #-> (Of x b)
ofFmap f (a :> b) = a :> f b

instance Data.Functor (Of a) where
  fmap = Linear.forget ofFmap

instance Control.Functor (Of a) where
  fmap = ofFmap

