{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}


module Streaming.Type where

import Data.Functor.Identity
import qualified Control.Monad.Linear as Linear
import qualified Data.Functor.Linear as Data
import qualified Prelude.Linear as Linear
import qualified Control.Monad.Builder as Control

-- # Data Definitions
-------------------------------------------------------------------------------

data Stream f m r where
  Step :: !(f (Stream f m r)) #-> Stream f m r
  Effect :: (m (Stream f m r)) #-> Stream f m r
  Return :: r #-> Stream f m r

data Of a b where
  (:>) :: !a -> b #-> Of a b



-- # Instances
-------------------------------------------------------------------------------

instance Data.Functor (Of a) where
  fmap f (a :> b) =  a :> (f b)

instance Linear.Functor (Of a) where
  fmap f (a :> b) =  a :> (f b)

instance (Data.Functor f, Linear.Monad m) => Data.Functor (Stream f m) where
  fmap :: (Data.Functor f, Linear.Monad m) =>
    (a #-> b) -> Stream f m a #-> Stream f m b
  fmap f (Return r) = Return (f r)
  fmap f (Effect mstream) = Effect (Data.fmap (Data.fmap f) mstream)
  fmap f (Step fstream) = Step (Data.fmap (Data.fmap f) fstream)

instance (Linear.Functor f, Linear.Monad m) => Linear.Functor (Stream f m) where
  fmap :: (Linear.Functor f, Linear.Monad m) =>
    (a #-> b) #-> Stream f m a #-> Stream f m b
  fmap f (Return r) = Return (f r)
  fmap f (Effect mstream) = Effect (Linear.fmap (Linear.fmap f) mstream)
  fmap f (Step fstream) = Step (Linear.fmap (Linear.fmap f) fstream)


instance (Linear.Functor f, Linear.Monad m) => Data.Applicative (Stream f m) where
  pure :: r -> Stream f m r
  pure = Return

  -- XXX: Why is do notation a pain here?
  (<*>) :: (Linear.Functor f, Linear.Monad m) =>
    Stream f m (a #-> b) #-> Stream f m a #-> Stream f m b
  Return f <*> streamx = Linear.fmap f streamx
  (Effect mStream) <*> streamx = Effect Linear.$
    mStream Linear.>>= \stream -> Linear.return (stream Data.<*> streamx)
  (Step fStream) <*> streamx = Step Linear.$
    Linear.fmap ((flip (Data.<*>)) streamx) fStream


instance (Linear.Functor f, Linear.Monad m) => Linear.Applicative (Stream f m) where
  pure :: r #-> Stream f m r
  pure = Return

  (<*>) = (Data.<*>)

instance (Linear.Functor f, Linear.Monad m) => Linear.Monad (Stream f m) where
  Return r >>= useR = useR r
  Step r >>= useR = Step Linear.$ Linear.fmap (Linear.>>= useR) r
  Effect m >>= useR =  Effect Linear.$
    m Linear.>>= (Linear.return Linear.. (Linear.>>= useR))

instance Linear.Functor f => Linear.MonadTrans (Stream f) where
  lift :: Linear.Monad m => m a #-> Stream f m a
  lift ma = Effect (Linear.fmap Linear.return ma)


-- XXX: Should be in linear base
flip :: (a #-> b #-> c) #-> (b #-> a #-> c)
flip f b a = f a b
