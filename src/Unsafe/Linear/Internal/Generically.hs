{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Unsafe.Linear.Internal.Generically where

import qualified Data.Functor.Linear.Internal.Functor as Data
import qualified Data.Functor.Linear.Internal.Traversable as Data
import qualified Control.Functor.Linear.Internal.Class as Control
import GHC.Generics
import qualified Unsafe.Linear.Internal as Unsafe
import Prelude.Linear.Internal

newtype Generically a = Generically a
newtype Generically1 f a = Generically1 (f a)

unGenerically :: Generically a %1-> a
unGenerically (Generically a) = a

unGenerically1 :: Generically1 f a %1-> f a
unGenerically1 (Generically1 fa) = fa

instance (Generic1 f, Data.Functor (Rep1 f)) => Data.Functor (Generically1 f) where
  fmap f = Generically1 . Unsafe.toLinear to1 . Data.fmap f . Unsafe.toLinear from1 . unGenerically1

instance (Generic1 f, Data.Traversable (Rep1 f)) => Data.Traversable (Generically1 f) where
  traverse f = Control.fmap (Generically1 . Unsafe.toLinear to1) . Data.traverse f . Unsafe.toLinear from1 . unGenerically1

instance (Generic1 f, Control.Functor (Rep1 f)) => Control.Functor (Generically1 f) where
  fmap f = Generically1 . Unsafe.toLinear to1 . Control.fmap f . Unsafe.toLinear from1 . unGenerically1
