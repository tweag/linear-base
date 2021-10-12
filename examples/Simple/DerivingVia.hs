{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
module Simple.DerivingVia where

import Generics.Linear (Generic, Generic1)
import Unsafe.Linear (Generically(..), Generically1(..), genericTraverse)

import qualified Data.Functor.Linear as Data
import qualified Control.Functor.Linear as Control
import Data.Unrestricted.Linear

data Test a = One a | Two a Bool | Three (Test (Test a))
  deriving stock (Functor, Generic, Generic1)

  deriving (Consumable, Dupable, Movable)
  via Generically (Test a)

  deriving (Data.Functor, Control.Functor)
  via Generically1 Test

instance Data.Traversable Test where
  traverse = genericTraverse
