{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module Simple.DerivingVia where

import Generics.Linear.TH (deriveGenericAnd1)
import Prelude.Linear.Generically (Generically(..), Generically1(..), genericTraverse)

import qualified Data.Functor.Linear as Data
import Data.Unrestricted.Linear

data Test a = One a | Two a Bool | Three (Test (Test a))
  deriving stock (Functor)

deriveGenericAnd1 ''Test

deriving via Generically (Test a)
  instance Consumable a => Consumable (Test a)

deriving via Generically (Test a)
  instance Dupable a => Dupable (Test a)

deriving via Generically (Test a)
  instance Movable a => Movable (Test a)

deriving via Generically1 Test
  instance Data.Functor Test

instance Data.Traversable Test where
  traverse = genericTraverse
