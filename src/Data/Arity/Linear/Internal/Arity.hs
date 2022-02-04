{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Arity.Linear.Internal.Arity (Arity) where

import GHC.TypeLits

type family Arity b f where
  Arity b b = 0
  Arity b (a %1 -> f) = Arity b f + 1
  Arity b f =
    TypeError
      ( 'Text "Arity: "
          ':<>: 'ShowType f
          ':<>: 'Text " isn't a linear function with head "
          ':<>: 'ShowType b
          ':<>: 'Text "."
      )
