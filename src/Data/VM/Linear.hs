{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.VM.Linear
  ( VM
  , elim
  , make
  , update
  , module Data.VM.Linear.Internal.Instances
  ) where

import Data.VM.Linear.Internal.VM
import Data.VM.Linear.Internal.Instances ()

