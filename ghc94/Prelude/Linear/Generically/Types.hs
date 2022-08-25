{-# LANGUAGE LinearTypes #-}

-- | Prior to GHC 9.4, linear-base defined its own versions of `Generically` and 
-- `Generically1`. As a temporary workaround to enable compilation on both
-- GHC 9.4 and 9.2, this module simply re-exports Generics.Linear, while the
-- 9.2 version exposes linear-base's own implementations.
module Prelude.Linear.Generically.Types
( module Generics.Linear
) where

import Generics.Linear
