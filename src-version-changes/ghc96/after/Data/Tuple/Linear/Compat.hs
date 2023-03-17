{-# LANGUAGE LinearTypes #-}

module Data.Tuple.Linear where

import Data.Tuple

-- | The 'Solo' data constructor was renamed to 'MkSolo' in GHC 9.6 (see
-- [#437](https://github.com/tweag/linear-base/issues/437)). Because at present
-- there is no linear pattern synonym, and in order to stay compatible with GHC
-- 9.4 we use a constructor and a destructor functions as a workaround (it's
-- quite easy in the case of 'Solo' anyway).
unSolo :: Solo a %p -> a
unSolo (MkSolo a) = a

-- | See 'unSolo'.
mkSolo :: a %p -> Solo a
mkSolo = MkSolo
