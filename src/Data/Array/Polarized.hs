{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Array.Polarized
  ( transfer
  , walk
  )
  where

-- XXX: there is lots of scope for testing array functions

import qualified Data.Array.Destination as DArray
import qualified Data.Array.Polarized.Pull.Internal as Pull
import qualified Data.Array.Polarized.Pull as Pull
import qualified Data.Array.Polarized.Push as Push
import Prelude.Linear

import Data.Vector (Vector)

-- See:
--
-- - http://lopezjuan.com/limestone/vectorcomp.pdf (available on the wayback
--   machine)
-- - http://jyp.github.io/posts/controlled-fusion.html
--
-- The general spirit is: `Array` are those arrays which are friendly in
-- returned-value position. And `PullArray` are those arrays which are friendly
-- in argument position. If you have more than one array in an unfriendly
-- position, you need to allocate (allocated arrays are friendly in all
-- positions).

transfer :: Pull.Array a ->. Push.Array a
transfer (Pull.Array f n) = Push.Array (\g -> DArray.fromFunction (\i -> g (f i))) n

-- | This is a shortcut convenience function, which does the same thing as
-- `transfer` . `Pull.fromVector`
walk :: Vector a ->. Push.Array a
walk = transfer . Pull.fromVector
