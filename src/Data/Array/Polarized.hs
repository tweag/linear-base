{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Array.Polarized
  ( transfer
  , walk
  )
  where

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
-- | The general spirit is: `Push.Array` are those arrays which are friendly in
-- returned-value position. And `Pull.Array` are those arrays which are friendly
-- in argument position. If you have more than one array in an unfriendly
-- position, you need to allocate (allocated arrays are friendly in all
-- positions).
--
-- There are three types of array which are involved, with conversion
-- functions available between them, the third being an allocated Vector.
-- The primary conversion functions are:
-- > Polarized.transfer :: Pull.Array a #-> Push.Array a
-- > Push.alloc :: Push.Array a #-> Vector a
-- > Pull.fromVector :: Vector a #-> Pull.Array a
-- In this way, we gain further control over exactly when allocation may occur
-- in a fusion pipeline.
-- In such a pipeline converting one allocated array to another, it would be
-- common to begin with Pull.fromVector, and end with Push.alloc.

-- | Convert a PullArray into a PushArray.
transfer :: Pull.Array a #-> Push.Array a
transfer (Pull.Array f n) = Push.Array (\g -> DArray.fromFunction (\i -> g (f i))) n

-- | This is a shortcut convenience function, which does the same thing as
-- `transfer` . `Pull.fromVector`
walk :: Vector a #-> Push.Array a
walk = transfer . Pull.fromVector
