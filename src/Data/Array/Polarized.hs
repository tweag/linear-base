{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | This module provides converters and documentation for polarized arrays.
--
-- There are two polarities or \"directions\": push arrays and pull arrays.
--
-- You should use push an pull arrays if you want to explicity control
-- when GHC allocates space in the garbage-collector heap for arrays.
-- The point of doing this is to reduce the amount of memory used
-- and improve the performance of your program.
--
-- The performance of your program is improved because linear types enable the
-- API which does not allow operations on push or pull arrays that cannot be
-- fused. That is, for some subtle and complicated reasons, we can be sure that
-- only one function allocates memory (and it's called 'alloc') and all other
-- compositions do not allocate memory.
--
-- == How to use push and pull arrays
--
-- __The trifecta of controlling allocation:__
--
-- \[ \texttt{Vector a} \]
-- \[ \text{fromVector}~~ { \Huge \swarrow  ~~~~ \nwarrow } ~~~ \text{alloc} \]
-- \[ \texttt{Pull a} {\Huge ~~~ \longrightarrow ~~~} \texttt{Push a}\]
-- \[ \text{transfer}\]
--
-- Pull arrays are good as arguments to functions, because they are easy to
-- \"pull\" from, map over and zip with.
--
-- Push arrays are good as values returned from functions, and are easy to
-- \"write\" to and modify.
--
-- A typical application starts with concrete lists as @Vector@s (which are
-- inputs), goes to @Pull@ arrays, then to @Push@ arrays and ends with
-- @Vector@s (which are outputs).  All the computations inbetween the
-- "fromVector" and "alloc" are fused and no memory from the GC heap is
-- allocated when they are executed.
--
-- Such a combination of functions that controls fusion is called 
-- a __fusion pipeline__.
--
-- == A toy example
--
-- > import Data.Array.Polarized
-- > import qualified Data.Array.Polarized.Push as Push
-- > import qualified Data.Array.Polarized.Pull as Pull
-- > import qualified Data.Functor.Linear as Linear
-- > import Data.Vector (Vector, (!), fromList)
-- > import Prelude
-- >
-- > type Pull a = Pull.Array a
-- > type Push a = Push.Array a
-- >
-- > pipeline :: Vector Int -> Vector Int -> Vector Int
-- > pipeline coeff =
-- >   pushToVector .
-- >   diff .
-- >   addCoeffVector .
-- >   multThree .
-- >   fromVector'
-- >   where
-- >     pushToVector :: Pull Int -> Vector Int
-- >     pushToVector arr = Push.alloc (transfer arr)
-- >
-- >     fromVector' :: Vector Int -> Pull Int
-- >     fromVector' arr = Pull.fromVector arr
-- >
-- >     addCoeffVector :: Pull Int -> Pull Int
-- >     addCoeffVector arr = addCoeff (Pull.fromVector coeff) arr
-- >
-- > multThree :: Pull Int -> Pull Int
-- > multThree arr = Linear.fmap (* 3) arr
-- >
-- > addCoeff :: Pull Int -> Pull Int %1-> Pull Int
-- > addCoeff arr = Pull.zipWith (+) arr
-- >
-- > -- | Takes the difference between elements
-- > -- which is common in differentials
-- > diff :: Pull Int -> Pull Int
-- > diff arr = withLen (Pull.findLength arr)
-- >   where
-- >     withLen :: (Int, Pull Int) -> Pull Int
-- >     withLen (len, arr) =
-- >       case (Pull.split 1 arr, Pull.split (len - 1) arr) of
-- >         ((_, tail), (top,_)) -> Pull.zipWith (-) tail top
-- >
--
--
-- == Background for the interested
--
-- To understand a bit more about why and how these work, see these:
--
-- * http://www.cse.chalmers.se/~josefs/talks/LinArrays.pdf
-- * https://www.sciencedirect.com/science/article/pii/030439759090147A
-- * http://jyp.github.io/posts/controlled-fusion.html
-- * http://lopezjuan.com/limestone/vectorcomp.pdf -- find this on an
-- internet archive like https://archive.org/web/
--
--
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


-- DEVELOPER NOTE:
--
-- The general spirit is: `Push.Array` are those arrays which are friendly in
-- returned-value position. And `Pull.Array` are those arrays which are friendly
-- in argument position. If you have more than one array in an unfriendly
-- position, you need to allocate (allocated arrays are friendly in all
-- positions).
--
-- There are three types of array which are involved, with conversion
-- functions available between them, the third being an allocated Vector.
-- The primary conversion functions are:
-- > Polarized.transfer :: Pull.Array a %1-> Push.Array a
-- > Push.alloc :: Push.Array a %1-> Vector a
-- > Pull.fromVector :: Vector a %1-> Pull.Array a
--
-- In this way, we gain further control over exactly when allocation may occur
-- in a fusion pipeline.
-- In such a pipeline converting one allocated array to another, it would be
-- common to begin with Pull.fromVector, and end with Push.alloc.


-- | Convert a PullArray into a PushArray.
-- NOTE: this does NOT require allocation and can be in-lined.
transfer :: Pull.Array a %1-> Push.Array a
transfer (Pull.Array f n) = Push.Array (\g -> DArray.fromFunction (\i -> g (f i))) n

-- | This is a shortcut convenience function, which does
-- the same thing as `transfer` . `Pull.fromVector`
walk :: Vector a %1-> Push.Array a
walk = transfer . Pull.fromVector
