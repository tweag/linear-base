{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.LArray.Mutable.Unlifted.Linear.Internal where

import Data.Unrestricted.Linear hiding (lseq, dup2)
import qualified Data.Unrestricted.Linear
import Prelude (Int)
import Data.List (length)
import qualified Prelude as Prelude
import Prelude.Linear.Internal
import qualified Unsafe.Linear as Unsafe
import qualified GHC.Exts as GHC

-- | A mutable array holding @a@s
newtype LArray# a = LArray# (GHC.MutableArray# GHC.RealWorld a)

-- | Consume a 'LArray#' and its elements. O(n)
--
-- Note that we can not implement a 'Consumable' instance because 'LArray#'
-- is unlifted.
lseq :: Consumable a => LArray# a %1-> b %1-> b
lseq arr b =
  (Unsafe.toLinear2 (\x _ -> x) b)
    (map consume arr)

-- | Allocate a mutable array of given size using a default value.
--
-- The size should be non-negative.
alloc :: Int -> a -> (LArray# a %1-> Ur b) %1-> Ur b
alloc (GHC.I# s) a f =
  let new = GHC.runRW# Prelude.$ \st ->
        case GHC.newArray# s a st of
          (# _, arr #) -> LArray# arr
   in f new
{-# NOINLINE alloc #-}  -- prevents runRW# from floating outwards

fromList :: [a] %1-> (LArray# a %1-> Ur b) %1-> Ur b
fromList = Unsafe.toLinear2 $ \l f ->
  let !(GHC.I# len) = length l
      new = GHC.runRW# Prelude.$ \st ->
        case GHC.newArray# len Prelude.undefined st of
          (# st', arr #) -> go 0# arr st' l
   in f new
 where
  go :: GHC.Int# -> GHC.MutableArray# GHC.RealWorld a -> GHC.State# GHC.RealWorld -> [a] -> LArray# a
  go _ arr !_ [] = LArray# arr
  go i arr !st (x:xs) =
    let st' = GHC.writeArray# arr i x st
    in  go (i GHC.+# 1#) arr st' xs

{-# NOINLINE fromList #-}  -- prevents runRW# from floating outwards

-- For the reasoning behind these NOINLINE pragmas, see the discussion at:
-- https://github.com/tweag/linear-base/pull/187#pullrequestreview-489183531

-- | Allocate a mutable array of given size using a default value,
-- using another 'LArray#' as a uniqueness proof.
--
-- The size should be non-negative.
allocBeside :: Int -> a -> LArray# b %1-> (# LArray# a, LArray# b #)
allocBeside (GHC.I# s) a orig =
  let new = GHC.runRW# Prelude.$ \st ->
        case GHC.newArray# s a st of
          (# _, arr #) -> LArray# arr
   in (# new, orig #)
{-# NOINLINE allocBeside #-}  -- prevents runRW# from floating outwards

size :: LArray# a %1-> (# Ur Int, LArray# a #)
size = Unsafe.toLinear go
  where
    go :: LArray# a -> (# Ur Int, LArray# a #)
    go (LArray# arr) =
      let !s = GHC.sizeofMutableArray# arr
      in  (# Ur (GHC.I# s), LArray# arr  #)

get :: Dupable a => Int -> LArray# a %1-> (# a, LArray# a #)
get ix arr = runRW# (update' ix Data.Unrestricted.Linear.dup2 arr)
{-# NOINLINE get #-}  -- prevents the runRW# effect from being reordered

set :: Consumable a => Int -> a %1-> LArray# a %1-> LArray# a
set ix x xs =
  (\(# (), r #) -> r)
    (runRW# (update' ix (\old -> (consume old, x)) xs))
{-# NOINLINE set #-}  -- prevents the runRW# effect from being reordered

update' :: Int -> (a %1-> (b, a)) %1-> LArray# a %1-> GHC.State# GHC.RealWorld %1-> (# b, LArray# a #)
update' (GHC.I# ix) = Unsafe.toLinear3 go
  where
    go :: (a %1-> (b, a)) -> LArray# a -> GHC.State# GHC.RealWorld -> (# b, LArray# a #)
    go f (LArray# arr) st =
      case GHC.readArray# arr ix st of
        (# st', a #) ->
          case f a of
            (b, a') ->
               case GHC.writeArray# arr ix a' st' of
                 !_ -> (# b, LArray# arr #)
{-# INLINE update' #-}

update :: Int -> (a %1-> (b, a)) %1-> LArray# a %1-> (# b, LArray# a #)
update ix f arr = runRW# (update' ix f arr)
{-# NOINLINE update #-}  -- prevents the runRW# effect from being reordered

-- | Map over the LArray in-place.
map :: (a %1-> b) -> LArray# a %1-> LArray# b
map (f :: a %1-> b) = Unsafe.toLinear (\(LArray# as) ->
  let -- We alias the input array to write the resulting -- 'b's to,
      -- just to make the typechecker happy. Care must be taken to
      -- only read indices from 'as' that is not yet written to 'bs'.
      bs :: GHC.MutableArray# GHC.RealWorld b
      bs = GHC.unsafeCoerce# as
      len :: GHC.Int#
      len = GHC.sizeofMutableArray# as

      -- For each index ([0..len]), we read the element on 'as', pass
      -- it through 'f' and write to the same location on 'bs'.
      go :: GHC.Int# -> GHC.State# GHC.RealWorld -> ()
      go i st
        | GHC.I# i Prelude.== GHC.I# len = ()
        | Prelude.otherwise =
          case GHC.readArray# as i st of
            (# st', a #) ->
              case GHC.writeArray# bs i (f a) st' of
                !st'' -> go (i GHC.+# 1#) st''
   in GHC.runRW# (go 0#) `GHC.seq` LArray# bs
  )
{-# NOINLINE map #-}

append :: LArray# a %1-> LArray# a %1-> LArray# a
append (LArray# left) (LArray# right) = Unsafe.toLinear2 go left right
 where
  go l r =
    let lsize = GHC.sizeofMutableArray# l
        rsize = GHC.sizeofMutableArray# r
     in GHC.runRW# (\st ->
          case GHC.newArray# (lsize GHC.+# rsize) Prelude.undefined st of
            (# st', dst #) ->
              case GHC.copyMutableArray# l 0# dst 0# lsize st' of
                !st'' ->
                  case GHC.copyMutableArray# r 0# dst lsize rsize st'' of
                    !_ -> LArray# dst
        )

-- | Return the array elements as a lazy list.
toList :: LArray# a %1-> [a]
toList (LArray# arr) =
  Unsafe.toLinear
    (\xs -> go 0 (GHC.I# (GHC.sizeofMutableArray# xs)) xs)
    arr
 where
  go :: Int -> Int -> GHC.MutableArray# GHC.RealWorld a -> [a]
  go i len xs
    | i Prelude.== len = []
    | GHC.I# i# <- i =
        case GHC.runRW# (GHC.readArray# xs i#) of
          (# _, x #) -> x : go (i Prelude.+ 1) len xs

-- | Clone an array and all its elements.
dup2 :: forall a. Dupable a => LArray# a %1-> (# LArray# a, LArray# a #)
dup2 = Unsafe.toLinear (\arr -> GHC.runRW# (go arr))
 where
  go :: LArray# a -> GHC.State# GHC.RealWorld -> (# LArray# a, LArray# a #)
  go (LArray# orig) st =
    let len :: GHC.Int#
        len = GHC.sizeofMutableArray# orig
     in case GHC.newArray# len Prelude.undefined st of
          (# st', left #) ->
            case GHC.newArray# len Prelude.undefined st' of
              (# st'', right #) ->
                 let loop :: GHC.Int# -> GHC.State# GHC.RealWorld -> ()
                     loop i !st0
                       | GHC.I# i Prelude.== GHC.I# len = ()
                       | Prelude.otherwise =
                           case GHC.readArray# orig i st0 of
                             (# st1, a #) -> case Data.Unrestricted.Linear.dup2 a of
                               (a1, a2) -> case GHC.writeArray# left i a1 st1 of
                                 st2 -> case GHC.writeArray# right i a2 st2 of
                                   st3 -> loop (i GHC.+# 1#) st3
                  in case loop 0# st'' of
                      () -> (# LArray# left, LArray# right #)
{-# NOINLINE dup2 #-}

-- Utils

runRW# :: forall (r :: GHC.RuntimeRep) (o :: GHC.TYPE r). (GHC.State# GHC.RealWorld %1-> o) %1-> o
runRW# = Unsafe.coerce GHC.runRW#
