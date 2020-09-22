{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains all functions that do something with
-- multiple streams as input or output. This includes combining
-- streams, splitting a stream, etc.
module Streaming.Internal.Many
  (
  -- * Operations that use or return multiple 'Stream's
  -- ** Unzip
    unzip
  -- ** Merging
  -- $ merging
  , merge
  , mergeOn
  , mergeBy
  ) where

import Streaming.Internal.Type
import Streaming.Internal.Consume
import Prelude (Ord(..), Ordering(..))
import Prelude.Linear (($), (&))
import qualified Control.Monad.Linear as Control


-- # Unzip
-------------------------------------------------------------------------------

{-| The type

> Data.List.unzip     :: [(a,b)] -> ([a],[b])

   might lead us to expect

> Streaming.unzip :: Stream (Of (a,b)) m r -> Stream (Of a) m (Stream (Of b) m r)

   which would not stream, since it would have to accumulate the second stream (of @b@s).
   Of course, @Data.List@ 'Data.List.unzip' doesn't stream either.

   This @unzip@ does
   stream, though of course you can spoil this by using e.g. 'toList':

>>> let xs = Prelude.map (\x -> (x, Prelude.show x)) [1..5 :: Int]

>>> S.toList $ S.toList $ S.unzip (S.each' xs)
["1","2","3","4","5"] :> ([1,2,3,4,5] :> ())

>>> Prelude.unzip xs
([1,2,3,4,5],["1","2","3","4","5"])

    Note the difference of order in the results. It may be of some use to think why.
    The first application of 'toList' was applied to a stream of integers:

>>> :t S.unzip $ S.each' xs
S.unzip $ S.each' xs :: Control.Monad m => Stream (Of Int) (Stream (Of String) m) ()

    Like any fold, 'toList' takes no notice of the monad of effects.

> toList :: Control.Monad m => Stream (Of a) m r #-> m (Of [a] r)

    In the case at hand (since I am in @ghci@) @m = Stream (Of String) IO@.
    So when I apply 'toList', I exhaust that stream of integers, folding
    it into a list:

>>> :t S.toList $ S.unzip $ S.each' xs
S.toList $ S.unzip $ S.each' xs
  :: Control.Monad m => Stream (Of String) m (Of [Int] ())

    When I apply 'toList' to /this/, I reduce everything to an ordinary action in @IO@,
    and return a list of strings:

>>> S.toList $ S.toList $ S.unzip (S.each' xs)
["1","2","3","4","5"] :> ([1,2,3,4,5] :> ())

'unzip' can be considered a special case of either 'unzips' or 'expand':

@
  unzip = 'unzips' . 'maps' (\((a,b) :> x) -> Compose (a :> b :> x))
  unzip = 'expand' $ \p ((a,b) :> abs) -> b :> p (a :> abs)
@
-}
unzip :: Control.Monad m =>
  Stream (Of (a, b)) m r #-> Stream (Of a) (Stream (Of b) m) r
unzip = loop
  where
  loop :: Control.Monad m =>
    Stream (Of (a, b)) m r #-> Stream (Of a) (Stream (Of b) m) r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap loop $ Control.lift m
    Step ((a,b):> rest) -> Step (a :> Effect (Step (b :> Return (loop rest))))
{-# INLINABLE unzip #-}


-- # Merging
-------------------------------------------------------------------------------

{- $merging
   These functions combine two sorted streams of orderable elements
   into one sorted stream. The elements of the merged stream are
   guaranteed to be in a sorted order if the two input streams are
   also sorted.

   The merge operation is /left-biased/: when merging two elements
   that compare as equal, the left element is chosen first.
-}

{- | Merge two streams of elements ordered with their 'Ord' instance.

   The return values of both streams are returned.

>>> S.print $ merge (each [1,3,5]) (each [2,4])
1
2
3
4
5
((), ())

-}
merge :: (Control.Monad m, Ord a) =>
  Stream (Of a) m r #-> Stream (Of a) m s #-> Stream (Of a) m (r,s)
merge = mergeBy compare
{-# INLINE merge #-}

{- | Merge two streams, ordering them by applying the given function to
   each element before comparing.

   The return values of both streams are returned.
-}
mergeOn :: (Control.Monad m, Ord b) =>
  (a -> b) ->
  Stream (Of a) m r #->
  Stream (Of a) m s #->
  Stream (Of a) m (r,s)
mergeOn f = mergeBy (\x y -> compare (f x) (f y))
{-# INLINE mergeOn #-}

{- | Merge two streams, ordering the elements using the given comparison function.

   The return values of both streams are returned.
-}
mergeBy :: forall m a r s . Control.Monad m =>
  (a -> a -> Ordering) ->
  Stream (Of a) m r #->
  Stream (Of a) m s #->
  Stream (Of a) m (r,s)
mergeBy comp s1 s2 = loop s1 s2
  where
    loop :: Stream (Of a) m r #-> Stream (Of a) m s #-> Stream (Of a) m (r,s)
    loop s1 s2 = s1 & \case
      Return r ->
        Effect $ effects s2 Control.>>= \s -> Control.return $ Return (r, s)
      Effect ms -> Effect $
        ms Control.>>= \s1' -> Control.return $ mergeBy comp s1' s2
      Step (a :> as) -> s2 & \case
        Return s ->
          Effect $ effects as Control.>>= \r -> Control.return $ Return (r, s)
        Effect ms -> Effect $
          ms Control.>>= \s2' ->
            Control.return $ mergeBy comp (Step (a :> as)) s2'
        Step (b :> bs) -> case comp a b of
          LT -> Step (a :> Step (b :> mergeBy comp as bs))
          _ -> Step (b :> Step (a :> mergeBy comp as bs))
{-# INLINABLE mergeBy #-}

