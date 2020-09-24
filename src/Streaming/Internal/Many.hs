{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE GADTs #-}
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
  -- ** Zips and Unzip
    unzip
  , ZipResidual
  , ZipResidual3
  , zip
  , zipR
  , zipWith
  , zipWithR
  , zip3
  , zip3R
  , zipWith3
  , zipWith3R
  , Either3 (..)
  -- ** Merging
  -- $ merging
  , merge
  , mergeOn
  , mergeBy
  ) where

import Streaming.Internal.Type
import Streaming.Internal.Consume
import Prelude (Either(..), Ord(..), Ordering(..))
import Prelude.Linear (($), (&))
import qualified Control.Monad.Linear as Control


-- # Zips and Unzip
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


{- Remarks on the design of zip functions

Zip functions have two design choices:
(1) What do we do with the end-of-stream values of both streams?
(2) If the streams are of different length, do we keep or throw out the
remainder of the longer stream?

* We are assuming not to take infinite streams as input and instead deal with
reasonably small finite streams.
* To avoid making choices for the user, we keep both end-of-stream payloads
* The default zips (ones without a prime in the name) use @effects@ to consume
the remainder stream after zipping. We include zip function variants that
return no remainder (for equal length streams), or the remainder of the
longer stream.

-}

data Either3 a b c where
  Left3 :: a #-> Either3 a b c
  Middle3 :: b #-> Either3 a b c
  Right3 :: c #-> Either3 a b c

-- | The remainder of zipping two streams
type ZipResidual a b m r1 r2 =
  Either3
    (r1, r2)
    (r1, Stream (Of b) m r2)
    (Stream (Of a) m r1, r2)

-- | @zipWithR@ zips two streams applying a function along the way,
-- keeping the remainder of zipping if there is one.  Note. If two streams have
-- the same length, but one needs to perform some effects to obtain the
-- end-of-stream result, that stream is treated as a residual.
zipWithR :: Control.Monad m =>
  (a -> b -> c) ->
  Stream (Of a) m r1 #->
  Stream (Of b) m r2 #->
  Stream (Of c) m (ZipResidual a b m r1 r2)
zipWithR = loop
  where
  loop :: Control.Monad m =>
    (a -> b -> c) ->
    Stream (Of a) m r1 #->
    Stream (Of b) m r2 #->
    Stream (Of c) m (ZipResidual a b m r1 r2)
  loop f st1 st2 = st1 & \case
    Effect ms -> Effect $ Control.fmap (\s -> loop f s st2) ms
    Return r1 -> st2 & \case
      Return r2 -> Return $ Left3 (r1,r2)
      st2' -> Return $ Middle3 (r1,st2')
    Step (a :> as) -> st2 & \case
      Effect ms ->
        Effect $ Control.fmap (\s -> loop f (Step (a :> as)) s) ms
      Return r2 -> Return $ Right3 (Step (a :> as), r2)
      Step (b :> bs) -> Step $ (f a b) :> loop f as bs
{-# INLINABLE zipWithR #-}

zipWith :: Control.Monad m =>
  (a -> b -> c) ->
  Stream (Of a) m r1 #->
  Stream (Of b) m r2 #->
  Stream (Of c) m (r1,r2)
zipWith f s1 s2 = Control.do
  result <- zipWithR f s1 s2
  result & \case
    Left3 rets -> Control.return rets
    Middle3 (r1, s2') -> Control.do
      r2 <- Control.lift $ effects s2'
      Control.return (r1, r2)
    Right3 (s1', r2) -> Control.do
      r1 <- Control.lift $ effects s1'
      Control.return (r1, r2)
{-# INLINABLE zipWith #-}

-- | @zip@ zips two streams exhausing the remainder of the longer
-- stream and consuming its effects.
zip :: Control.Monad m =>
  Stream (Of a) m r1 #->
  Stream (Of b) m r2 #->
  Stream (Of (a,b)) m (r1, r2)
zip = zipWith (,)
{-# INLINE zip #-}

-- | @zipR@ zips two streams keeping the remainder if there is one.
zipR :: Control.Monad m =>
  Stream (Of a) m r1 #->
  Stream (Of b) m r2 #->
  Stream (Of (a,b)) m (ZipResidual a b m r1 r2)
zipR = zipWithR (,)
{-# INLINE zipR #-}

-- Remark. For simplicity, we do not create an @Either7@ which is the
-- proper remainder type for 'zip3R'. Our type simply has one impossible
-- case which is when all three streams have a remainder.

-- | The (liberal) remainder of zipping three streams.
-- This has the downside that the possibility of three remainders
-- is allowed, though it will never occur.
type ZipResidual3 a b c m r1 r2 r3 =
  ( Either r1 (Stream (Of a) m r1)
  , Either r2 (Stream (Of b) m r2)
  , Either r3 (Stream (Of c) m r3)
  )

-- | Like @zipWithR@ but with three streams.
zipWith3R :: Control.Monad m =>
  (a -> b -> c -> d) ->
  Stream (Of a) m r1 #->
  Stream (Of b) m r2 #->
  Stream (Of c) m r3 #->
  Stream (Of d) m (ZipResidual3 a b c m r1 r2 r3)
zipWith3R = loop
  where
  loop :: Control.Monad m =>
    (a -> b -> c -> d) ->
    Stream (Of a) m r1 #->
    Stream (Of b) m r2 #->
    Stream (Of c) m r3 #->
    Stream (Of d) m (ZipResidual3 a b c m r1 r2 r3)
  loop f s1 s2 s3 = s1 & \case
    Effect ms -> Effect $ Control.fmap (\s -> loop f s s2 s3) ms
    Return r1 -> (s2, s3) & \case
      (Return r2, Return r3) -> Return (Left r1, Left r2, Left r3)
      (s2', s3') -> Return (Left r1, Right s2', Right s3')
    Step (a :> as) -> s2 & \case
      Effect ms -> Effect $
        Control.fmap (\s -> loop f (Step $ a :> as) s s3) ms
      Return r2 -> Return (Right (Step $ a :> as), Left r2, Right s3)
      Step (b :> bs) -> s3 & \case
        Effect ms -> Effect $
          Control.fmap (\s -> loop f (Step $ a :> as) (Step $ b :> bs) s) ms
        Return r3 ->
          Return (Right (Step $ a :> as), Right (Step $ b :> bs), Left r3)
        Step (c :> cs) -> Step $ (f a b c) :> loop f as bs cs
{-# INLINABLE zipWith3R #-}

-- | Like @zipWith@ but with three streams
zipWith3 :: Control.Monad m =>
  (a -> b -> c -> d) ->
  Stream (Of a) m r1 #->
  Stream (Of b) m r2 #->
  Stream (Of c) m r3 #->
  Stream (Of d) m (r1, r2, r3)
zipWith3 f s1 s2 s3 = Control.do
  result <- zipWith3R f s1 s2 s3
  result & \case
    (res1, res2, res3) -> Control.do
      r1 <- Control.lift $ extractResult res1
      r2 <- Control.lift $ extractResult res2
      r3 <- Control.lift $ extractResult res3
      Control.return (r1, r2, r3)
{-# INLINABLE zipWith3 #-}

-- | Like @zipR@ but with three streams.
zip3 :: Control.Monad m =>
  Stream (Of a) m r1 #->
  Stream (Of b) m r2 #->
  Stream (Of c) m r3 #->
  Stream (Of (a,b,c)) m (r1, r2, r3)
zip3 = zipWith3 (,,)
{-# INLINABLE zip3 #-}

-- | Like @zipR@ but with three streams.
zip3R :: Control.Monad m =>
  Stream (Of a) m r1 #->
  Stream (Of b) m r2 #->
  Stream (Of c) m r3 #->
  Stream (Of (a,b,c)) m (ZipResidual3 a b c m r1 r2 r3)
zip3R = zipWith3R (,,)
{-# INLINABLE zip3R #-}

-- | Internal function to consume a stream remainder to
-- get the payload
extractResult :: Control.Monad m => Either r (Stream (Of a) m r) #-> m r
extractResult (Left r) = Control.return r
extractResult (Right s) = effects s


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

