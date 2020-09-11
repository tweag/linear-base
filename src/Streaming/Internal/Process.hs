{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides functions that take one input
-- stream and produce one output stream. These are functions that
-- process a single stream.
module Streaming.Internal.Process
  (
  -- * Stream processors
  -- ** Splitting and inspecting streams of elements
    next
  , uncons
  , splitAt
  , split
  , breaks
  , break
  , breakWhen
  , span
  , group
  , groupBy
  -- ** Sum and compose manipulation
  , distinguish
  , switch
  , separate
  , unseparate
  , eitherToSum
  , sumToEither
  , sumToCompose
  , composeToSum
  -- ** Partitions
  , partitionEithers
  , partition
  -- ** Maybes
  , catMaybes
  , mapMaybe
  , mapMaybeM
  -- ** Direct Transformations
  , hoist
  , map
  , mapM
  , maps
  , mapped
  , mapsPost
  , mapsMPost
  , mappedPost
  , for
  , with
  , subst
  , copy
  , duplicate
  , store
  , chain
  , sequence
  , nubOrd
  , nubOrdOn
  , nubInt
  , nubIntOn
  , filter
  , filterM
  , intersperse
  , drop
  , dropWhile
  , scan
  , scanM
  , scanned
  , delay
  , read
  , show
  , cons
  , slidingWindow
  , wrapEffect
  -- ** Internal
  , destroyExposed
  ) where

import Streaming.Internal.Type
import Prelude.Linear ((&), ($), (.))
import qualified Prelude.Linear as Linear
import Prelude (Maybe(..), Either(..), Bool(..), Int, fromInteger,
               Ordering(..), Num(..), Eq(..), id, Ord(..), Read(..),
               String, Double)
import qualified Prelude
import Data.Unrestricted.Linear
import qualified Control.Monad.Linear as Control
import Control.Monad.Linear.Builder (BuilderType(..), monadBuilder)
import System.IO.Linear
import Data.Functor.Sum
import Data.Functor.Compose
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.IntSet as IntSet
import Text.Read (readMaybe)
import Control.Concurrent (threadDelay)
import GHC.Stack


-- # Internal Library
-------------------------------------------------------------------------------

-- | When chunking streams, it's useful to have a combinator
-- that can add an element to the functor that is itself a stream.
-- Basically `consFirstChunk 42 [[1,2,3],[4,5]] = [[42,1,2,3],[4,5]]`.
consFirstChunk :: Control.Monad m =>
  a -> Stream (Stream (Of a) m) m r #-> Stream (Stream (Of a) m) m r
consFirstChunk a stream = stream & \case
    Return r -> Step (Step (a :> Return (Return r)))
    Effect m -> Effect $ Control.fmap (consFirstChunk a) m
    Step f -> Step (Step (a :> f))
  where
    Builder{..} = monadBuilder

-- This is an internal function used in 'seperate' from the original source.
-- It removes functoral and monadic steps and reduces to some type 'b'.
-- Here it's adapted to consume the stream linearly.
destroyExposed
  :: forall f m r b. (Control.Functor f, Control.Monad m) =>
     Stream f m r #-> (f b #-> b) -> (m b #-> b) -> (r #-> b) -> b
destroyExposed stream0 construct theEffect done = loop stream0
  where
    loop :: (Control.Functor f, Control.Monad m) =>
      Stream f m r #-> b
    loop stream = stream & \case
      Return r -> done r
      Effect m -> theEffect (Control.fmap loop m)
      Step f  -> construct (Control.fmap loop f)


-- # Splitting and inspecting streams of elements
-------------------------------------------------------------------------------

-- Remark. Since the 'a' is not held linearly in the 'Of' pair,
-- we return it inside an 'Unrestricted'.
--
{-| The standard way of inspecting the first item in a stream of elements, if the
     stream is still \'running\'. The @Right@ case contains a
     Haskell pair, where the more general @inspect@ would return a left-strict pair.
     There is no reason to prefer @inspect@ since, if the @Right@ case is exposed,
     the first element in the pair will have been evaluated to whnf.

> next    :: Control.Monad m => Stream (Of a) m r #-> m (Either r    (a, Stream (Of a) m r))
> inspect :: Control.Monad m => Stream (Of a) m r #-> m (Either r (Of a (Stream (Of a) m r)))
-}
next :: forall a m r. Control.Monad m =>
  Stream (Of a) m r #-> m (Either r (Unrestricted a, Stream (Of a) m r))
next stream = loop stream
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> m (Either r (Unrestricted a, Stream (Of a) m r))
    loop stream = stream & \case
      Return r -> return $ Left r
      Effect ms -> ms >>= next
      Step (a :> as) -> return $ Right (Unrestricted a, as)
{-# INLINABLE next #-}

{-| Inspect the first item in a stream of elements, without a return value.

-}
uncons :: forall a m r. (Consumable r, Control.Monad m) =>
  Stream (Of a) m r #-> m (Maybe (a, Stream (Of a) m r))
uncons  stream = loop stream
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> m (Maybe (a, Stream (Of a) m r))
    loop stream = stream & \case
      Return r -> lseq r $ return Nothing
      Effect ms -> ms >>= uncons
      Step (a :> as) -> return $ Just (a, as)
{-# INLINABLE uncons #-}

{-| Split a succession of layers after some number, returning a streaming or
    effectful pair. This function is the same as the 'splitsAt' exported by the
    @Streaming@ module, but since this module is imported qualified, it can
    usurp a Prelude name. It specializes to:

>  splitAt :: Control.Monad m => Int -> Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)

-}
splitAt :: forall f m r. (Control.Monad m, Control.Functor f) =>
  Int -> Stream f m r #-> Stream f m (Stream f m r)
splitAt n stream = loop n stream where
  Builder{..} = monadBuilder
  loop :: Int -> Stream f m r #-> Stream f m (Stream f m r)
  loop n stream = case Prelude.compare n 0 of
    GT -> stream & \case
      Return r -> Return (Return r)
      Effect m -> Effect $ m >>= (return . splitAt n)
      Step f -> Step $ Control.fmap (splitAt (n-1)) f
    _ -> Return stream
{-# INLINABLE splitAt #-}

{-| Split a stream of elements wherever a given element arises.
    The action is like that of 'Prelude.words'.

>>> S.stdoutLn $ mapped S.toList $ S.split ' ' $ each' "hello world  "
hello
world

-}
split :: forall a m r. (Eq a, Control.Monad m) =>
  a -> Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
split x stream = loop stream
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ m >>= (return . split x)
      Step (a :> as) -> case a == x of
        True -> split x as
        False -> consFirstChunk a (split x as)
{-# INLINABLE split #-}

{-| Break a sequence upon meeting an element that falls under a predicate,
    keeping it and the rest of the stream as the return value.

>>> rest <- S.print $ S.break even $ each' [1,1,2,3]
1
1
>>> S.print rest
2
3

-}
break :: forall a m r. Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)
break f stream = loop stream
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)
    loop stream = stream & \case
      Return r -> Return (Return r)
      Effect m -> Effect $ Control.fmap (break f) m
      Step (a :> as) -> case f a of
        True -> Return $ Step (a :> as)
        False -> Step (a :> (break f as))
{-# INLINABLE break #-}

{-| Break during periods where the predicate is not satisfied,
   grouping the periods when it is.

>>> S.print $ mapped S.toList $ S.breaks not $ S.each' [False,True,True,False,True,True,False]
[True,True]
[True,True]
>>> S.print $ mapped S.toList $ S.breaks id $ S.each' [False,True,True,False,True,True,False]
[False]
[False]
[False]

-}
breaks :: forall a m r. Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
breaks f stream = loop stream 
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap (breaks f) m
      Step (a :> as) -> case f a of
        True -> breaks f as
        False -> consFirstChunk a (breaks f as)
{-# INLINABLE breaks #-}

-- Remark. The funny type of this seems to be made to interoperate well with
-- `purely` from the `foldl` package.
--
{-| Yield elements, using a fold to maintain state, until the accumulated
   value satifies the supplied predicate. The fold will then be short-circuited
   and the element that breaks it will be put after the break.
   This function is easiest to use with 'Control.Foldl.purely'

>>> rest <- each' [1..10] & L.purely S.breakWhen L.sum (>10) & S.print
1
2
3
4
>>> S.print rest
5
6
7
8
9
10

-}
breakWhen :: forall m a x b r. Control.Monad m
          => (x -> a -> x) -> x -> (x -> b) -> (b -> Bool)
          -> Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)
breakWhen step x end pred stream = loop stream
  where
    loop :: Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)
    loop stream = stream & \case
      Return r -> Return (Return r)
      Effect m -> Effect $ Control.fmap (breakWhen step x end pred) m
      Step (a :> as) -> case pred (end (step x a)) of
        False -> Step $ a :> (breakWhen step (step x a) end pred as)
        True -> Return (Step (a :> as))
{-# INLINABLE breakWhen #-}

-- | Breaks on the first element to satisfy the predicate
breakWhen' :: Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)
breakWhen' f stream = breakWhen (\x a -> f a) True id id stream
{-# INLINE breakWhen' #-}

-- | Stream elements until one fails the condition, return the rest.
span :: Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)
span f = break (Prelude.not Prelude.. f)
{-# INLINE span #-}

{-| Group elements of a stream in accordance with the supplied comparison.


>>> S.print $ mapped S.toList $ S.groupBy (>=) $ each' [1,2,3,1,2,3,4,3,2,4,5,6,7,6,5]
[1]
[2]
[3,1,2,3]
[4,3,2,4]
[5]
[6]
[7,6,5]

-}
groupBy :: forall a m r. Control.Monad m =>
  (a -> a -> Bool) -> Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
groupBy equals stream = loop stream
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap (groupBy equals) m
      Step (a :> as) -> as & \case
        Return r -> Step (Step (a :> Return (Return r)))
        Effect m -> Effect $ m >>= (\s -> return $ groupBy equals (Step (a :> s)))
        Step (a' :> as') -> case equals a a' of
          False -> Step $ Step $ a :> (Return $ groupBy equals (Step (a' :> as')))
          True -> Step $ Step $ a :> (Step $ a' :> (Return $ groupBy equals as'))
{-# INLINABLE groupBy #-}

{-| Group successive equal items together

>>> S.toList $ mapped S.toList $ S.group $ each' "baaaaad"
["b","aaaaa","d"] :> ()

>>> S.toList $ concats $ maps (S.drained . S.splitAt 1) $ S.group $ each' "baaaaaaad"
"bad" :> ()

-}
group :: (Control.Monad m, Eq a) =>
  Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
group = groupBy (==)
{-# INLINE group #-}

-- # Sum and compose manipulation
-------------------------------------------------------------------------------

-- Remark. Most of these functions are general and were merely cut and pasted
-- from the original library.

distinguish :: (a -> Bool) -> Of a r -> Sum (Of a) (Of a) r
distinguish predicate (a :> b) = case predicate a of
  True -> InR (a :> b)
  False -> InL (a :> b)
{-# INLINE distinguish #-}

{-| Swap the order of functors in a sum of functors.

>>> S.toList $ S.print $ separate $ maps S.switch $ maps (S.distinguish (=='a')) $ S.each' "banana"
'a'
'a'
'a'
"bnn" :> ()
>>> S.toList $ S.print $ separate $ maps (S.distinguish (=='a')) $ S.each' "banana"
'b'
'n'
'n'
"aaa" :> ()
-}
switch :: Sum f g r -> Sum g f r
switch s = case s of InL a -> InR a; InR a -> InL a
{-# INLINE switch #-}

sumToEither :: Sum (Of a) (Of b) r ->  Of (Either a b) r
sumToEither s = case s of
  InL (a :> r) -> Left a :> r
  InR (b :> r) -> Right b :> r
{-# INLINE sumToEither #-}

eitherToSum :: Of (Either a b) r -> Sum (Of a) (Of b) r
eitherToSum s = case s of
  Left a :> r  -> InL (a :> r)
  Right b :> r -> InR (b :> r)
{-# INLINE eitherToSum #-}

composeToSum ::  Compose (Of Bool) f r -> Sum f f r
composeToSum x = case x of
  Compose (True :> f) -> InR f
  Compose (False :> f) -> InL f
{-# INLINE composeToSum #-}

sumToCompose :: Sum f f r -> Compose (Of Bool) f r
sumToCompose x = case x of
  InR f -> Compose (True :> f)
  InL f -> Compose (False :> f)
{-# INLINE sumToCompose #-}

{-| Given a stream on a sum of functors, make it a stream on the left functor,
    with the streaming on the other functor as the governing monad. This is
    useful for acting on one or the other functor with a fold, leaving the
    other material for another treatment. It generalizes
    'Data.Either.partitionEithers', but actually streams properly.

>>> let odd_even = S.maps (S.distinguish even) $ S.each' [1..10::Int]
>>> :t separate odd_even
separate odd_even
  :: Monad m => Stream (Of Int) (Stream (Of Int) m) ()

    Now, for example, it is convenient to fold on the left and right values separately:

>>> S.toList $ S.toList $ separate odd_even
[2,4,6,8,10] :> ([1,3,5,7,9] :> ())


   Or we can write them to separate files or whatever.

   Of course, in the special case of @Stream (Of a) m r@, we can achieve the above
   effects more simply by using 'Streaming.Prelude.copy'

>>> S.toList . S.filter even $ S.toList . S.filter odd $ S.copy $ each' [1..10::Int]
[2,4,6,8,10] :> ([1,3,5,7,9] :> ())


    But 'separate' and 'unseparate' are functor-general.

-}
separate :: forall m f g r.
  (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream (Sum f g) m r -> Stream f (Stream g m) r
separate stream = destroyExposed stream fromSum (Effect . Control.lift) Return
  where
    fromSum :: Sum f g (Stream f (Stream g m) r) #-> (Stream f (Stream g m) r)
    fromSum x = x & \case
      InL fss -> Step fss
      InR gss -> Effect (Step $ Control.fmap Return gss)
{-# INLINABLE separate #-}

unseparate :: (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream f (Stream g m) r -> Stream (Sum f g) m r
unseparate stream =
  destroyExposed stream (Step . InL) (Control.join . maps InR) return
 where
    Builder{..} = monadBuilder
{-# INLINABLE unseparate #-}

-- # Partitions
-------------------------------------------------------------------------------

{-|
> filter p = hoist effects (partition p)

 -}
partition :: forall a m r. Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Of a) (Stream (Of a) m) r
partition pred = loop
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Of a) (Stream (Of a) m) r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect (Control.fmap loop (Control.lift m))
      Step (a :> as) -> case pred a of
        True -> Step (a :> loop as)
        False -> Effect $ Step $ a :> (Return (loop as))

{-| Separate left and right values in distinct streams. ('separate' is
    a more powerful, functor-general, equivalent using 'Sum' in place of 'Either').

> partitionEithers = separate . maps S.eitherToSum
> lefts  = hoist S.effects . partitionEithers
> rights = S.effects . partitionEithers
> rights = S.concat

-}
partitionEithers :: Control.Monad m =>
  Stream (Of (Either a b)) m r #-> Stream (Of a) (Stream (Of b) m) r
partitionEithers = loop
  where
    Builder{..} = monadBuilder
    loop :: Control.Monad m =>
      Stream (Of (Either a b)) m r #-> Stream (Of a) (Stream (Of b) m) r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop (Control.lift m)
      Step (Left a :> as) -> Step (a :> loop as)
      Step (Right b :> as) -> Effect $ (Step $ b :> Return (loop as))


-- # Maybes
-------------------------------------------------------------------------------

{-| The 'catMaybes' function takes a 'Stream' of 'Maybe's and returns
    a 'Stream' of all of the 'Just' values. 'concat' has the same behavior,
    but is more general; it works for any foldable container type.
-}
catMaybes :: Control.Monad m => Stream (Of (Maybe a)) m r #-> Stream (Of a) m r
catMaybes stream = loop stream
  where
    Builder{..} = monadBuilder
    loop :: Control.Monad m => Stream (Of (Maybe a)) m r #-> Stream (Of a) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap catMaybes m
      Step (maybe :> as) -> case maybe of
        Nothing -> catMaybes as
        Just a -> Step $ a :> (catMaybes as)
{-# INLINABLE catMaybes #-}

{-| The 'mapMaybe' function is a version of 'map' which can throw out elements. In particular,
    the functional argument returns something of type @'Maybe' b@. If this is 'Nothing', no element
    is added on to the result 'Stream'. If it is @'Just' b@, then @b@ is included in the result 'Stream'.

-}
mapMaybe :: forall a b m r. Control.Monad m =>
  (a -> Maybe b) -> Stream (Of a) m r #-> Stream (Of b) m r
mapMaybe f stream = loop stream
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Of b) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect ms -> Effect $ ms >>= (return . mapMaybe f)
      Step (a :> s) -> case f a of
        Just b -> Step $ b :> (mapMaybe f s)
        Nothing -> mapMaybe f s
{-# INLINABLE mapMaybe #-}

-- Note: the first function needs to wrap the 'b' in an 'Unrestricted'
-- since the control monad is bound and the 'b' ends up in the first
-- unrestricted spot of 'Of'.
--
-- | Map monadically over a stream, producing a new stream
--   only containing the 'Just' values.
mapMaybeM :: forall a m b r. Control.Monad m =>
  (a -> m (Maybe (Unrestricted b))) -> Stream (Of a) m r #-> Stream (Of b) m r
mapMaybeM f stream = loop stream
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Of b) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap (mapMaybeM f) m
      Step (a :> as) -> Effect $ do
        mb <- f a
        mb & \case
          Nothing -> return $ mapMaybeM f as
          Just (Unrestricted b) -> return $ Step (b :> mapMaybeM f as)
{-# INLINABLE mapMaybeM #-}

-- # Direct Transformations
-------------------------------------------------------------------------------

{-| Change the effects of one monad to another with a transformation.
    This is one of the fundamental transformations on streams.
    Compare with 'maps':

> maps  :: (Control.Monad m, Control.Functor f) => (forall x. f x #-> g x) -> Stream f m r #-> Stream g m r
> hoist :: (Control.Monad m, Control.Functor f) => (forall a. m a #-> n a) -> Stream f m r #-> Stream f n r

-}
hoist :: forall f m n r. (Control.Monad m, Control.Functor f) =>
  (forall a. m a #-> n a) ->
  Stream f m r #-> Stream f n r
hoist f stream = loop stream where
  loop :: Stream f m r #-> Stream f n r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ f $ Control.fmap loop m
    Step f -> Step $ Control.fmap loop f
{-# INLINABLE hoist #-}

{-| Standard map on the elements of a stream.

>>> S.stdoutLn $ S.map reverse $ each' (words "alpha beta")
ahpla
ateb
-}
map :: Control.Monad m => (a -> b) -> Stream (Of a) m r #-> Stream (Of b) m r
map f = maps (\(x :> rest) -> f x :> rest)
{-# INLINABLE map #-}

-- Remark.
--
-- The functor transformation in functions like maps, mapped, mapsPost,
-- and such must be linear since the 'Stream' data type holds each 
-- functor step with a linear arrow.

{- | Map layers of one functor to another with a transformation. Compare
     hoist, which has a similar effect on the 'monadic' parameter.

> maps id = id
> maps f . maps g = maps (f . g)

-}
maps :: forall f g m r . (Control.Monad m, Control.Functor f) =>
  (forall x . f x #-> g x) -> Stream f m r #-> Stream g m r
maps phi = loop
  where
    loop :: Stream f m r #-> Stream g m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap (maps phi) m
      Step f -> Step (phi (Control.fmap loop f))
{-# INLINABLE maps #-}

-- Remark: Since the mapping function puts its result in a control monad,
-- it must be used exactly once after the monadic value is bound.
-- As a result the mapping function needs to return an 'Unrestricted b'
-- so that we can place the 'b' in the first argument of the
-- 'Of' constructor, which is unrestricted.
--
{-| Replace each element of a stream with the result of a monadic action

>>> S.print $ S.mapM readIORef $ S.chain (\ior -> modifyIORef ior (*100)) $ S.mapM newIORef $ each' [1..6]
100
200
300
400
500
600

See also 'chain' for a variant of this which ignores the return value of the function and just uses the side effects.
-}
mapM :: Control.Monad m =>
  (a -> m (Unrestricted b)) -> Stream (Of a) m r #-> Stream (Of b) m r
mapM f s = loop f s
  where
    Builder{..} = monadBuilder
    loop :: Control.Monad m =>
      (a -> m (Unrestricted b)) -> Stream (Of a) m r #-> Stream (Of b) m r
    loop f stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap (loop f) m
      Step (a :> as) -> Effect $ do
        Unrestricted b <- f a
        return $ Step (b :> (loop f as))
{-# INLINABLE mapM #-}

{- | Map layers of one functor to another with a transformation. Compare
     hoist, which has a similar effect on the 'monadic' parameter.

> mapsPost id = id
> mapsPost f . mapsPost g = mapsPost (f . g)
> mapsPost f = maps f

     @mapsPost@ is essentially the same as 'maps', but it imposes a @Control.Functor@ constraint on
     its target functor rather than its source functor. It should be preferred if 'fmap'
     is cheaper for the target functor than for the source functor.
-}
mapsPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> g x) -> Stream f m r #-> Stream g m r
mapsPost phi = loop
  where
    loop :: Stream f m r #-> Stream g m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop m
      Step f -> Step $ Control.fmap loop $ phi f
{-# INLINABLE mapsPost #-}

{- | Map layers of one functor to another with a transformation involving the base monad.
 
     This function is completely functor-general. It is often useful with the more concrete type

@
mapped :: (forall x. Stream (Of a) IO x -> IO (Of b x)) -> Stream (Stream (Of a) IO) IO r -> Stream (Of b) IO r
@

     to process groups which have been demarcated in an effectful, @IO@-based
     stream by grouping functions like 'Streaming.Prelude.group',
     'Streaming.Prelude.split' or 'Streaming.Prelude.breaks'. Summary functions
     like 'Streaming.Prelude.fold', 'Streaming.Prelude.foldM',
     'Streaming.Prelude.mconcat' or 'Streaming.Prelude.toList' are often used
     to define the transformation argument. For example:

>>> S.toList_ $ S.mapped S.toList $ S.split 'c' (S.each' "abcde")
["ab","de"]

     'Streaming.Prelude.maps' and 'Streaming.Prelude.mapped' obey these rules:

> maps id              = id
> mapped return        = id
> maps f . maps g      = maps (f . g)
> mapped f . mapped g  = mapped (f <=< g)
> maps f . mapped g    = mapped (fmap f . g)
> mapped f . maps g    = mapped (f <=< fmap g)

     where @f@ and @g@ are @Control.Monad@s

     'Streaming.Prelude.maps' is more fundamental than
     'Streaming.Prelude.mapped', which is best understood as a convenience for
     effecting this frequent composition:

> mapped phi = decompose . maps (Compose . phi)


-}
mapped :: forall f g m r . (Control.Monad m, Control.Functor f) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mapped phi = loop
  where
  loop :: Stream f m r #-> Stream g m r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap loop m
    Step f -> Effect $ Control.fmap Step $ phi $ Control.fmap loop f

{- | Map layers of one functor to another with a transformation involving the base monad.
     @mapsMPost@ is essentially the same as 'mapsM', but it imposes a @Control.Functor@ constraint on
     its target functor rather than its source functor. It should be preferred if 'fmap'
     is cheaper for the target functor than for the source functor.

     @mapsPost@ is more fundamental than @mapsMPost@, which is best understood as a convenience
     for effecting this frequent composition:

> mapsMPost phi = decompose . mapsPost (Compose . phi)

     The streaming prelude exports the same function under the better name @mappedPost@,
     which overlaps with the lens libraries.

-}
{-# INLINABLE mapped #-}

mapsMPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mapsMPost phi = loop
  where
  loop :: Stream f m r #-> Stream g m r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap loop m
    Step f -> Effect $ Control.fmap (Step . Control.fmap loop) $ phi f
{-# INLINABLE mapsMPost #-}

{-| A version of 'mapped' that imposes a @Control.Functor@ constraint on the target functor rather
    than the source functor. This version should be preferred if 'fmap' on the target
    functor is cheaper.

-}
mappedPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mappedPost phi = loop
  where
  loop :: Stream f m r #-> Stream g m r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap loop m
    Step f -> Effect $ Control.fmap (Step . Control.fmap loop) $ phi f
{-# INLINABLE mappedPost #-}

-- | @for@ replaces each element of a stream with an associated stream. Note that the
-- associated stream may layer any control functor.
for :: forall f m r a x . (Control.Monad m, Control.Functor f, Consumable x) =>
  Stream (Of a) m r #-> (a -> Stream f m x) -> Stream f m r
for stream expand = loop stream
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream f m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop m
      Step (a :> as) -> do
         x <- expand a
         lseq x $ loop as
{-# INLINABLE for #-}

-- Note: since the 'x' is discarded inside a control functor,
-- we need it to be consumable
--
{-| Replace each element in a stream of individual Haskell values (a @Stream (Of a) m r@) with an associated 'functorial' step.

> for str f  = concats (with str f)
> with str f = for str (yields . f)
> with str f = maps (\(a:>r) -> r <$ f a) str
> with = flip subst
> subst = flip with

>>> with (each' [1..3]) (yield . Prelude.show) & intercalates (yield "--") & S.stdoutLn
1
--
2
--
3
 -}
with :: forall f m r a x . (Control.Monad m, Control.Functor f, Consumable x) =>
  Stream (Of a) m r #-> (a -> f x) -> Stream f m r
with s f = loop s
  where
    loop :: Stream (Of a) m r #-> Stream f m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop m
      Step (a :> as) -> Step $ Control.fmap (`lseq` (loop as)) (f a)
{-# INLINABLE with #-}

{-| Replace each element in a stream of individual values with a functorial
    layer of any sort. @subst = flip with@ and is more convenient in
    a sequence of compositions that transform a stream.

> with = flip subst
> for str f = concats $ subst f str
> subst f = maps (\(a:>r) -> r <$ f a)
> S.concat = concats . subst each

-}
subst :: (Control.Monad m, Control.Functor f, Consumable x) =>
  (a -> f x) -> Stream (Of a) m r #-> Stream f m r
subst = flip with where
  flip :: (a #-> b -> c) -> b -> a #-> c
  flip f b a = f a b
{-# INLINE subst #-}

{-| Duplicate the content of a stream, so that it can be acted on twice in different ways,
    but without breaking streaming. Thus, with @each' [1,2]@ I might do:

>>> S.print $ each' ["one","two"]
"one"
"two"
>>> S.stdoutLn $ each' ["one","two"]
one
two

    With copy, I can do these simultaneously:

>>> S.print $ S.stdoutLn $ S.copy $ each' ["one","two"]
"one"
one
"two"
two

    'copy' should be understood together with 'effects' and is subject to the rules

> S.effects . S.copy       = id
> hoist S.effects . S.copy = id

    The similar operations in 'Data.ByteString.Streaming' obey the same rules.

    Where the actions you are contemplating are each simple folds over
    the elements, or a selection of elements, then the coupling of the
    folds is often more straightforwardly effected with `Control.Foldl`,
    e.g.

>>> L.purely S.fold (liftA2 (,) L.sum L.product) $ each' [1..10]
(55,3628800) :> ()

    rather than

>>> S.sum $ S.product . S.copy $ each' [1..10]
55 :> (3628800 :> ())

    A @Control.Foldl@ fold can be altered to act on a selection of elements by
    using 'Control.Foldl.handles' on an appropriate lens. Some such
    manipulations are simpler and more 'Data.List'-like, using 'copy':

>>> L.purely S.fold (liftA2 (,) (L.handles (L.filtered odd) L.sum) (L.handles (L.filtered even) L.product)) $ each' [1..10]
(25,3840) :> ()

     becomes

>>> S.sum $ S.filter odd $ S.product $ S.filter even $ S.copy' $ each' [1..10]
25 :> (3840 :> ())

    or using 'store'

>>> S.sum $ S.filter odd $ S.store (S.product . S.filter even) $ each' [1..10]
25 :> (3840 :> ())

    But anything that fold of a @Stream (Of a) m r@ into e.g. an @m (Of b r)@
    that has a constraint on @m@ that is carried over into @Stream f m@ -
    e.g. @Control.Monad@, @Control.Functor@, etc. can be used on the stream.
    Thus, I can fold over different groupings of the original stream:

>>>  (S.toList . mapped S.toList . chunksOf 5) $  (S.toList . mapped S.toList . chunksOf 3) $ S.copy $ each' [1..10]
[[1,2,3,4,5],[6,7,8,9,10]] :> ([[1,2,3],[4,5,6],[7,8,9],[10]] :> ())

    The procedure can be iterated as one pleases, as one can see from this (otherwise unadvisable!) example:

>>>  (S.toList . mapped S.toList . chunksOf 4) $ (S.toList . mapped S.toList . chunksOf 3) $ S.copy $ (S.toList . mapped S.toList . chunksOf 2) $ S.copy $ each' [1..12]
[[1,2,3,4],[5,6,7,8],[9,10,11,12]] :> ([[1,2,3],[4,5,6],[7,8,9],[10,11,12]] :> ([[1,2],[3,4],[5,6],[7,8],[9,10],[11,12]] :> ()))


@copy@ can be considered a special case of 'expand':

@
  copy = 'expand' $ \p (a :> as) -> a :> p (a :> as)
@

If 'Of' were an instance of 'Control.Comonad.Comonad', then one could write

@
  copy = 'expand' extend
@
-}
copy :: forall a m r . Control.Monad m =>
     Stream (Of a) m r #-> Stream (Of a) (Stream (Of a) m) r
copy = Effect . return . loop
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Of a) (Stream (Of a) m) r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop (Control.lift m)
      Step (a :> as) -> Effect $ Step (a :> Return (Step (a :> loop as)))
{-# INLINABLE copy#-}

{-| An alias for @copy@.
-}
duplicate :: forall a m r . Control.Monad m =>
     Stream (Of a) m r #-> Stream (Of a) (Stream (Of a) m) r
duplicate = copy
{-# INLINE duplicate#-}


-- Note: to use the stream linearly the first argument
-- must be a linear function
--
{-| Store the result of any suitable fold over a stream, keeping the stream for
    further manipulation. @store f = f . copy@ :

>>> S.print $ S.store S.product $ each' [1..4]
1
2
3
4
24 :> ()

>>> S.print $ S.store S.sum $ S.store S.product $ each' [1..4]
1
2
3
4
10 :> (24 :> ())

   Here the sum (10) and the product (24) have been \'stored\' for use when
   finally we have traversed the stream with 'print' . Needless to say,
   a second 'pass' is excluded conceptually, so the
   folds that you apply successively with @store@ are performed
   simultaneously, and in constant memory -- as they would be if,
   say, you linked them together with @Control.Fold@:

>>> L.impurely S.foldM (liftA3 (\a b c -> (b, c)) (L.sink Prelude.print) (L.generalize L.sum) (L.generalize L.product)) $ each' [1..4]
1
2
3
4
(10,24) :> ()

   Fusing folds after the fashion of @Control.Foldl@ will generally be a bit faster
   than the corresponding succession of uses of 'store', but by
   constant factor that will be completely dwarfed when any IO is at issue.

   But 'store' \/ 'copy' is /much/ more powerful, as you can see by reflecting on
   uses like this:

>>> S.sum $ S.store (S.sum . mapped S.product . chunksOf 2) $ S.store (S.product . mapped S.sum . chunksOf 2) $ each' [1..6]
21 :> (44 :> (231 :> ()))

   It will be clear that this cannot be reproduced with any combination of lenses,
   @Control.Fold@ folds, or the like.  (See also the discussion of 'copy'.)

   It would conceivably be clearer to import a series of specializations of 'store'.
   It is intended to be used at types like this:

> storeM ::  (forall s m . Control.Monad m => Stream (Of a) m s #-> m (Of b s))
>         -> (Control.Monad n => Stream (Of a) n r #-> Stream (Of a) n (Of b r))
> storeM = store

    It is clear from this type that we are just using the general instance:

> instance (Control.Functor f, Control.Monad m)   => Control.Monad (Stream f m)

    We thus can't be touching the elements of the stream, or the final return value.
    It is the same with other constraints that @Stream (Of a)@ inherits from the underlying monad.
    Thus I can independently filter and write to one file, but
    nub and write to another, or interact with a database and a logfile and the like:

>>> (S.writeFile "hello2.txt" . S.nubOrd) $ store (S.writeFile "hello.txt" . S.filter (/= "world")) $ each' ["hello", "world", "goodbye", "world"]
>>> :! cat hello.txt
hello
goodbye
>>> :! cat hello2.txt
hello
world
goodbye


-}
store :: Control.Monad m =>
  (Stream (Of a) (Stream (Of a) m) r #-> t) -> Stream (Of a) m r #-> t
store f x = f (copy x)
{-# INLINE store #-}

-- Note: since we discard the 'y' inside a control monad, it needs to be
-- consumable
--
{-| Apply an action to all values, re-yielding each.
    The return value (@y@) of the function is ignored.

>>> S.product $ S.chain Prelude.print $ S.each' [1..5]
1
2
3
4
5
120 :> ()

See also 'mapM' for a variant of this which uses the return value of the function to transorm the values in the stream.
-}
chain :: forall a m r y . (Control.Monad m, Consumable y) =>
  (a -> m y) -> Stream (Of a) m r #-> Stream (Of a) m r
chain f = loop
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Of a) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m  -> Effect $ Control.fmap loop m
      Step (a :> as) -> Effect $ do
        y <- f a
        return $ lseq y $ Step (a :> loop as)
{-# INLINABLE chain #-}

-- Note: since the value of type 'a' is inside a control monad but
-- needs to be used in an unrestricted position in 'Of', the input stream
-- needs to hold values of type 'm (Unrestricted a)'.
--
{-| Like the 'Data.List.sequence' but streaming. The result type is a
    stream of a\'s, /but is not accumulated/; the effects of the elements
    of the original stream are interleaved in the resulting stream. Compare:

> sequence :: Monad m =>         [m a]                 ->  m [a]
> sequence :: Control.Monad m => Stream (Of (m a)) m r #-> Stream (Of a) m r

-}
sequence :: forall a m r . Control.Monad m =>
  Stream (Of (m (Unrestricted a))) m r #-> Stream (Of a) m r
sequence = loop
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of (m (Unrestricted a))) m r #-> Stream (Of a) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop m
      Step (ma :> mas) -> Effect $ do
        Unrestricted a <- ma
        return $ Step (a :> loop mas)
{-# INLINABLE sequence #-}

{-| Remove repeated elements from a Stream. 'nubOrd' of course accumulates a 'Data.Set.Set' of
    elements that have already been seen and should thus be used with care.

-}
nubOrd :: (Control.Monad m, Ord a) => Stream (Of a) m r #-> Stream (Of a) m r
nubOrd = nubOrdOn id
{-# INLINE nubOrd #-}

{-|  Use 'nubOrdOn' to have a custom ordering function for your elements. -}
nubOrdOn :: forall m a b r . (Control.Monad m, Ord b) =>
  (a -> b) -> Stream (Of a) m r #-> Stream (Of a) m r
nubOrdOn f xs = loop Set.empty xs
  where
  loop :: Set.Set b -> Stream (Of a) m r #-> Stream (Of a) m r
  loop !set stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap (loop set) m
    Step (a :> as) -> case Set.member (f a) set of
         True -> loop set as
         False-> Step (a :> loop (Set.insert (f a) set) as)

{-| More efficient versions of above when working with 'Int's that use 'Data.IntSet.IntSet'. -}
nubInt :: Control.Monad m => Stream (Of Int) m r #-> Stream (Of Int) m r
nubInt = nubIntOn id
{-# INLINE nubInt #-}

nubIntOn :: forall m a r . (Control.Monad m) =>
  (a -> Int) -> Stream (Of a) m r #-> Stream (Of a) m r
nubIntOn f xs = loop IntSet.empty xs
  where
  loop :: IntSet.IntSet -> Stream (Of a) m r #-> Stream (Of a) m r
  loop !set stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap (loop set) m
    Step (a :> as) -> case IntSet.member (f a) set of
         True -> loop set as
         False-> Step (a :> loop (IntSet.insert (f a) set) as)

-- | Skip elements of a stream that fail a predicate
filter  :: forall a m r . Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Of a) m r
filter pred = loop
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Of a) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop m
      Step (a :> as) -> case pred a of
        True -> Step (a :> loop as)
        False -> loop as
{-# INLINE filter #-}

-- | Skip elements of a stream that fail a monadic test
filterM  :: forall a m r . Control.Monad m =>
  (a -> m Bool) -> Stream (Of a) m r #-> Stream (Of a) m r
filterM pred = loop
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Of a) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m-> Effect $ Control.fmap loop m
      Step (a :> as) -> Effect $ do
        bool <- pred a
        bool & \case
          True -> return $ Step (a :> loop as)
          False -> return $ loop as
{-# INLINE filterM #-}

{-| Intersperse given value between each element of the stream.

>>> S.print $ S.intersperse 0 $ each [1,2,3]
1
0
2
0
3

-}
intersperse :: forall a m r . Control.Monad m =>
  a -> Stream (Of a) m r #-> Stream (Of a) m r
intersperse x stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap (intersperse x) m
    Step (a :> as) -> loop a as
  where
    -- Given the first element of a stream, intersperse the bound
    -- element named 'x'
    loop :: a -> Stream (Of a) m r #-> Stream (Of a) m r
    loop !a stream = stream & \case
      Return r -> Step (a :> Return r)
      Effect m -> Effect $ Control.fmap (loop a) m
      Step (a' :> as) -> Step (a :> Step (x :> loop a' as))
{-# INLINABLE intersperse #-}

{-|  Ignore the first n elements of a stream, but carry out the actions

>>> S.toList $ S.drop 2 $ S.replicateM 5 getLine
a<Enter>
b<Enter>
c<Enter>
d<Enter>
e<Enter>
["c","d","e"] :> ()

     Because it retains the final return value, @drop n@  is a suitable argument
     for @maps@:

>>> S.toList $ concats $ maps (S.drop 4) $ chunksOf 5 $ each [1..20]
[5,10,15,20] :> ()
  -}
drop :: forall a m r. (HasCallStack, Control.Monad m) =>
  Int -> Stream (Of a) m r #-> Stream (Of a) m r
drop n stream = case compare n 0 of
  LT -> Prelude.error "drop called with negative int" $ stream
  EQ -> stream
  GT -> loop stream where
    loop :: Stream (Of a) m r #-> Stream (Of a) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap (drop n) m
      Step (_ :> as) -> drop (n-1) as
{-# INLINABLE drop #-}

{- | Ignore elements of a stream until a test succeeds, retaining the rest.

>>> S.print $ S.dropWhile ((< 5) . length) S.stdinLn
one<Enter>
two<Enter>
three<Enter>
"three"
four<Enter>
"four"
^CInterrupted.


-}
dropWhile :: forall a m r . Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Of a) m r
dropWhile pred = loop
  where
    loop :: Stream (Of a) m r #-> Stream (Of a) m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop m
      Step (a :> as) -> case pred a of
        True -> loop as
        False -> Step (a :> as)
{-# INLINABLE dropWhile #-}

{-| Strict left scan, streaming, e.g. successive partial results. The seed
    is yielded first, before any action of finding the next element is performed.


>>> S.print $ S.scan (++) "" id $ each' (words "a b c d")
""
"a"
"ab"
"abc"
"abcd"

    'scan' is fitted for use with @Control.Foldl@, thus:

>>> S.print $ L.purely S.scan L.list $ each' [3..5]
[]
[3]
[3,4]
[3,4,5]

-}
scan :: forall a x b m r . Control.Monad m =>
  (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r #-> Stream (Of b) m r
scan step begin done stream = Step (done begin :> loop begin stream)
  where
    loop :: x -> Stream (Of a) m r #-> Stream (Of b) m r
    loop !acc stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap (loop acc) m
      Step (a :> as) -> Step (done acc' :> loop acc' as) where
        !acc' = step acc a
{-# INLINABLE scan #-}

-- Note: since the accumulated value (inside the control monad) is used both in
-- populating the output stream and in accumulation, it needs to be wrapped in
-- an 'Unrestricted' accross the function
--
{-| Strict left scan, accepting a monadic function. It can be used with
    'FoldM's from @Control.Foldl@ using 'impurely'. Here we yield
    a succession of vectors each recording

>>> let v = L.impurely scanM L.vectorM $ each' [1..4::Int] :: Stream (Of (Vector Int)) IO ()
>>> S.print v
[]
[1]
[1,2]
[1,2,3]
[1,2,3,4]

-}
scanM :: forall a x b m r . Control.Monad m =>
  (x #-> a -> m (Unrestricted x)) ->
  m (Unrestricted x) ->
  (x #-> m (Unrestricted b)) ->
  Stream (Of a) m r #->
  Stream (Of b) m r
scanM step mx done stream = loop stream
  where
    Builder{..} = monadBuilder
    loop :: Stream (Of a) m r #-> Stream (Of b) m r
    loop stream = stream & \case
      Return r -> Effect $ do
        Unrestricted x <- mx
        Unrestricted b <- done x
        return $ Step $ b :> Return r
      Effect m -> Effect $ Control.fmap (scanM step mx done) m
      Step (a :> as) -> Effect $ do
        Unrestricted x <- mx
        Unrestricted b <- done x
        return $ Step $ b :> (scanM step (step x a) done as)
{-# INLINABLE scanM #-}

{-| Label each element in a stream with a value accumulated according to a fold.

>>> S.print $ S.scanned (*) 1 id $ S.each' [100,200,300]
(100,100)
(200,20000)
(300,6000000)

>>> S.print $ L.purely S.scanned' L.product $ S.each [100,200,300]
(100,100)
(200,20000)
(300,6000000)

-}
scanned :: forall a x b m r . Control.Monad m =>
  (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r #-> Stream (Of (a,b)) m r
scanned step begin done = loop begin
  where
    Builder{..} = monadBuilder
    loop :: x -> Stream (Of a) m r #-> Stream (Of (a,b)) m r
    loop !x stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap (loop x) m
      Step (a :> as) -> do
        let !acc = done (step x a)
        Step $ (a, acc) :> Return () -- same as yield
        loop (step x a) as
{-# INLINABLE scanned #-}

-- Note: this skips failed parses
-- XXX re-write with Text
--
{- | Make a stream of strings into a stream of parsed values, skipping bad cases

>>> S.sum_ $ S.read $ S.takeWhile (/= "total") S.stdinLn :: IO Int
1000<Enter>
2000<Enter>
total<Enter>
3000


-}
read :: (Control.Monad m, Read a) =>
  Stream (Of String) m r #-> Stream (Of a) m r
read = mapMaybe readMaybe
{-# INLINE read #-}

{-| Interpolate a delay of n seconds between yields.
-}
delay :: forall a r. Double -> Stream (Of a) IO r #-> Stream (Of a) IO r
delay seconds = loop
  where
    Builder{..} = monadBuilder
    pico = Prelude.truncate (seconds * 1000000)
    loop :: Stream (Of a) IO r #-> Stream (Of a) IO r
    loop stream = do
      e <- Control.lift $ next stream
      e & \case
        Left r -> Return r
        Right (Unrestricted a,rest) -> do
          Step (a :> Return ()) -- same as yield
          Control.lift $ fromSystemIO $ threadDelay pico
          loop rest
{-# INLINABLE delay #-}

show :: (Control.Monad m, Prelude.Show a) =>
  Stream (Of a) m r #-> Stream (Of String) m r
show = map Prelude.show
{-# INLINE show #-}


{-| The natural @cons@ for a @Stream (Of a)@.

> cons a stream = yield a Control.>> stream

   Useful for interoperation:

> Data.Text.foldr S.cons (return ()) :: Text -> Stream (Of Char) m ()
> Lazy.foldrChunks S.cons (return ()) :: Lazy.ByteString -> Stream (Of Strict.ByteString) m ()

    and so on.
-}
cons :: Control.Monad m => a -> Stream (Of a) m r #-> Stream (Of a) m r
cons a str = Step (a :> str)
{-# INLINE cons #-}

-- Note. The action function that is the second argument must be linear since
-- it gets its argument from binding to the first argument, which uses a 
-- control monad.
--
{-| Before evaluating the monadic action returning the next step in the 'Stream', @wrapEffect@
    extracts the value in a monadic computation @m a@ and passes it to a computation @a -> m y@.

-}
wrapEffect :: (Control.Monad m, Control.Functor f, Consumable y) =>
  m a -> (a #-> m y) -> Stream f m r #-> Stream f m r
wrapEffect ma action stream = stream & \case
  Return r -> Return r
  Effect m -> Effect $ do
    a <- ma
    y <- action a
    lseq y $ m
  Step f -> Effect $ do
    a <- ma
    y <- action a
    return $ lseq y $ Step f
  where
    Builder{..} = monadBuilder

{-| 'slidingWindow' accumulates the first @n@ elements of a stream,
     update thereafter to form a sliding window of length @n@.
     It follows the behavior of the slidingWindow function in
     <https://hackage.haskell.org/package/conduit-combinators-1.0.4/docs/Data-Conduit-Combinators.html#v:slidingWindow conduit-combinators>.

>>> S.print $ S.slidingWindow 4 $ S.each "123456"
fromList "1234"
fromList "2345"
fromList "3456"

-}
slidingWindow :: forall a b m. Control.Monad m => Int -> Stream (Of a) m b
              #-> Stream (Of (Seq.Seq a)) m b
slidingWindow n = setup (max 1 n :: Int) Seq.empty
  where
    Builder{..} = monadBuilder
    -- Given the current sliding window, yield it and then recurse with
    -- updated sliding window
    window :: Seq.Seq a -> Stream (Of a) m b #-> Stream (Of (Seq.Seq a)) m b
    window !sequ str = do
      e <- Control.lift (next str)
      e & \case
        Left r -> return r
        Right (Unrestricted a,rest) -> do
          Step $ (sequ Seq.|> a) :> Return () -- same as yield
          window (Seq.drop 1 sequ Seq.|> a) rest
    -- Collect the first n elements in a sequence and call 'window'
    setup ::
      Int -> Seq.Seq a -> Stream (Of a) m b #-> Stream (Of (Seq.Seq a)) m b
    setup 0 !sequ str = do
       Step (sequ :> Return ()) -- same as yield
       window (Seq.drop 1 sequ) str
    setup n' sequ str = do
      e <- Control.lift $ next str
      e & \case
        Left r -> do
          Step (sequ :> Return ()) -- same as yield
          return r
        Right (Unrestricted x,rest) -> setup (n'-1) (sequ Seq.|> x) rest
{-# INLINABLE slidingWindow #-}

