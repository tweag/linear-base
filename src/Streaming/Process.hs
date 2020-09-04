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
module Streaming.Process
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

import Streaming.Type
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

-- | Remark. Since the 'a' is not held linearly in the 'Of' pair,
-- we return it inside an 'Unrestricted'.
next :: Control.Monad m =>
  Stream (Of a) m r #-> m (Either r (Unrestricted a, Stream (Of a) m r))
next stream = stream & \case
  Return r -> return $ Left r
  Effect ms -> ms >>= next
  Step (a :> as) -> return $ Right (Unrestricted a, as)
  where
    Builder{..} = monadBuilder

uncons :: (Consumable r, Control.Monad m) =>
  Stream (Of a) m r #-> m (Maybe (a, Stream (Of a) m r))
uncons  stream = stream & \case
  Return r -> lseq r $ return Nothing
  Effect ms -> ms >>= uncons
  Step (a :> as) -> return $ Just (a, as)
  where
    Builder{..} = monadBuilder

splitAt :: (HasCallStack, Control.Monad m, Control.Functor f) =>
  Int -> Stream f m r #-> Stream f m (Stream f m r)
splitAt n stream = Prelude.compare n 0 & \case
  LT -> Prelude.error "splitAt called with negative integer" $ stream
  EQ -> Return stream
  GT -> stream & \case
    Return r -> Prelude.error "splitAt called with too large index" $ r
    Effect m -> Effect $ m >>= (return . splitAt n)
    Step f -> Step $ Control.fmap (splitAt (n-1)) f
  where
    Builder{..} = monadBuilder

split :: (Eq a, Control.Monad m) =>
  a -> Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
split x stream = stream & \case
  Return r -> Return r
  Effect m -> Effect $ m >>= (return . split x)
  Step (a :> as) -> case a == x of
    True -> split x as
    False -> consFirstChunk a (split x as)
  where
    Builder{..} = monadBuilder

break :: Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)
break f stream = stream & \case
  Return r -> Return (Return r)
  Effect m -> Effect $ Control.fmap (break f) m
  Step (a :> as) -> case f a of
    True -> Return $ Step (a :> as)
    False -> Step (a :> (break f as))
  where
    Builder{..} = monadBuilder

-- | Elements that fail the predicate are grouped, and elements that
-- pass the predicate are discarded
breaks :: Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
breaks f stream = stream & \case
  Return r -> Return r
  Effect m -> Effect $ Control.fmap (breaks f) m
  Step (a :> as) -> case f a of
    True -> breaks f as
    False -> consFirstChunk a (breaks f as)
  where
    Builder{..} = monadBuilder

-- Remark. The funny type of this seems to be made to interoperate well with
-- `purely` from the `foldl` package.
breakWhen :: Control.Monad m => (x -> a -> x) -> x -> (x -> b) -> (b -> Bool)
          -> Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)
breakWhen step x end pred stream = stream & \case
  Return r -> Return (Return r)
  Effect m -> Effect $ Control.fmap (breakWhen step x end pred) m
  Step (a :> as) -> case pred (end (step x a)) of
    False -> Step $ a :> (breakWhen step (step x a) end pred as)
    True -> Return (Step (a :> as))

breakWhen' :: Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)
breakWhen' f stream = breakWhen (\x a -> f a) True id id stream

span :: Control.Monad m =>
  (a -> Bool) -> Stream (Of a) m r #-> Stream (Of a) m (Stream (Of a) m r)
span f = break (Prelude.not Prelude.. f)

groupBy :: Control.Monad m =>
  (a -> a -> Bool) -> Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
groupBy equals stream = stream & \case
  Return r -> Return r
  Effect m -> Effect $ Control.fmap (groupBy equals) m
  Step (a :> as) -> as & \case
    Return r -> Step (Step (a :> Return (Return r)))
    Effect m -> Effect $ m >>= (\s -> return $ groupBy equals (Step (a :> s)))
    Step (a' :> as') -> case equals a a' of
      False -> Step $ Step $ a :> (Return $ groupBy equals (Step (a' :> as')))
      True -> Step $ Step $ a :> (Step $ a' :> (Return $ groupBy equals as'))
  where
    Builder{..} = monadBuilder

group :: (Control.Monad m, Eq a) =>
  Stream (Of a) m r #-> Stream (Stream (Of a) m) m r
group = groupBy (==)


-- # Sum and compose manipulation
-------------------------------------------------------------------------------

-- Remark. Most of these functions are general and were merely cut and pasted
-- from the original library.

distinguish :: (a -> Bool) -> Of a r -> Sum (Of a) (Of a) r
distinguish predicate (a :> b) = case predicate a of
  True -> InR (a :> b)
  False -> InL (a :> b)

switch :: Sum f g r -> Sum g f r
switch s = case s of InL a -> InR a; InR a -> InL a

sumToEither :: Sum (Of a) (Of b) r ->  Of (Either a b) r
sumToEither s = case s of
  InL (a :> r) -> Left a :> r
  InR (b :> r) -> Right b :> r

eitherToSum :: Of (Either a b) r -> Sum (Of a) (Of b) r
eitherToSum s = case s of
  Left a :> r  -> InL (a :> r)
  Right b :> r -> InR (b :> r)

composeToSum ::  Compose (Of Bool) f r -> Sum f f r
composeToSum x = case x of
  Compose (True :> f) -> InR f
  Compose (False :> f) -> InL f

sumToCompose :: Sum f f r -> Compose (Of Bool) f r
sumToCompose x = case x of
  InR f -> Compose (True :> f)
  InL f -> Compose (False :> f)

separate :: forall m f g r.
  (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream (Sum f g) m r -> Stream f (Stream g m) r
separate stream = destroyExposed stream fromSum (Effect . Control.lift) Return
  where
    fromSum :: Sum f g (Stream f (Stream g m) r) #-> (Stream f (Stream g m) r)
    fromSum x = x & \case
      InL fss -> Step fss
      InR gss -> Effect (Step $ Control.fmap Return gss)

unseparate :: (Control.Monad m, Control.Functor f, Control.Functor g) =>
  Stream f (Stream g m) r -> Stream (Sum f g) m r
unseparate stream =
  destroyExposed stream (Step . InL) (Control.join . maps InR) return
 where
    Builder{..} = monadBuilder


-- # Partitions
-------------------------------------------------------------------------------

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

catMaybes :: Control.Monad m => Stream (Of (Maybe a)) m r #-> Stream (Of a) m r
catMaybes stream = stream & \case
  Return r -> Return r
  Effect m -> Effect $ Control.fmap catMaybes m
  Step (maybe :> as) -> case maybe of
    Nothing -> catMaybes as
    Just a -> Step $ a :> (catMaybes as)
  where
    Builder{..} = monadBuilder

mapMaybe :: Control.Monad m =>
  (a -> Maybe b) -> Stream (Of a) m r #-> Stream (Of b) m r
mapMaybe f stream = stream & \case
  Return r -> Return r
  Effect ms -> Effect $ ms >>= (return . mapMaybe f)
  Step (a :> s) -> case f a of
    Just b -> Step $ b :> (mapMaybe f s)
    Nothing -> mapMaybe f s
  where
    Builder{..} = monadBuilder

-- Note: the first function needs to wrap the 'b' in an 'Unrestricted'
-- since the control monad is bound and the 'b' ends up in the first
-- unrestricted spot of 'Of'.
mapMaybeM :: Control.Monad m =>
  (a -> m (Maybe (Unrestricted b))) -> Stream (Of a) m r #-> Stream (Of b) m r
mapMaybeM f stream = stream & \case
  Return r -> Return r
  Effect m -> Effect $ Control.fmap (mapMaybeM f) m
  Step (a :> as) -> Effect $ do
    mb <- f a
    mb & \case
      Nothing -> return $ mapMaybeM f as
      Just (Unrestricted b) -> return $ Step (b :> mapMaybeM f as)
  where
    Builder{..} = monadBuilder


-- # Direct Transformations
-------------------------------------------------------------------------------

map :: Control.Monad m => (a -> b) -> Stream (Of a) m r #-> Stream (Of b) m r
map f stream = stream & \case
  Return r -> Return r
  Step (a :> rest) -> Step $ (f a) :> map f rest
  Effect ms -> Effect $ Control.fmap (map f) ms

-- Remark.
--
-- The functor transformation in functions like maps, mapped, mapsPost,
-- and such must be linear since the 'Stream' data type holds each 
-- functor step with a linear arrow.

maps :: forall f g m r . (Control.Monad m, Control.Functor f) =>
  (forall x . f x #-> g x) -> Stream f m r #-> Stream g m r
maps phi = loop
  where
    loop :: Stream f m r #-> Stream g m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap (maps phi) m
      Step f -> Step (phi (Control.fmap loop f))

-- Remark: Since the mapping function puts its result in a control monad,
-- it must be used exactly once after the monadic value is bound.
-- As a result the mapping function needs to return an 'Unrestricted b'
-- so that we can place the 'b' in the first argument of the
-- 'Of' constructor, which is unrestricted.
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

mapsPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> g x) -> Stream f m r #-> Stream g m r
mapsPost phi = loop
  where
    loop :: Stream f m r #-> Stream g m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop m
      Step f -> Step $ Control.fmap loop $ phi f

mapped :: forall f g m r . (Control.Monad m, Control.Functor f) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mapped phi = loop
  where
  loop :: Stream f m r #-> Stream g m r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap loop m
    Step f -> Effect $ Control.fmap Step $ phi $ Control.fmap loop f

mapsMPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mapsMPost phi = loop
  where
  loop :: Stream f m r #-> Stream g m r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap loop m
    Step f -> Effect $ Control.fmap (Step . Control.fmap loop) $ phi f

mappedPost :: forall m f g r. (Control.Monad m, Control.Functor g) =>
  (forall x. f x #-> m (g x)) -> Stream f m r #-> Stream g m r
mappedPost phi = loop
  where
  loop :: Stream f m r #-> Stream g m r
  loop stream = stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap loop m
    Step f -> Effect $ Control.fmap (Step . Control.fmap loop) $ phi f

for :: forall f m r a x . (Control.Monad m, Control.Functor f, Consumable x) =>
  Stream (Of a) m r #-> (a -> Stream f m x) -> Stream f m r
for stream expand = for' stream
  where
    Builder{..} = monadBuilder
    for' :: Stream (Of a) m r #-> Stream f m r
    for' stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap for' m
      Step (a :> as) -> do
         x <- expand a
         lseq x $ for' as

-- Note: since the 'x' is discarded inside a control functor,
-- we need it to be consumable
with :: forall f m r a x . (Control.Monad m, Control.Functor f, Consumable x) =>
  Stream (Of a) m r #-> (a -> f x) -> Stream f m r
with s f = loop s
  where
    loop :: Stream (Of a) m r #-> Stream f m r
    loop stream = stream & \case
      Return r -> Return r
      Effect m -> Effect $ Control.fmap loop m
      Step (a :> as) -> Step $ Control.fmap (`lseq` (loop as)) (f a)

subst :: (Control.Monad m, Control.Functor f, Consumable x) =>
  (a -> f x) -> Stream (Of a) m r #-> Stream f m r
subst = flip with where
  flip :: (a #-> b -> c) -> b -> a #-> c
  flip f b a = f a b

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

duplicate :: forall a m r . Control.Monad m =>
     Stream (Of a) m r #-> Stream (Of a) (Stream (Of a) m) r
duplicate = copy

-- Note: to use the stream linearly the first argument
-- must be a linear function
store :: Control.Monad m =>
  (Stream (Of a) (Stream (Of a) m) r #-> t) -> Stream (Of a) m r #-> t
store f x = f (copy x)

-- Note: since we discard the 'y' inside a control monad, it needs to be
-- consumable
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

-- Note: since the value of type 'a' is inside a control monad but
-- needs to be used in an unrestricted position in 'Of', the input stream
-- needs to hold values of type 'm (Unrestricted a)'.
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


nubOrd :: (Control.Monad m, Ord a) => Stream (Of a) m r #-> Stream (Of a) m r
nubOrd = nubOrdOn id

-- XXX Could improve with linear mutable sets
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

nubInt :: Control.Monad m => Stream (Of Int) m r #-> Stream (Of Int) m r
nubInt = nubIntOn id

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

drop :: (HasCallStack, Control.Monad m) =>
  Int -> Stream (Of a) m r #-> Stream (Of a) m r
drop n stream = case compare n 0 of
  LT -> Prelude.error "drop called with negative int" $ stream
  EQ -> stream
  GT -> stream & \case
    Return r -> Return r
    Effect m -> Effect $ Control.fmap (drop n) m
    Step (_ :> as) -> drop (n-1) as

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

-- Note: since the accumulated value (inside the control monad) is used both in
-- populating the output stream and in accumulation, it needs to be wrapped in
-- an 'Unrestricted' accross the function
scanM :: forall a x b m r . Control.Monad m =>
  (x #-> a -> m (Unrestricted x)) ->
  m (Unrestricted x) ->
  (x #-> m (Unrestricted b)) ->
  Stream (Of a) m r #->
  Stream (Of b) m r
scanM step mx done stream = stream & \case
  Return r -> Effect $ do
    Unrestricted x <- mx
    Unrestricted b <- done x
    return $ Step $ b :> Return r
  Effect m -> Effect $ Control.fmap (scanM step mx done) m
  Step (a :> as) -> Effect $ do
    Unrestricted x <- mx
    Unrestricted b <- done x
    return $ Step $ b :> (scanM step (step x a) done as)
  where
    Builder{..} = monadBuilder

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

-- Note: this skips failed parses
-- XXX re-write with Text
read :: (Control.Monad m, Read a) =>
  Stream (Of String) m r #-> Stream (Of a) m r
read = mapMaybe readMaybe

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

show :: (Control.Monad m, Prelude.Show a) =>
  Stream (Of a) m r #-> Stream (Of String) m r
show = map Prelude.show

cons :: Control.Monad m => a -> Stream (Of a) m r #-> Stream (Of a) m r
cons a str = Step (a :> str)

-- Note. The action function that is the second argument must be linear since
-- it gets its argument from binding to the first argument, which uses a 
-- control monad.
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

