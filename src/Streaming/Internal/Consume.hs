{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides all functions that take input streams
-- but do not return output streams.
module Streaming.Internal.Consume
  ( -- * Consuming 'Stream's of elements
  -- ** IO Consumers
    stdoutLn
  , stdoutLn'
  , print
  , toHandle
  , writeFile
  -- ** Basic Pure Consumers
  , effects
  , erase
  , drained
  , mapM_
  -- ** Folds
  , fold
  , fold_
  , foldM
  , foldM_
  , all
  , all_
  , any
  , any_
  , sum
  , sum_
  , product
  , product_
  , head
  , head_
  , last
  , last_
  , elem
  , elem_
  , notElem
  , notElem_
  , length
  , length_
  , toList
  , toList_
  , mconcat
  , mconcat_
  , minimum
  , minimum_
  , maximum
  , maximum_
  , foldrM
  , foldrT
  ) where

import Streaming.Internal.Type
import Streaming.Internal.Process
import System.IO.Linear
import System.IO.Resource
import qualified Data.Bool.Linear as Linear
import Prelude.Linear ((&), ($), (.))
import Prelude (Show(..), FilePath, (&&), Bool(..), id, (||),
               Num(..), Maybe(..), Eq(..), Int, Ord(..))
import qualified Prelude as Prelude
import Data.Unrestricted.Linear
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Functor.Identity
import qualified System.IO as System
import Control.Monad.Linear.Builder (BuilderType(..), monadBuilder)
import qualified Control.Monad.Linear as Control


-- #  IO Consumers
-------------------------------------------------------------------------------

-- Note: crashes on a broken output pipe
--
{-| Write 'String's to 'System.stdout' using 'Text.putStrLn'; terminates on a broken output pipe
    (The name and implementation are modelled on the @Pipes.Prelude@ @stdoutLn@).

>>> withLinearIO $ Control.fmap move $ S.stdoutLn $ S.each $ words "one two three"
one
two
three
-}
stdoutLn :: Stream (Of Text) IO () #-> IO ()
stdoutLn stream = stdoutLn' stream
{-# INLINE stdoutLn #-}

-- | Like stdoutLn but with an arbitrary return value
stdoutLn' :: forall r. Stream (Of Text) IO r #-> IO r
stdoutLn' stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of Text) IO r #-> IO r
  loop stream = stream & \case
    Return r -> return r
    Effect ms -> ms >>= stdoutLn'
    Step (str :> stream) -> do
      fromSystemIO $ Text.putStrLn str
      stdoutLn' stream
{-# INLINABLE stdoutLn' #-}

{-| Print the elements of a stream as they arise.

-}
print :: Show a => Stream (Of a) IO r #-> IO r
print = stdoutLn' . map (Text.pack Prelude.. Prelude.show)

-- | Write a stream to a handle and return the handle.
toHandle :: Handle #-> Stream (Of Text) RIO r #-> RIO (r, Handle)
toHandle handle stream = loop handle stream where
  Builder{..} = monadBuilder
  loop :: Handle #-> Stream (Of Text) RIO r #-> RIO (r, Handle)
  loop handle stream = stream & \case
    Return r -> return (r, handle)
    Effect ms -> ms >>= toHandle handle
    Step (text :> stream') -> do
      handle' <- hPutStrLn handle text
      toHandle handle' stream'
{-# INLINABLE toHandle #-}

-- | Write a stream of text as lines as lines to a file
writeFile :: FilePath -> Stream (Of Text) RIO r #-> RIO r
writeFile filepath stream = do
  handle <- openFile filepath System.WriteMode
  (r,handle') <- toHandle handle stream
  hClose handle'
  return r
  where
    Builder{..} = monadBuilder


-- #  Basic Pure Consumers
-------------------------------------------------------------------------------

{- | Reduce a stream, performing its actions but ignoring its elements.

>>> rest <- S.effects $ S.splitAt 2 $ each' [1..5]
>>> S.print rest
3
4
5

    'effects' should be understood together with 'copy' and is subject to the rules

> S.effects . S.copy       = id
> hoist S.effects . S.copy = id

    The similar @effects@ and @copy@ operations in @Data.ByteString.Streaming@ obey the same rules.

-}
effects :: forall a m r. Control.Monad m => Stream (Of a) m r #-> m r
effects stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> m r
  loop stream = stream & \case
    Return r -> return r
    Effect ms -> ms >>= effects
    Step (_ :> stream') -> effects stream'
{-# INLINABLE effects #-}

{- | Remove the elements from a stream of values, retaining the structure of layers.
-}
erase :: forall a m r. Control.Monad m => Stream (Of a) m r #-> Stream Identity m r
erase stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> Stream Identity m r
  loop stream = stream & \case
    Return r -> Return r
    Step (_ :> stream') -> Step $ Identity (erase stream')
    Effect ms -> Effect $ ms >>= (return . erase)
{-# INLINABLE erase #-}

{-| Where a transformer returns a stream, run the effects of the stream, keeping
   the return value. This is usually used at the type

> drained :: Control.Monad m => Stream (Of a) m (Stream (Of b) m r) -> Stream (Of a) m r
> drained = Control.join . Control.fmap (Control.lift . effects)

   Here, for example, we split a stream in two places and throw out the middle segment:

>>> rest <- S.print $ S.drained $ S.splitAt 2 $ S.splitAt 5 $ each' [1..7]
1
2
>>> S.print rest
6
7

-}
drained ::
  ( Control.Monad m
  , Control.Monad (t m)
  , Control.Functor (t m)
  , Control.MonadTrans t) =>
  t m (Stream (Of a) m r) #-> t m r
drained = Control.join . Control.fmap (Control.lift . effects)
{-# INLINE drained #-}

{-| Reduce a stream to its return value with a monadic action.

>>> S.mapM_ Prelude.print $ each' [1..3]
1
2
3


>>> rest <- S.mapM_ Prelude.print $ S.splitAt 3 $ each' [1..10]
1
2
3
>>> S.sum rest
49 :> ()

-}
mapM_ :: forall a m b r. (Consumable b, Control.Monad m) =>
  (a -> m b) -> Stream (Of a) m r #-> m r
mapM_  f stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> m r
  loop stream = stream & \case
    Return r -> return r
    Effect ms -> ms >>= mapM_ f
    Step (a :> stream') -> do
      b <- f a
      return $ consume b
      mapM_ f stream'
{-# INLINABLE mapM_ #-}


-- #  Folds
-------------------------------------------------------------------------------


{-| Strict fold of a 'Stream' of elements that preserves the return value.
   This does not short circuit and all effects are performed.
   The third parameter will often be 'id' where a fold is written by hand:

>>> S.fold (+) 0 id $ each' [1..10]
55 :> ()

>>> S.fold (*) 1 id $ S.fold (+) 0 id $ S.copy $ each' [1..10]
3628800 :> (55 :> ())

    It can be used to replace a standard Haskell type with one more suited to
    writing a strict accumulation function. It is also crucial to the
    Applicative instance for @Control.Foldl.Fold@  We can apply such a fold
    @purely@

> Control.Foldl.purely S.fold :: Control.Monad m => Fold a b -> Stream (Of a) m r #-> m (Of b r)

    Thus, specializing a bit:

> L.purely S.fold L.sum :: Stream (Of Int) Int r #-> m (Of Int r)
> mapped (L.purely S.fold L.sum) :: Stream (Stream (Of Int)) IO r #-> Stream (Of Int) IO r

    Here we use the Applicative instance for @Control.Foldl.Fold@ to
    stream three-item segments of a stream together with their sums and products.

>>> S.print $ mapped (L.purely S.fold (liftA3 (,,) L.list L.product L.sum)) $ chunksOf 3 $ each' [1..10]
([1,2,3],6,6)
([4,5,6],120,15)
([7,8,9],504,24)
([10],10,10)

-}
fold :: forall x a b m r. Control.Monad m =>
  (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r #-> m (Of b r)
fold f x g stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> m (Of b r)
  loop stream = stream & \case
    Return r -> return $ g x :> r
    Effect ms -> ms >>= fold f x g
    Step (a :> stream') -> fold f (f x a) g stream'
{-# INLINABLE fold #-}

{-| Strict fold of a 'Stream' of elements, preserving only the result of the fold, not
    the return value of the stream. This does not short circuit and all effects
    are performed. The third parameter will often be 'id' where a fold
    is written by hand:

>>> S.fold_ (+) 0 id $ each [1..10]
55

    It can be used to replace a standard Haskell type with one more suited to
    writing a strict accumulation function. It is also crucial to the
    Applicative instance for @Control.Foldl.Fold@

> Control.Foldl.purely fold :: Control.Monad m => Fold a b -> Stream (Of a) m () #-> m b

-}
fold_ :: forall x a b m r. (Control.Monad m, Consumable r) =>
  (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r #-> m b
fold_ f x g stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> m b
  loop stream = stream & \case
    Return r -> lseq r $ return $ g x
    Effect ms -> ms >>= fold_ f x g
    Step (a :> stream') -> fold_ f (f x a) g stream'
{-# INLINABLE fold_ #-}

-- Note: We can't use 'Of' since the left component is unrestricted.
-- Remark: to use the (`m x`) in the folding function that is the first
-- argument, we must bind to it. Since `m` is a `Control.Monad`, we need
-- the folding function to consume `x` linearly.
--
{-| Strict, monadic fold of the elements of a @Stream (Of a)@

> Control.Foldl.impurely foldM :: Control.Monad m => FoldM a b -> Stream (Of a) m r #-> m (b, r)

   Thus to accumulate the elements of a stream as a vector, together with a random
   element we might write:

>>> L.impurely S.foldM (liftA2 (,) L.vectorM L.random) $ each' [1..10::Int] :: IO (Of (Vector Int, Maybe Int) ())
([1,2,3,4,5,6,7,8,9,10],Just 9) :> ()

-}
foldM :: forall x a m b r. Control.Monad m =>
  (x #-> a -> m x) -> m x -> (x #-> m b) -> Stream (Of a) m r #-> m (b,r)
foldM f mx g stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> m (b,r)
  loop stream = stream & \case
    Return r -> mx >>= g >>= (\b -> return (b,r))
    Effect ms -> ms >>= foldM f mx g
    Step (a :> stream') -> foldM f (mx >>= \x -> f x a) g stream'
{-# INLINABLE foldM #-}

{-| Strict, monadic fold of the elements of a @Stream (Of a)@

> Control.Foldl.impurely foldM_ :: Control.Monad m => FoldM a b -> Stream (Of a) m () #-> m b
-}
foldM_ :: forall a m x b r. (Control.Monad m, Consumable r) =>
  (x #-> a -> m x) -> m x -> (x #-> m b) -> Stream (Of a) m r #-> m b
foldM_ f mx g stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> m b
  loop stream = stream & \case
    Return r  -> lseq r $ mx >>= g
    Effect ms -> ms >>= foldM_ f mx g
    Step (a :> stream') -> foldM_ f (mx >>= \x -> f x a) g stream'
{-# INLINABLE foldM_ #-}

-- | Note: does not short circuit
all :: Control.Monad m => (a -> Bool) -> Stream (Of a) m r #-> m (Of Bool r)
all f stream = fold (&&) True id (map f stream)
{-# INLINABLE all #-}

-- | Note: does not short circuit
all_ :: (Consumable r, Control.Monad m) => (a -> Bool) -> Stream (Of a) m r #-> m Bool
all_ f stream = fold_ (&&) True id (map f stream)
{-# INLINABLE all_ #-}

-- | Note: does not short circuit
any :: Control.Monad m => (a -> Bool) -> Stream (Of a) m r #-> m (Of Bool r)
any f stream = fold (||) False id (map f stream)
{-# INLINABLE any #-}

-- | Note: does not short circuit
any_ :: (Consumable r, Control.Monad m) => (a -> Bool) -> Stream (Of a) m r #-> m Bool
any_ f stream = fold_ (||) False id (map f stream)
{-# INLINABLE any_ #-}

{-| Fold a 'Stream' of numbers into their sum with the return value

>  mapped S.sum :: Stream (Stream (Of Int)) m r #-> Stream (Of Int) m r


>>> S.sum $ each' [1..10]
55 :> ()

>>> (n :> rest)  <- S.sum $ S.splitAt 3 $ each' [1..10]
>>> System.IO.print n
6
>>> (m :> rest') <- S.sum $ S.splitAt 3 rest
>>> System.IO.print m
15
>>> S.print rest'
7
8
9
10

-}
sum :: (Control.Monad m, Num a) => Stream (Of a) m r #-> m (Of a r)
sum stream = fold (+) 0 id stream
{-# INLINE sum #-}

-- | Fold a 'Stream' of numbers into their sum
sum_ :: (Control.Monad m, Num a) => Stream (Of a) m () #-> m a
sum_ stream = fold_ (+) 0 id stream
{-# INLINE sum_ #-}

{-| Fold a 'Stream' of numbers into their product with the return value

>  mapped product :: Stream (Stream (Of Int)) m r -> Stream (Of Int) m r
-}
product :: (Control.Monad m, Num a) => Stream (Of a) m r #-> m (Of a r)
product stream = fold (*) 1 id stream
{-# INLINE product #-}

-- | Fold a 'Stream' of numbers into their product
product_ :: (Control.Monad m, Num a) => Stream (Of a) m () #-> m a
product_ stream = fold_ (*) 1 id stream
{-# INLINE product_ #-}

-- | Note that 'head' exhausts the rest of the stream following the
-- first element, performing all monadic effects via 'effects'
head :: Control.Monad m => Stream (Of a) m r #-> m (Of (Maybe a) r)
head str = str & \case
  Return r -> return (Nothing :> r)
  Effect m -> m >>= head
  Step (a :> rest) ->
    effects rest >>= \r -> return (Just a :> r)
  where
    Builder{..} = monadBuilder
{-# INLINABLE head #-}

-- | Note that 'head' exhausts the rest of the stream following the
-- first element, performing all monadic effects via 'effects'
head_ :: (Consumable r, Control.Monad m) => Stream (Of a) m r #-> m (Maybe a)
head_ str = str & \case
  Return r -> lseq r $ return Nothing
  Effect m -> m >>= head_
  Step (a :> rest) ->
    effects rest >>= \r -> lseq r $ return  (Just a)
  where
    Builder{..} = monadBuilder
{-# INLINABLE head_ #-}

last :: Control.Monad m => Stream (Of a) m r #-> m (Of (Maybe a) r)
last = loop Nothing where
  loop :: Control.Monad m => Maybe a -> Stream (Of a) m r #-> m (Of (Maybe a) r)
  loop m s = s & \case
    Return r  -> return (m :> r)
    Effect m -> m >>= last
    Step (a :> rest) -> loop (Just a) rest
  Builder{..} = monadBuilder
{-# INLINABLE last #-}

last_ :: (Consumable r, Control.Monad m) => Stream (Of a) m r #-> m (Maybe a)
last_ = loop Nothing where
  loop :: (Consumable r, Control.Monad m) =>
    Maybe a -> Stream (Of a) m r #-> m (Maybe a)
  loop m s = s & \case
    Return r  -> lseq r $ return m
    Effect m -> m >>= last_
    Step (a :> rest) -> loop (Just a) rest
  Builder{..} = monadBuilder
{-# INLINABLE last_ #-}

elem :: forall a m r. (Control.Monad m, Eq a) =>
  a -> Stream (Of a) m r #-> m (Of Bool r)
elem a stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> m (Of Bool r)
  loop stream = stream & \case
    Return r -> return $ False :> r
    Effect ms -> ms >>= elem a
    Step (a' :> stream') -> case a == a' of
      True -> effects stream' >>= (\r -> return $ True :> r)
      False -> elem a stream'
{-# INLINABLE elem #-}

elem_ :: forall a m r. (Consumable r, Control.Monad m, Eq a) =>
  a -> Stream (Of a) m r #-> m Bool
elem_ a stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> m Bool
  loop stream = stream & \case
    Return r -> lseq r $ return False
    Effect ms -> ms >>= elem_ a
    Step (a' :> stream') -> case a == a' of
      True -> effects stream' >>= \r -> lseq r $ return True
      False -> elem_ a stream'
{-# INLINABLE elem_ #-}

{-| Exhaust a stream deciding whether @a@ was an element.

-}
notElem :: (Control.Monad m, Eq a) => a -> Stream (Of a) m r #-> m (Of Bool r)
notElem a stream = Control.fmap negate $ elem a stream
  where
    negate :: Of Bool r #-> Of Bool r
    negate (b :> r) = Prelude.not b :> r
{-# INLINE notElem #-}

notElem_ :: (Consumable r, Control.Monad m, Eq a) => a -> Stream (Of a) m r #-> m Bool
notElem_ a stream = Control.fmap Linear.not $ elem_ a stream
{-# INLINE notElem_ #-}

{-| Run a stream, keeping its length and its return value.

>>> S.print $ mapped S.length $ chunksOf 3 $ S.each' [1..10]
3
3
3
1

-}
length :: Control.Monad m => Stream (Of a) m r #-> m (Of Int r)
length = fold (\n _ -> n + 1) 0 id
{-# INLINE length #-}


{-| Run a stream, remembering only its length:

>>> runIdentity $ S.length_ (S.each [1..10] :: Stream (Of Int) Identity ())
10

-}
length_ :: (Consumable r, Control.Monad m) => Stream (Of a) m r #-> m Int
length_ = fold_ (\n _ -> n + 1) 0 id
{-# INLINE length_ #-}

{-| Convert an effectful 'Stream' into a list alongside the return value

>  mapped toList :: Stream (Stream (Of a) m) m r #-> Stream (Of [a]) m r

    Like 'toList_', 'toList' breaks streaming; unlike 'toList_' it /preserves the return value/
    and thus is frequently useful with e.g. 'mapped'

>>> S.print $ mapped S.toList $ chunksOf 3 $ each' [1..9]
[1,2,3]
[4,5,6]
[7,8,9]

>>> S.print $ mapped S.toList $ chunksOf 2 $ S.replicateM 4 getLine
s<Enter>
t<Enter>
["s","t"]
u<Enter>
v<Enter>
["u","v"]
-}
toList :: Control.Monad m => Stream (Of a) m r #-> m (Of [a] r)
toList = fold (Prelude.flip (:)) [] id

{-| Convert an effectful @Stream (Of a)@ into a list of @as@

    Note: Needless to say, this function does not stream properly.
    It is basically the same as Prelude 'mapM' which, like 'replicateM',
    'sequence' and similar operations on traversable containers
    is a leading cause of space leaks.

-}
toList_ :: Control.Monad m => Stream (Of a) m () #-> m [a]
toList_ stream = fold_ (Prelude.flip (:)) [] id stream
{-# INLINE toList #-}

{-| Fold streamed items into their monoidal sum

 -}
mconcat :: (Control.Monad m, Prelude.Monoid w) => Stream (Of w) m r #-> m (Of w r)
mconcat = fold (Prelude.<>) Prelude.mempty id
{-# INLINE mconcat #-}

mconcat_ :: (Consumable r, Control.Monad m, Prelude.Monoid w) =>
  Stream (Of w) m r #-> m w
mconcat_ = fold_ (Prelude.<>) Prelude.mempty id
{-# INLINE mconcat_ #-}

minimum :: (Control.Monad m, Ord a) => Stream (Of a) m r #-> m (Of (Maybe a) r)
minimum = fold getMin Nothing id . map Just
{-# INLINE minimum #-}

minimum_ :: (Consumable r, Control.Monad m, Ord a) =>
  Stream (Of a) m r #-> m (Maybe a)
minimum_ = fold_ getMin Nothing id . map Just
{-# INLINE minimum_ #-}

maximum :: (Control.Monad m, Ord a) => Stream (Of a) m r #-> m (Of (Maybe a) r)
maximum = fold getMax Nothing id . map Just
{-# INLINE maximum #-}

maximum_ :: (Consumable r, Control.Monad m, Ord a) =>
  Stream (Of a) m r #-> m (Maybe a)
maximum_ = fold_ getMax Nothing id . map Just
{-# INLINE maximum_ #-}

getMin :: Ord a => Maybe a -> Maybe a -> Maybe a
getMin = mCompare Prelude.min

getMax :: Ord a => Maybe a -> Maybe a -> Maybe a
getMax = mCompare Prelude.max

mCompare :: Ord a => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mCompare comp Nothing Nothing = Nothing
mCompare comp (Just a) Nothing = Just a
mCompare comp Nothing (Just a) = Just a
mCompare comp (Just x) (Just y) = Just $ comp x y

{-| A natural right fold for consuming a stream of elements.
    See also the more general 'iterT' in the 'Streaming' module and the
    still more general 'destroy'
-}
foldrM :: forall a m r. Control.Monad m
       => (a -> m r #-> m r) -> Stream (Of a) m r #-> m r
foldrM step stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> m r
  loop stream = stream & \case
    Return r -> return r
    Effect m -> m >>= foldrM step
    Step (a :> as) -> step a (foldrM step as)
{-# INLINABLE foldrM #-}

{-| A natural right fold for consuming a stream of elements.
    See also the more general 'iterTM' in the 'Streaming' module
    and the still more general 'destroy'

> foldrT (\a p -> Streaming.yield a >> p) = id

-}
foldrT :: forall a t m r.
  (Control.Monad m, Control.MonadTrans t, Control.Monad (t m)) =>
  (a -> t m r #-> t m r) -> Stream (Of a) m r #-> t m r
foldrT step stream = loop stream where
  Builder{..} = monadBuilder
  loop :: Stream (Of a) m r #-> t m r
  loop stream = stream & \case
    Return r -> return r
    Effect ms -> (Control.lift ms) >>= foldrT step
    Step (a :> as) -> step a (foldrT step as)
{-# INLINABLE foldrT #-}

