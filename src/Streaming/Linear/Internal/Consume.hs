{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

-- | This module provides all functions that take input streams
-- but do not return output streams.
module Streaming.Linear.Internal.Consume
  ( -- * Consuming 'Stream's of elements

    -- ** IO Consumers
    stdoutLn,
    stdoutLn',
    print,
    toHandle,
    writeFile,

    -- ** Basic Pure Consumers
    effects,
    erase,
    drained,
    mapM_,

    -- ** Folds
    fold,
    fold_,
    foldM,
    foldM_,
    all,
    all_,
    any,
    any_,
    sum,
    sum_,
    product,
    product_,
    head,
    head_,
    last,
    last_,
    elem,
    elem_,
    notElem,
    notElem_,
    length,
    length_,
    toList,
    toList_,
    mconcat,
    mconcat_,
    minimum,
    minimum_,
    maximum,
    maximum_,
    foldrM,
    foldrT,
  )
where

import qualified Control.Functor.Linear as Control
import qualified Data.Bool.Linear as Linear
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Unrestricted.Linear
import Prelude.Linear (($), (&), (.))
import Streaming.Linear.Internal.Process
import Streaming.Linear.Internal.Type
import qualified System.IO as System
import System.IO.Linear
import System.IO.Resource.Linear
import Prelude
  ( Bool (..),
    Eq (..),
    FilePath,
    Int,
    Maybe (..),
    Num (..),
    Ord (..),
    Show (..),
    id,
    (&&),
    (||),
  )
import qualified Prelude as Prelude

-- #  IO Consumers
-------------------------------------------------------------------------------

-- Note: crashes on a broken output pipe
--

-- | Write 'String's to 'System.stdout' using 'Text.putStrLn'; terminates on a broken output pipe
--    (The name and implementation are modelled on the @Pipes.Prelude@ @stdoutLn@).
--
-- \>\>\> withLinearIO $ Control.fmap move $ S.stdoutLn $ S.each $ words "one two three"
-- one
-- two
-- three
stdoutLn :: Stream (Of Text) IO () %1 -> IO ()
stdoutLn stream = stdoutLn' stream
{-# INLINE stdoutLn #-}

-- | Like stdoutLn but with an arbitrary return value
stdoutLn' :: forall r. Stream (Of Text) IO r %1 -> IO r
stdoutLn' stream = loop stream
  where
    loop :: Stream (Of Text) IO r %1 -> IO r
    loop stream =
      stream & \case
        Return r -> Control.return r
        Effect ms -> ms Control.>>= stdoutLn'
        Step (str :> stream) -> Control.do
          fromSystemIO $ Text.putStrLn str
          stdoutLn' stream
{-# INLINEABLE stdoutLn' #-}

-- | Print the elements of a stream as they arise.
print :: Show a => Stream (Of a) IO r %1 -> IO r
print = stdoutLn' . map (Text.pack Prelude.. Prelude.show)

-- | Write a stream to a handle and return the handle.
toHandle :: Handle %1 -> Stream (Of Text) RIO r %1 -> RIO (r, Handle)
toHandle handle stream = loop handle stream
  where
    loop :: Handle %1 -> Stream (Of Text) RIO r %1 -> RIO (r, Handle)
    loop handle stream =
      stream & \case
        Return r -> Control.return (r, handle)
        Effect ms -> ms Control.>>= toHandle handle
        Step (text :> stream') -> Control.do
          handle' <- hPutStrLn handle text
          toHandle handle' stream'
{-# INLINEABLE toHandle #-}

-- | Write a stream of text as lines as lines to a file
writeFile :: FilePath -> Stream (Of Text) RIO r %1 -> RIO r
writeFile filepath stream = Control.do
  handle <- openFile filepath System.WriteMode
  (r, handle') <- toHandle handle stream
  hClose handle'
  Control.return r

-- #  Basic Pure Consumers
-------------------------------------------------------------------------------

-- | Reduce a stream, performing its actions but ignoring its elements.
--
-- @
-- \>\>\> rest <- S.effects $ S.splitAt 2 $ each' [1..5]
-- \>\>\> S.print rest
-- 3
-- 4
-- 5
-- @
--
--    'effects' should be understood together with 'copy' and is subject to the rules
--
-- > S.effects . S.copy       = id
-- > hoist S.effects . S.copy = id
--
--    The similar @effects@ and @copy@ operations in @Data.ByteString.Streaming@ obey the same rules.
effects :: forall a m r. Control.Monad m => Stream (Of a) m r %1 -> m r
effects stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> m r
    loop stream =
      stream & \case
        Return r -> Control.return r
        Effect ms -> ms Control.>>= effects
        Step (_ :> stream') -> effects stream'
{-# INLINEABLE effects #-}

-- | Remove the elements from a stream of values, retaining the structure of layers.
erase :: forall a m r. Control.Monad m => Stream (Of a) m r %1 -> Stream Identity m r
erase stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> Stream Identity m r
    loop stream =
      stream & \case
        Return r -> Return r
        Step (_ :> stream') -> Step $ Identity (erase stream')
        Effect ms -> Effect $ ms Control.>>= (Control.return . erase)
{-# INLINEABLE erase #-}

-- | Where a transformer returns a stream, run the effects of the stream, keeping
--   the return value. This is usually used at the type
--
-- > drained :: Control.Monad m => Stream (Of a) m (Stream (Of b) m r) -> Stream (Of a) m r
-- > drained = Control.join . Control.fmap (Control.lift . effects)
--
--   Here, for example, we split a stream in two places and throw out the middle segment:
--
-- @
-- \>\>\> rest <- S.print $ S.drained $ S.splitAt 2 $ S.splitAt 5 $ each' [1..7]
-- 1
-- 2
-- \>\>\> S.print rest
-- 6
-- 7
-- @
drained ::
  ( Control.Monad m,
    Control.Monad (t m),
    Control.Functor (t m),
    Control.MonadTrans t
  ) =>
  t m (Stream (Of a) m r) %1 ->
  t m r
drained = Control.join . Control.fmap (Control.lift . effects)
{-# INLINE drained #-}

-- | Reduce a stream to its return value with a monadic action.
--
-- @
-- \>\>\> S.mapM_ Prelude.print $ each' [1..3]
-- 1
-- 2
-- 3
-- @
--
-- @
-- \>\>\> rest <- S.mapM_ Prelude.print $ S.splitAt 3 $ each' [1..10]
-- 1
-- 2
-- 3
-- \>\>\> S.sum rest
-- 49 :> ()
-- @
mapM_ ::
  forall a m b r.
  (Consumable b, Control.Monad m) =>
  (a -> m b) ->
  Stream (Of a) m r %1 ->
  m r
mapM_ f stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> m r
    loop stream =
      stream & \case
        Return r -> Control.return r
        Effect ms -> ms Control.>>= mapM_ f
        Step (a :> stream') -> Control.do
          b <- f a
          Control.return $ consume b
          mapM_ f stream'
{-# INLINEABLE mapM_ #-}

-- #  Folds
-------------------------------------------------------------------------------

-- | Strict fold of a 'Stream' of elements that preserves the return value.
--   This does not short circuit and all effects are performed.
--   The third parameter will often be 'id' where a fold is written by hand:
--
-- @
-- \>\>\> S.fold (+) 0 id $ each' [1..10]
-- 55 :> ()
-- @
--
-- @
-- \>\>\> S.fold (*) 1 id $ S.fold (+) 0 id $ S.copy $ each' [1..10]
-- 3628800 :> (55 :> ())
-- @
--
--    It can be used to replace a standard Haskell type with one more suited to
--    writing a strict accumulation function. It is also crucial to the
--    Applicative instance for @Control.Foldl.Fold@  We can apply such a fold
--    @purely@
--
-- > Control.Foldl.purely S.fold :: Control.Monad m => Fold a b -> Stream (Of a) m r %1-> m (Of b r)
--
--    Thus, specializing a bit:
--
-- > L.purely S.fold L.sum :: Stream (Of Int) Int r %1-> m (Of Int r)
-- > mapped (L.purely S.fold L.sum) :: Stream (Stream (Of Int)) IO r %1-> Stream (Of Int) IO r
--
--    Here we use the Applicative instance for @Control.Foldl.Fold@ to
--    stream three-item segments of a stream together with their sums and products.
--
-- @
-- \>\>\> S.print $ mapped (L.purely S.fold (liftA3 (,,) L.list L.product L.sum)) $ chunksOf 3 $ each' [1..10]
-- ([1,2,3],6,6)
-- ([4,5,6],120,15)
-- ([7,8,9],504,24)
-- ([10],10,10)
-- @
fold ::
  forall x a b m r.
  Control.Monad m =>
  (x -> a -> x) ->
  x ->
  (x -> b) ->
  Stream (Of a) m r %1 ->
  m (Of b r)
fold f x g stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> m (Of b r)
    loop stream =
      stream & \case
        Return r -> Control.return $ g x :> r
        Effect ms -> ms Control.>>= fold f x g
        Step (a :> stream') -> fold f (f x a) g stream'
{-# INLINEABLE fold #-}

-- | Strict fold of a 'Stream' of elements, preserving only the result of the fold, not
--    the return value of the stream. This does not short circuit and all effects
--    are performed. The third parameter will often be 'id' where a fold
--    is written by hand:
--
-- @
-- \>\>\> S.fold_ (+) 0 id $ each [1..10]
-- 55
-- @
--
--    It can be used to replace a standard Haskell type with one more suited to
--    writing a strict accumulation function. It is also crucial to the
--    Applicative instance for @Control.Foldl.Fold@
--
-- > Control.Foldl.purely fold :: Control.Monad m => Fold a b -> Stream (Of a) m () %1-> m b
fold_ ::
  forall x a b m r.
  (Control.Monad m, Consumable r) =>
  (x -> a -> x) ->
  x ->
  (x -> b) ->
  Stream (Of a) m r %1 ->
  m b
fold_ f x g stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> m b
    loop stream =
      stream & \case
        Return r -> lseq r $ Control.return $ g x
        Effect ms -> ms Control.>>= fold_ f x g
        Step (a :> stream') -> fold_ f (f x a) g stream'
{-# INLINEABLE fold_ #-}

-- Note: We can't use 'Of' since the left component is unrestricted.
-- Remark: to use the (`m x`) in the folding function that is the first
-- argument, we must bind to it. Since `m` is a `Control.Monad`, we need
-- the folding function to consume `x` linearly.
--

-- | Strict, monadic fold of the elements of a @Stream (Of a)@
--
-- > Control.Foldl.impurely foldM :: Control.Monad m => FoldM a b -> Stream (Of a) m r %1-> m (b, r)
--
--   Thus to accumulate the elements of a stream as a vector, together with a random
--   element we might write:
--
-- @
-- \>\>\> L.impurely S.foldM (liftA2 (,) L.vectorM L.random) $ each' [1..10::Int] :: IO (Of (Vector Int, Maybe Int) ())
-- ([1,2,3,4,5,6,7,8,9,10],Just 9) :> ()
-- @
foldM ::
  forall x a m b r.
  Control.Monad m =>
  (x %1 -> a -> m x) ->
  m x ->
  (x %1 -> m b) ->
  Stream (Of a) m r %1 ->
  m (b, r)
foldM f mx g stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> m (b, r)
    loop stream =
      stream & \case
        Return r -> mx Control.>>= g Control.>>= (\b -> Control.return (b, r))
        Effect ms -> ms Control.>>= foldM f mx g
        Step (a :> stream') -> foldM f (mx Control.>>= \x -> f x a) g stream'
{-# INLINEABLE foldM #-}

-- | Strict, monadic fold of the elements of a @Stream (Of a)@
--
-- > Control.Foldl.impurely foldM_ :: Control.Monad m => FoldM a b -> Stream (Of a) m () %1-> m b
foldM_ ::
  forall a m x b r.
  (Control.Monad m, Consumable r) =>
  (x %1 -> a -> m x) ->
  m x ->
  (x %1 -> m b) ->
  Stream (Of a) m r %1 ->
  m b
foldM_ f mx g stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> m b
    loop stream =
      stream & \case
        Return r -> lseq r $ mx Control.>>= g
        Effect ms -> ms Control.>>= foldM_ f mx g
        Step (a :> stream') -> foldM_ f (mx Control.>>= \x -> f x a) g stream'
{-# INLINEABLE foldM_ #-}

-- | Note: does not short circuit
all :: Control.Monad m => (a -> Bool) -> Stream (Of a) m r %1 -> m (Of Bool r)
all f stream = fold (&&) True id (map f stream)
{-# INLINEABLE all #-}

-- | Note: does not short circuit
all_ :: (Consumable r, Control.Monad m) => (a -> Bool) -> Stream (Of a) m r %1 -> m Bool
all_ f stream = fold_ (&&) True id (map f stream)
{-# INLINEABLE all_ #-}

-- | Note: does not short circuit
any :: Control.Monad m => (a -> Bool) -> Stream (Of a) m r %1 -> m (Of Bool r)
any f stream = fold (||) False id (map f stream)
{-# INLINEABLE any #-}

-- | Note: does not short circuit
any_ :: (Consumable r, Control.Monad m) => (a -> Bool) -> Stream (Of a) m r %1 -> m Bool
any_ f stream = fold_ (||) False id (map f stream)
{-# INLINEABLE any_ #-}

-- | Fold a 'Stream' of numbers into their sum with the return value
--
-- >  mapped S.sum :: Stream (Stream (Of Int)) m r %1-> Stream (Of Int) m r
--
-- @
-- \>\>\> S.sum $ each' [1..10]
-- 55 :> ()
-- @
--
-- @
-- \>\>\> (n :> rest)  <- S.sum $ S.splitAt 3 $ each' [1..10]
-- \>\>\> System.IO.print n
-- 6
-- \>\>\> (m :> rest') <- S.sum $ S.splitAt 3 rest
-- \>\>\> System.IO.print m
-- 15
-- \>\>\> S.print rest'
-- 7
-- 8
-- 9
-- 10
-- @
sum :: (Control.Monad m, Num a) => Stream (Of a) m r %1 -> m (Of a r)
sum stream = fold (+) 0 id stream
{-# INLINE sum #-}

-- | Fold a 'Stream' of numbers into their sum
sum_ :: (Control.Monad m, Num a) => Stream (Of a) m () %1 -> m a
sum_ stream = fold_ (+) 0 id stream
{-# INLINE sum_ #-}

-- | Fold a 'Stream' of numbers into their product with the return value
--
-- >  mapped product :: Stream (Stream (Of Int)) m r -> Stream (Of Int) m r
product :: (Control.Monad m, Num a) => Stream (Of a) m r %1 -> m (Of a r)
product stream = fold (*) 1 id stream
{-# INLINE product #-}

-- | Fold a 'Stream' of numbers into their product
product_ :: (Control.Monad m, Num a) => Stream (Of a) m () %1 -> m a
product_ stream = fold_ (*) 1 id stream
{-# INLINE product_ #-}

-- | Note that 'head' exhausts the rest of the stream following the
-- first element, performing all monadic effects via 'effects'
head :: Control.Monad m => Stream (Of a) m r %1 -> m (Of (Maybe a) r)
head str =
  str & \case
    Return r -> Control.return (Nothing :> r)
    Effect m -> m Control.>>= head
    Step (a :> rest) ->
      effects rest Control.>>= \r -> Control.return (Just a :> r)
{-# INLINEABLE head #-}

-- | Note that 'head' exhausts the rest of the stream following the
-- first element, performing all monadic effects via 'effects'
head_ :: (Consumable r, Control.Monad m) => Stream (Of a) m r %1 -> m (Maybe a)
head_ str =
  str & \case
    Return r -> lseq r $ Control.return Nothing
    Effect m -> m Control.>>= head_
    Step (a :> rest) ->
      effects rest Control.>>= \r -> lseq r $ Control.return (Just a)
{-# INLINEABLE head_ #-}

last :: Control.Monad m => Stream (Of a) m r %1 -> m (Of (Maybe a) r)
last = loop Nothing
  where
    loop ::
      Control.Monad m =>
      Maybe a ->
      Stream (Of a) m r %1 ->
      m (Of (Maybe a) r)
    loop m s =
      s & \case
        Return r -> Control.return (m :> r)
        Effect m -> m Control.>>= last
        Step (a :> rest) -> loop (Just a) rest
{-# INLINEABLE last #-}

last_ :: (Consumable r, Control.Monad m) => Stream (Of a) m r %1 -> m (Maybe a)
last_ = loop Nothing
  where
    loop ::
      (Consumable r, Control.Monad m) =>
      Maybe a ->
      Stream (Of a) m r %1 ->
      m (Maybe a)
    loop m s =
      s & \case
        Return r -> lseq r $ Control.return m
        Effect m -> m Control.>>= last_
        Step (a :> rest) -> loop (Just a) rest
{-# INLINEABLE last_ #-}

elem ::
  forall a m r.
  (Control.Monad m, Eq a) =>
  a ->
  Stream (Of a) m r %1 ->
  m (Of Bool r)
elem a stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> m (Of Bool r)
    loop stream =
      stream & \case
        Return r -> Control.return $ False :> r
        Effect ms -> ms Control.>>= elem a
        Step (a' :> stream') -> case a == a' of
          True -> effects stream' Control.>>= (\r -> Control.return $ True :> r)
          False -> elem a stream'
{-# INLINEABLE elem #-}

infix 4 `elem` -- same fixity as base.elem

elem_ ::
  forall a m r.
  (Consumable r, Control.Monad m, Eq a) =>
  a ->
  Stream (Of a) m r %1 ->
  m Bool
elem_ a stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> m Bool
    loop stream =
      stream & \case
        Return r -> lseq r $ Control.return False
        Effect ms -> ms Control.>>= elem_ a
        Step (a' :> stream') -> case a == a' of
          True -> effects stream' Control.>>= \r -> lseq r $ Control.return True
          False -> elem_ a stream'
{-# INLINEABLE elem_ #-}

-- | Exhaust a stream deciding whether @a@ was an element.
notElem :: (Control.Monad m, Eq a) => a -> Stream (Of a) m r %1 -> m (Of Bool r)
notElem a stream = Control.fmap negate $ elem a stream
  where
    negate :: Of Bool r %1 -> Of Bool r
    negate (b :> r) = Prelude.not b :> r
{-# INLINE notElem #-}

notElem_ :: (Consumable r, Control.Monad m, Eq a) => a -> Stream (Of a) m r %1 -> m Bool
notElem_ a stream = Control.fmap Linear.not $ elem_ a stream
{-# INLINE notElem_ #-}

-- | Run a stream, keeping its length and its return value.
--
-- @
-- \>\>\> S.print $ mapped S.length $ chunksOf 3 $ S.each' [1..10]
-- 3
-- 3
-- 3
-- 1
-- @
length :: Control.Monad m => Stream (Of a) m r %1 -> m (Of Int r)
length = fold (\n _ -> n + 1) 0 id
{-# INLINE length #-}

-- | Run a stream, remembering only its length:
--
-- @
-- \>\>\> runIdentity $ S.length_ (S.each [1..10] :: Stream (Of Int) Identity ())
-- 10
-- @
length_ :: (Consumable r, Control.Monad m) => Stream (Of a) m r %1 -> m Int
length_ = fold_ (\n _ -> n + 1) 0 id
{-# INLINE length_ #-}

-- | Convert an effectful 'Stream' into a list alongside the return value
--
-- >  mapped toList :: Stream (Stream (Of a) m) m r %1-> Stream (Of [a]) m r
--
--    Like 'toList_', 'toList' breaks streaming; unlike 'toList_' it /preserves the return value/
--    and thus is frequently useful with e.g. 'mapped'
--
-- @
-- \>\>\> S.print $ mapped S.toList $ chunksOf 3 $ each' [1..9]
-- [1,2,3]
-- [4,5,6]
-- [7,8,9]
-- @
--
-- @
-- \>\>\> S.print $ mapped S.toList $ chunksOf 2 $ S.replicateM 4 getLine
-- s<Enter>
-- t<Enter>
-- ["s","t"]
-- u<Enter>
-- v<Enter>
-- ["u","v"]
-- @
toList :: Control.Monad m => Stream (Of a) m r %1 -> m (Of [a] r)
toList = fold (\diff a ls -> diff (a : ls)) id (\diff -> diff [])
{-# INLINE toList #-}

-- | Convert an effectful @Stream (Of a)@ into a list of @as@
--
--    Note: Needless to say, this function does not stream properly.
--    It is basically the same as Prelude 'mapM' which, like 'replicateM',
--    'sequence' and similar operations on traversable containers
--    is a leading cause of space leaks.
toList_ :: Control.Monad m => Stream (Of a) m () %1 -> m [a]
toList_ = fold_ (\diff a ls -> diff (a : ls)) id (\diff -> diff [])
{-# INLINE toList_ #-}

-- | Fold streamed items into their monoidal sum
mconcat :: (Control.Monad m, Prelude.Monoid w) => Stream (Of w) m r %1 -> m (Of w r)
mconcat = fold (Prelude.<>) Prelude.mempty id
{-# INLINE mconcat #-}

mconcat_ ::
  (Consumable r, Control.Monad m, Prelude.Monoid w) =>
  Stream (Of w) m r %1 ->
  m w
mconcat_ = fold_ (Prelude.<>) Prelude.mempty id
{-# INLINE mconcat_ #-}

minimum :: (Control.Monad m, Ord a) => Stream (Of a) m r %1 -> m (Of (Maybe a) r)
minimum = fold getMin Nothing id . map Just
{-# INLINE minimum #-}

minimum_ ::
  (Consumable r, Control.Monad m, Ord a) =>
  Stream (Of a) m r %1 ->
  m (Maybe a)
minimum_ = fold_ getMin Nothing id . map Just
{-# INLINE minimum_ #-}

maximum :: (Control.Monad m, Ord a) => Stream (Of a) m r %1 -> m (Of (Maybe a) r)
maximum = fold getMax Nothing id . map Just
{-# INLINE maximum #-}

maximum_ ::
  (Consumable r, Control.Monad m, Ord a) =>
  Stream (Of a) m r %1 ->
  m (Maybe a)
maximum_ = fold_ getMax Nothing id . map Just
{-# INLINE maximum_ #-}

getMin :: Ord a => Maybe a -> Maybe a -> Maybe a
getMin = mCompare Prelude.min

getMax :: Ord a => Maybe a -> Maybe a -> Maybe a
getMax = mCompare Prelude.max

mCompare :: Ord a => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mCompare _ Nothing Nothing = Nothing
mCompare _ (Just a) Nothing = Just a
mCompare _ Nothing (Just a) = Just a
mCompare comp (Just x) (Just y) = Just $ comp x y

-- | A natural right fold for consuming a stream of elements.
--    See also the more general 'iterT' in the 'Streaming' module and the
--    still more general 'destroy'
foldrM ::
  forall a m r.
  Control.Monad m =>
  (a -> m r %1 -> m r) ->
  Stream (Of a) m r %1 ->
  m r
foldrM step stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> m r
    loop stream =
      stream & \case
        Return r -> Control.return r
        Effect m -> m Control.>>= foldrM step
        Step (a :> as) -> step a (foldrM step as)
{-# INLINEABLE foldrM #-}

-- | A natural right fold for consuming a stream of elements.
--    See also the more general 'iterTM' in the 'Streaming' module
--    and the still more general 'destroy'
--
-- > foldrT (\a p -> Streaming.yield a >> p) = id
foldrT ::
  forall a t m r.
  (Control.Monad m, Control.MonadTrans t, Control.Monad (t m)) =>
  (a -> t m r %1 -> t m r) ->
  Stream (Of a) m r %1 ->
  t m r
foldrT step stream = loop stream
  where
    loop :: Stream (Of a) m r %1 -> t m r
    loop stream =
      stream & \case
        Return r -> Control.return r
        Effect ms -> (Control.lift ms) Control.>>= foldrT step
        Step (a :> as) -> step a (foldrT step as)
{-# INLINEABLE foldrT #-}
