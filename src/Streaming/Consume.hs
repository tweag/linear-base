{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides all functions that take input streams
-- but do not return output streams.
module Streaming.Consume
  ( -- * Consuming streams of elements
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

import Streaming.Type
import Streaming.Process
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

-- | Note: crashes on a broken output pipe
stdoutLn :: Stream (Of Text) IO () #-> IO ()
stdoutLn stream = stdoutLn' stream

stdoutLn' :: Stream (Of Text) IO r #-> IO r
stdoutLn' stream = stream & \case
  Return r -> return r
  Effect ms -> ms >>= stdoutLn'
  Step (str :> stream) -> do
    fromSystemIO $ Text.putStrLn str
    stdoutLn' stream
  where
    Builder{..} = monadBuilder

print :: Show a => Stream (Of a) IO r #-> IO r
print = stdoutLn' . map (Text.pack Prelude.. Prelude.show)

-- | Write a stream to a handle and return the handle.
toHandle :: Handle #-> Stream (Of Text) RIO r #-> RIO (r, Handle)
toHandle handle stream = stream & \case
  Return r -> return (r, handle)
  Effect ms -> ms >>= toHandle handle
  Step (text :> stream') -> do
    handle' <- hPutStrLn handle text
    toHandle handle' stream'
  where
    Builder{..} = monadBuilder

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

effects :: Control.Monad m => Stream (Of a) m r #-> m r
effects stream = stream & \case
  Return r -> return r
  Effect ms -> ms >>= effects
  Step (_ :> stream') -> effects stream'
  where
    Builder{..} = monadBuilder

erase :: Control.Monad m => Stream (Of a) m r #-> Stream Identity m r
erase stream = stream & \case
  Return r -> Return r
  Step (_ :> stream') -> Step $ Identity (erase stream')
  Effect ms -> Effect $ ms >>= (return . erase)
  where
    Builder{..} = monadBuilder

drained ::
  ( Control.Monad m
  , Control.Monad (t m)
  , Control.Functor (t m)
  , Control.MonadTrans t) =>
  t m (Stream (Of a) m r) #-> t m r
drained = Control.join . Control.fmap (Control.lift . effects)

mapM_ :: (Consumable b, Control.Monad m) => (a -> m b) -> Stream (Of a) m r #-> m r
mapM_  f stream = stream & \case
  Return r -> return r
  Effect ms -> ms >>= mapM_ f
  Step (a :> stream') -> do
    b <- f a
    return $ consume b
    mapM_ f stream'
  where
    Builder{..} = monadBuilder


-- #  Folds
-------------------------------------------------------------------------------

-- | Note: does not short circuit
fold :: Control.Monad m =>
  (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r #-> m (Of b r)
fold f x g stream = stream & \case
  Return r -> return $ g x :> r
  Effect ms -> ms >>= fold f x g
  Step (a :> stream') -> fold f (f x a) g stream'
  where
    Builder{..} = monadBuilder

-- | Note: does not short circuit
fold_ :: (Control.Monad m, Consumable r) =>
  (x -> a -> x) -> x -> (x -> b) -> Stream (Of a) m r #-> m b
fold_ f x g stream = stream & \case
  Return r -> lseq r $ return $ g x
  Effect ms -> ms >>= fold_ f x g
  Step (a :> stream') -> fold_ f (f x a) g stream'
  where
    Builder{..} = monadBuilder

-- Note: We can't use 'Of' since the left component is unrestricted.
-- Remark: to use the (`m x`) in the folding function that is the first
-- argument, we must bind to it. Since `m` is a `Control.Monad`, we need
-- the folding function to consume `x` linearly.
-- | Note: does not short circuit
foldM :: Control.Monad m =>
  (x #-> a -> m x) -> m x -> (x #-> m b) -> Stream (Of a) m r #-> m (b,r)
foldM f mx g stream = stream & \case
  Return r -> mx >>= g >>= (\b -> return (b,r))
  Effect ms -> ms >>= foldM f mx g
  Step (a :> stream') -> foldM f (mx >>= \x -> f x a) g stream'
  where
    Builder{..} = monadBuilder

-- | Note: does not short circuit
foldM_ :: (Control.Monad m, Consumable r) =>
  (x #-> a -> m x) -> m x -> (x #-> m b) -> Stream (Of a) m r #-> m b
foldM_ f mx g stream = stream & \case
  Return r  -> lseq r $ mx >>= g
  Effect ms -> ms >>= foldM_ f mx g
  Step (a :> stream') -> foldM_ f (mx >>= \x -> f x a) g stream'
  where
    Builder{..} = monadBuilder

-- | Note: does not short circuit
all :: Control.Monad m => (a -> Bool) -> Stream (Of a) m r #-> m (Of Bool r)
all f stream = fold (&&) True id (map f stream)

-- | Note: does not short circuit
all_ :: (Consumable r, Control.Monad m) => (a -> Bool) -> Stream (Of a) m r #-> m Bool
all_ f stream = fold_ (&&) True id (map f stream)

-- | Note: does not short circuit
any :: Control.Monad m => (a -> Bool) -> Stream (Of a) m r #-> m (Of Bool r)
any f stream = fold (||) False id (map f stream)

-- | Note: does not short circuit
any_ :: (Consumable r, Control.Monad m) => (a -> Bool) -> Stream (Of a) m r #-> m Bool
any_ f stream = fold_ (||) False id (map f stream)

sum :: (Control.Monad m, Num a) => Stream (Of a) m r #-> m (Of a r)
sum stream = fold (+) 0 id stream

sum_ :: (Control.Monad m, Num a) => Stream (Of a) m () #-> m a
sum_ stream = fold_ (+) 0 id stream

product :: (Control.Monad m, Num a) => Stream (Of a) m r #-> m (Of a r)
product stream = fold (*) 1 id stream

product_ :: (Control.Monad m, Num a) => Stream (Of a) m () #-> m a
product_ stream = fold_ (*) 1 id stream

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

last :: Control.Monad m => Stream (Of a) m r #-> m (Of (Maybe a) r)
last = loop Nothing where
  loop :: Control.Monad m => Maybe a -> Stream (Of a) m r #-> m (Of (Maybe a) r)
  loop m s = s & \case
    Return r  -> return (m :> r)
    Effect m -> m >>= last
    Step (a :> rest) -> loop (Just a) rest

  Builder{..} = monadBuilder

last_ :: (Consumable r, Control.Monad m) => Stream (Of a) m r #-> m (Maybe a)
last_ = loop Nothing where
  loop :: (Consumable r, Control.Monad m) =>
    Maybe a -> Stream (Of a) m r #-> m (Maybe a)
  loop m s = s & \case
    Return r  -> lseq r $ return m
    Effect m -> m >>= last_
    Step (a :> rest) -> loop (Just a) rest

  Builder{..} = monadBuilder

elem :: (Control.Monad m, Eq a) => a -> Stream (Of a) m r #-> m (Of Bool r)
elem a stream = stream & \case
  Return r -> return $ False :> r
  Effect ms -> ms >>= elem a
  Step (a' :> stream') -> case a == a' of
    True -> effects stream' >>= (\r -> return $ True :> r)
    False -> elem a stream'
  where
    Builder{..} = monadBuilder

elem_ :: (Consumable r, Control.Monad m, Eq a) => a -> Stream (Of a) m r #-> m Bool
elem_ a stream = stream & \case
  Return r -> lseq r $ return False
  Effect ms -> ms >>= elem_ a
  Step (a' :> stream') -> case a == a' of
    True -> effects stream' >>= \r -> lseq r $ return True
    False -> elem_ a stream'
  where
    Builder{..} = monadBuilder

notElem :: (Control.Monad m, Eq a) => a -> Stream (Of a) m r #-> m (Of Bool r)
notElem a stream = Control.fmap negate $ elem a stream
  where
    negate :: Of Bool r #-> Of Bool r
    negate (b :> r) = Prelude.not b :> r

notElem_ :: (Consumable r, Control.Monad m, Eq a) => a -> Stream (Of a) m r #-> m Bool
notElem_ a stream = Control.fmap Linear.not $ elem_ a stream

length :: Control.Monad m => Stream (Of a) m r #-> m (Of Int r)
length = fold (\n _ -> n + 1) 0 id

length_ :: (Consumable r, Control.Monad m) => Stream (Of a) m r #-> m Int
length_ = fold_ (\n _ -> n + 1) 0 id

toList :: Control.Monad m => Stream (Of a) m r #-> m (Of [a] r)
toList = fold (Prelude.flip (:)) [] id

toList_ :: Control.Monad m => Stream (Of a) m () #-> m [a]
toList_ stream = fold_ (Prelude.flip (:)) [] id stream

mconcat :: (Control.Monad m, Prelude.Monoid w) => Stream (Of w) m r #-> m (Of w r)
mconcat = fold (Prelude.<>) Prelude.mempty id

mconcat_ :: (Consumable r, Control.Monad m, Prelude.Monoid w) =>
  Stream (Of w) m r #-> m w
mconcat_ = fold_ (Prelude.<>) Prelude.mempty id

minimum :: (Control.Monad m, Ord a) => Stream (Of a) m r #-> m (Of (Maybe a) r)
minimum = fold getMin Nothing id . map Just

minimum_ :: (Consumable r, Control.Monad m, Ord a) =>
  Stream (Of a) m r #-> m (Maybe a)
minimum_ = fold_ getMin Nothing id . map Just

maximum :: (Control.Monad m, Ord a) => Stream (Of a) m r #-> m (Of (Maybe a) r)
maximum = fold getMax Nothing id . map Just

maximum_ :: (Consumable r, Control.Monad m, Ord a) =>
  Stream (Of a) m r #-> m (Maybe a)
maximum_ = fold_ getMax Nothing id . map Just

getMin :: Ord a => Maybe a -> Maybe a -> Maybe a
getMin = mCompare Prelude.min

getMax :: Ord a => Maybe a -> Maybe a -> Maybe a
getMax = mCompare Prelude.max

mCompare :: Ord a => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mCompare comp Nothing Nothing = Nothing
mCompare comp (Just a) Nothing = Just a
mCompare comp Nothing (Just a) = Just a
mCompare comp (Just x) (Just y) = Just $ comp x y

foldrM :: Control.Monad m
       => (a -> m r #-> m r) -> Stream (Of a) m r #-> m r
foldrM step stream = stream & \case
  Return r -> return r
  Effect m -> m >>= foldrM step
  Step (a :> as) -> step a (foldrM step as)
  where
    Builder{..} = monadBuilder

foldrT :: (Control.Monad m, Control.MonadTrans t, Control.Monad (t m)) =>
  (a -> t m r #-> t m r) -> Stream (Of a) m r #-> t m r
foldrT step stream = stream & \case
  Return r -> return r
  Effect ms -> (Control.lift ms) >>= foldrT step
  Step (a :> as) -> step a (foldrT step as)
  where
    Builder{..} = monadBuilder

