{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Mutable.HashMap (hmbench, getHMInput) where

import Gauge
import qualified System.Random as Random
import qualified System.Random.Shuffle as Random
import Control.DeepSeq (deepseq, force, NFData(..))
import Data.Hashable (Hashable(..), hashWithSalt)
import GHC.Generics (Generic)
import qualified Data.Unrestricted.Linear as Linear
import Data.List (foldl')
import qualified Prelude.Linear as Linear
import Control.Monad.ST (runST, ST)

import qualified Data.HashMap.Mutable.Linear as LMap
import qualified Data.HashMap.Strict as Map
import qualified Data.HashTable.ST.Basic as BasicST
import qualified Data.HashTable.ST.Cuckoo as CuckooST


-- # Exported benchmarks
-------------------------------------------------------------------------------

newtype Key = Key Int

deriving instance Eq Key
deriving instance Ord Key
deriving instance Generic Key
deriving instance NFData Key
instance Hashable Key where
    hash (Key x) =
      x  `hashWithSalt` (154669 :: Int)
    -- Note: salt with a prime

data BenchInput where
  BenchInput ::
    { pairs :: ![(Key, Int)] -- Keys paired with values
    , shuffle1 :: ![Key]
    , shuffle2 :: ![Key]
    , shuffle3 :: ![Key]
    } -> BenchInput

hmbench :: BenchInput -> Benchmark
hmbench inp = bgroup "Comparing Linear Hashmaps"
  [ bgroup "linear-base:Data.HashMap.Mutable.Linear" $
      linear_hashmap inp
  , bgroup "unordered-containers:Data.HashMap.Strict" $
      vanilla_hashmap_strict inp
  , bgroup "hashtables:Data.HashTable.ST.Basic" $
      st_basic inp
  , bgroup "hashtables:Data.HashTable.ST.Cuckoo" $
      st_cuckoo inp
  ]

descriptions :: [String]
descriptions =
  -- By "shuffle" we mean we vary the order we access keys
  [ "Insert x, delete x, repeat for whole range"
  , "Insert all, shuffle, modify all"
  , "Insert all, shuffle, lookup all"
  , "Insert all, shuffle, modify all, shuffle, lookup all"
  , "Insert all, shuffle, modify all, shuffle, modify all, shuffle, lookup all"
  ]


-- # Config
-------------------------------------------------------------------------------

num_keys :: Int
num_keys = 100_000

getHMInput :: IO BenchInput
getHMInput = do
  let keys = map Key $ enumFromTo 1 num_keys
  g0 <- Random.getStdGen
  let (gx,gc) = Random.split g0
  let (ga,gb) = Random.split gx
  let shuff1 = force $ Random.shuffle' keys num_keys ga
  let shuff2 = force $ Random.shuffle' shuff1 num_keys gb
  let shuff3 = force $ Random.shuffle' shuff2 num_keys gc
  g1 <- Random.getStdGen
  let (vals :: [Int]) = Random.randomRs (0,num_keys) g1
  let kv_pairs = force (zip keys vals)
  return $ BenchInput kv_pairs shuff1 shuff2 shuff3

modVal :: Maybe Int -> Maybe Int
modVal Nothing = Nothing
modVal (Just !k)
  | even k = Nothing
  | otherwise = Just $ floor (sqrt (fromIntegral k) :: Float) + (2*k) + 1


-- # Linear Hashmaps
-------------------------------------------------------------------------------

linear_hashmap :: BenchInput -> [Benchmark]
linear_hashmap inp@(BenchInput {pairs=kvs}) =
  [bench1, bench2, bench3, bench4, bench5]
  where
    mkBench ::
      Int ->
      ([(Key,Int)] -> LMap.HashMap Key Int %1-> LMap.HashMap Key Int) ->
      Benchmark
    mkBench n f = bench (descriptions!!n) $ nf
      (\xs -> unur $ LMap.empty num_keys Linear.$ kill Linear.. f xs) kvs

    kill :: LMap.HashMap k v %1-> Linear.Ur ()
    kill hmap = Linear.lseq hmap (Linear.Ur ())

    unur :: Linear.Ur a -> a
    unur (Linear.Ur a) = a

    foldlx :: (b %1-> a -> b) -> [a] -> b %1-> b
    foldlx _ [] !b = b
    foldlx f (a:as) !b = foldlx f as (f b a)

    look :: LMap.HashMap Key Int %1-> Key -> LMap.HashMap Key Int
    look hmap k = LMap.lookup k hmap Linear.& \case
      (Linear.Ur Nothing, hmap0) -> hmap0
      (Linear.Ur (Just v), hmap0) -> Linear.seq (force v) hmap0

    insertDelete ::
      LMap.HashMap Key Int %1-> (Key,Int) -> LMap.HashMap Key Int
    insertDelete hmap (c,v) = LMap.delete c (LMap.insert c v hmap)

    bench1 :: Benchmark
    bench1 = mkBench 0 bench1_

    bench1_ :: [(Key,Int)] -> LMap.HashMap Key Int %1-> LMap.HashMap Key Int
    bench1_ xs = foldlx insertDelete xs

    bench2 :: Benchmark
    bench2 = mkBench 1 bench2_

    bench2_ :: [(Key,Int)] -> LMap.HashMap Key Int %1-> LMap.HashMap Key Int
    bench2_ xs =
        foldlx (Linear.flip (LMap.alter modVal)) (shuffle1 inp) Linear..
        LMap.insertAll xs

    bench3 :: Benchmark
    bench3 = mkBench 2 bench3_

    bench3_ :: [(Key,Int)] -> LMap.HashMap Key Int %1-> LMap.HashMap Key Int
    bench3_ xs =
      foldlx look (shuffle1 inp) Linear..
      LMap.insertAll xs

    bench4 :: Benchmark
    bench4 = mkBench 3 bench4_

    bench4_ :: [(Key,Int)] -> LMap.HashMap Key Int %1-> LMap.HashMap Key Int
    bench4_ xs =
      foldlx look (shuffle2 inp) Linear..
      foldlx (Linear.flip (LMap.alter modVal)) (shuffle1 inp) Linear..
      LMap.insertAll xs

    bench5 :: Benchmark
    bench5 = mkBench 4 bench5_

    bench5_ :: [(Key,Int)] -> LMap.HashMap Key Int %1-> LMap.HashMap Key Int
    bench5_ xs =
      foldlx look (shuffle3 inp) Linear..
      foldlx (Linear.flip (LMap.alter modVal)) (shuffle2 inp) Linear..
      foldlx (Linear.flip (LMap.alter modVal)) (shuffle1 inp) Linear..
      LMap.insertAll xs


-- # Vanilla Hashmaps
-------------------------------------------------------------------------------

vanilla_hashmap_strict :: BenchInput -> [Benchmark]
vanilla_hashmap_strict inp@(BenchInput {pairs=kvs}) =
  [bench1, bench2, bench3, bench4, bench5]
  where
    mkBench ::
      Int ->
      ([(Key,Int)] -> Map.HashMap Key Int -> Map.HashMap Key Int) ->
      Benchmark
    mkBench n f =
      bench (descriptions!!n) $ nf (\xs -> rnf $ f xs Map.empty) kvs

    foldlx :: (b -> a -> b) -> [a] -> b -> b
    foldlx f xs b = foldl' f b xs

    look :: Map.HashMap Key Int -> Key -> Map.HashMap Key Int
    look m k = case m Map.!? k of
      Nothing -> m
      Just v -> deepseq v m

    bench1 :: Benchmark
    bench1 = mkBench 0 $
      \xs hm -> foldl' (\m (k,v) -> Map.delete k (Map.insert k v m)) hm xs

    bench2 :: Benchmark
    bench2 = mkBench 1 $
      \xs ->
        foldlx (flip $ Map.alter modVal) (shuffle1 inp) .
        foldlx (flip $ uncurry Map.insert) xs

    bench3 :: Benchmark
    bench3 = mkBench 2 $
      \xs ->
        foldlx look (shuffle1 inp) .
        foldlx (flip $ uncurry Map.insert) xs

    bench4 :: Benchmark
    bench4 = mkBench 3 $
      \xs ->
        foldlx look (shuffle2 inp) .
        foldlx (flip $ Map.alter modVal) (shuffle1 inp) .
        foldlx (flip $ uncurry Map.insert) xs

    bench5 :: Benchmark
    bench5 = mkBench 4 $
      \xs ->
        foldlx look (shuffle3 inp) .
        foldlx (flip $ Map.alter modVal) (shuffle2 inp) .
        foldlx (flip $ Map.alter modVal) (shuffle1 inp) .
        foldlx (flip $ uncurry Map.insert) xs


-- # ST Basic
-------------------------------------------------------------------------------

st_basic ::  BenchInput -> [Benchmark]
st_basic inp@(BenchInput {pairs=kvs}) =
  [bench1, bench2, bench3, bench4, bench5]
  where
    mkBench ::
      Int ->
      (forall s. [(Key,Int)] -> BasicST.HashTable s Key Int -> ST s ()) ->
      Benchmark
    mkBench n f = bench (descriptions!!n) $ nf
      (\xs -> runST (BasicST.newSized num_keys >>= f xs)) kvs

    look :: BasicST.HashTable s Key Int -> Key -> ST s ()
    look m k = do
      maybeV <- fmap force $ BasicST.lookup m k
      case maybeV of
        Nothing -> return ()
        Just v -> deepseq v (return ())

    bench1 :: Benchmark
    bench1 = mkBench 0 $ \xs hm ->
      mapM_ (\(k,v) -> BasicST.insert hm k v >> BasicST.delete hm k) xs

    bench2 :: Benchmark
    bench2 = mkBench 1 $ \xs hm -> do
      mapM_ (\(k,v) -> BasicST.insert hm k v) xs
      mapM_ (\k -> BasicST.mutate hm k ((,()) . modVal)) (shuffle1 inp)

    bench3 :: Benchmark
    bench3 = mkBench 2 $ \xs hm -> do
      mapM_ (\(k,v) -> BasicST.insert hm k v) xs
      mapM_ (look hm) (shuffle1 inp)

    bench4 :: Benchmark
    bench4 = mkBench 3 $ \xs hm -> do
      mapM_ (\(k,v) -> BasicST.insert hm k v) xs
      mapM_ (\k -> BasicST.mutate hm k ((,()) . modVal)) (shuffle1 inp)
      mapM_ (look hm) (shuffle2 inp)

    bench5 :: Benchmark
    bench5 = mkBench 4 $ \xs hm -> do
      mapM_ (\(k,v) -> BasicST.insert hm k v) xs
      mapM_ (\k -> BasicST.mutate hm k ((,()) . modVal)) (shuffle1 inp)
      mapM_ (\k -> BasicST.mutate hm k ((,()) . modVal)) (shuffle2 inp)
      mapM_ (look hm) (shuffle3 inp)


-- # ST Cuckoo
-------------------------------------------------------------------------------

st_cuckoo ::  BenchInput -> [Benchmark]
st_cuckoo inp@(BenchInput {pairs=kvs}) =
  [bench1, bench2, bench3, bench4, bench5]
  where
    mkBench ::
      Int ->
      (forall s. [(Key,Int)] -> CuckooST.HashTable s Key Int -> ST s ()) ->
      Benchmark
    mkBench n f = bench (descriptions!!n) $ nf
      (\xs -> runST (CuckooST.newSized num_keys >>= f xs)) kvs

    look :: CuckooST.HashTable s Key Int -> Key -> ST s ()
    look m k = do
      maybeV <- fmap force $ CuckooST.lookup m k
      case maybeV of
        Nothing -> return ()
        Just v -> deepseq v (return ())

    bench1 :: Benchmark
    bench1 = mkBench 0 $ \xs hm ->
      mapM_ (\(k,v) -> CuckooST.insert hm k v >> CuckooST.delete hm k) xs

    bench2 :: Benchmark
    bench2 = mkBench 1 $ \xs hm -> do
      mapM_ (\(k,v) -> CuckooST.insert hm k v) xs
      mapM_ (\k -> CuckooST.mutate hm k ((,()) . modVal)) (shuffle1 inp)

    bench3 :: Benchmark
    bench3 = mkBench 2 $ \xs hm -> do
      mapM_ (\(k,v) -> CuckooST.insert hm k v) xs
      mapM_ (look hm) (shuffle1 inp)

    bench4 :: Benchmark
    bench4 = mkBench 3 $ \xs hm -> do
      mapM_ (\(k,v) -> CuckooST.insert hm k v) xs
      mapM_ (\k -> CuckooST.mutate hm k ((,()) . modVal)) (shuffle1 inp)
      mapM_ (look hm) (shuffle2 inp)

    bench5 :: Benchmark
    bench5 = mkBench 4 $ \xs hm -> do
      mapM_ (\(k,v) -> CuckooST.insert hm k v) xs
      mapM_ (\k -> CuckooST.mutate hm k ((,()) . modVal)) (shuffle1 inp)
      mapM_ (\k -> CuckooST.mutate hm k ((,()) . modVal)) (shuffle2 inp)
      mapM_ (look hm) (shuffle3 inp)

