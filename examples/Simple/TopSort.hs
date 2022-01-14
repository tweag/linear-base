{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Simple.TopSort where

import Data.Bifunctor.Linear (second)
import qualified Data.Functor.Linear as Data
import Data.HashMap.Mutable.Linear (HashMap)
import qualified Data.HashMap.Mutable.Linear as HMap
import Data.Maybe.Linear (catMaybes)
import Data.Unrestricted.Linear
import Prelude.Linear ((&))
import qualified Prelude.Linear as Linear

-- # The topological sort of a DAG
-------------------------------------------------------------------------------

type Node = Int

type InDegGraph = HashMap Node ([Node], Int)

topsort :: [(Node, [Node])] -> [Node]
topsort = reverse . postOrder . fmap (\(n, nbrs) -> (n, (nbrs, 0)))
  where
    postOrder :: [(Node, ([Node], Int))] -> [Node]
    postOrder [] = []
    postOrder (xs) =
      let nodes = map fst xs
       in unur Linear.$
            HMap.empty (length xs * 2) Linear.$
              \hm -> postOrderHM nodes (HMap.insertAll xs hm)

postOrderHM :: [Node] -> InDegGraph %1 -> Ur [Node]
postOrderHM nodes dag =
  findSources nodes (computeInDeg nodes dag) & \case
    (dag, Ur sources) -> pluckSources sources [] dag
  where
    -- O(V + N)
    computeInDeg :: [Node] -> InDegGraph %1 -> InDegGraph
    computeInDeg nodes dag = Linear.foldl incChildren dag (map Ur nodes)

    -- Increment in-degree of all neighbors
    incChildren :: InDegGraph %1 -> Ur Node %1 -> InDegGraph
    incChildren dag (Ur node) =
      HMap.lookup node dag & \case
        (Ur Nothing, dag) -> dag
        (Ur (Just (xs, i)), dag) -> incNodes (move xs) dag
      where
        incNodes :: Ur [Node] %1 -> InDegGraph %1 -> InDegGraph
        incNodes (Ur ns) dag = Linear.foldl incNode dag (map Ur ns)

        incNode :: InDegGraph %1 -> Ur Node %1 -> InDegGraph
        incNode dag (Ur node) =
          HMap.lookup node dag & \case
            (Ur Nothing, dag') -> dag'
            (Ur (Just (n, d)), dag') ->
              HMap.insert node (n, d + 1) dag'

-- HMap.alter dag (\(Just (n,d)) -> Just (n,d+1)) node

-- pluckSources sources postOrdSoFar dag
pluckSources :: [Node] -> [Node] -> InDegGraph %1 -> Ur [Node]
pluckSources [] postOrd dag = lseq dag (move postOrd)
pluckSources (s : ss) postOrd dag =
  HMap.lookup s dag & \case
    (Ur Nothing, dag) -> pluckSources ss (s : postOrd) dag
    (Ur (Just (xs, i)), dag) ->
      walk xs dag & \case
        (dag', Ur newSrcs) ->
          pluckSources (newSrcs ++ ss) (s : postOrd) dag'
  where
    -- decrement degree of children, save newly made sources
    walk :: [Node] -> InDegGraph %1 -> (InDegGraph, Ur [Node])
    walk children dag =
      second (Data.fmap catMaybes) (mapAccum decDegree children dag)

    -- Decrement the degree of a node, save it if it is now a source
    decDegree :: Node -> InDegGraph %1 -> (InDegGraph, Ur (Maybe Node))
    decDegree node dag =
      HMap.lookup node dag & \case
        (Ur Nothing, dag') -> (dag', Ur Nothing)
        (Ur (Just (n, d)), dag') ->
          checkSource node (HMap.insert node (n, d - 1) dag')

-- Given a list of nodes, determines which are sources
findSources :: [Node] -> InDegGraph %1 -> (InDegGraph, Ur [Node])
findSources nodes dag =
  second (Data.fmap catMaybes) (mapAccum checkSource nodes dag)

-- | Check if a node is a source, and if so return it
checkSource :: Node -> InDegGraph %1 -> (InDegGraph, Ur (Maybe Node))
checkSource node dag =
  HMap.lookup node dag & \case
    (Ur Nothing, dag) -> (dag, Ur Nothing)
    (Ur (Just (xs, 0)), dag) -> (dag, Ur (Just node))
    (Ur (Just (xs, n)), dag) -> (dag, Ur Nothing)

mapAccum ::
  (a -> b %1 -> (b, Ur c)) -> [a] -> b %1 -> (b, Ur [c])
mapAccum f [] b = (b, Ur [])
mapAccum f (x : xs) b =
  mapAccum f xs b & \case
    (b, Ur cs) -> second (Data.fmap (: cs)) (f x b)
