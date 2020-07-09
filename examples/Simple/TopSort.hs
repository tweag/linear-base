{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}


module Simple.TopSort where

import qualified Prelude.Linear as Linear
import Prelude.Linear ((&))
import Data.Unrestricted.Linear
import qualified Data.HashMap.Linear as HMap
import Data.HashMap.Linear (HashMap)
import Data.Bifunctor.Linear (second)
import Data.Maybe.Linear (catMaybes)
import qualified Data.Functor.Linear as Data
import Test.HUnit hiding (Node)


-- # All Tests
-------------------------------------------------------------------------------

test1 :: Test
test1 = topsort [(1,[2,3]), (2, [4]), (3,[4]), (4,[])] ~=?
  [1,2,3,4]

test2 :: Test
test2 = topsort [(5,[2,0]), (4,[0,1]), (0,[]), (2,[3]), (3,[1]), (1,[])] ~=?
  [5,2,3,4,0,1]

test3 :: Test
test3 = topsort
  [ (1,[2]), (2,[4,5]), (3,[9,7]), (4,[7,8,10]), (5,[10]), (6,[10])
  , (7,[]),(8,[]),(9,[]),(10,[])
  ] ~=?
    [1,2,4,8,5,3,9,7,6,10]

topsortTests :: IO Counts
topsortTests = runTestTT  $ TestList [test1, test2, test3]

-- # The topological sort of a DAG
-------------------------------------------------------------------------------

type Node = Int
type InDegGraph = HashMap Node ([Node], Int)

topsort :: [(Node, [Node])] -> [Node]
topsort = reverse . postOrder . fmap (  \(n,nbrs) -> (n,(nbrs,0))  )
  where
    postOrder :: [(Node, ([Node], Int))] -> [Node]
    postOrder [] = []
    postOrder (x : xs) = let nodes = map fst (x:xs) in
      HMap.singleton' x $ \hm -> postOrderHM nodes (HMap.insertAll xs hm)


postOrderHM :: [Node] -> InDegGraph #-> Unrestricted [Node]
postOrderHM nodes dag = findSources nodes (computeInDeg nodes dag) & \case
  (dag, Unrestricted sources) -> pluckSources sources [] dag
 where
   -- O(V + N)
  computeInDeg :: [Node] -> InDegGraph #-> InDegGraph
  computeInDeg nodes dag = Linear.foldl incChildren dag nodes

  -- Increment in-degree of all neighbors
  incChildren :: InDegGraph #-> Node -> InDegGraph
  incChildren dag node = HMap.lookup dag node & \case
     (dag, Unrestricted Nothing) -> dag
     (dag, Unrestricted (Just (xs,i))) -> incNodes (move xs) dag
    where
      incNodes :: Unrestricted [Node] #-> InDegGraph #-> InDegGraph
      incNodes (Unrestricted ns) dag = Linear.foldl incNode dag ns

      incNode :: InDegGraph #-> Node -> InDegGraph
      incNode dag node =
        HMap.alter dag (\(Just (n,d)) -> Just (n,d+1)) node


-- pluckSources sources postOrdSoFar dag
pluckSources :: [Node] -> [Node] -> InDegGraph #-> Unrestricted [Node]
pluckSources [] postOrd dag = lseq dag (move postOrd)
pluckSources (s:ss) postOrd dag = HMap.lookup dag s & \case
  (dag, Unrestricted Nothing) -> pluckSources ss (s:postOrd) dag
  (dag, Unrestricted (Just (xs,i))) -> walk xs dag & \case
      (dag', Unrestricted newSrcs) ->
        pluckSources (newSrcs ++ ss) (s:postOrd) dag'
  where
    -- decrement degree of children, save newly made sources
    walk :: [Node] -> InDegGraph #-> (InDegGraph, Unrestricted [Node])
    walk children dag =
      second (Data.fmap catMaybes) (mapAccum decDegree children dag)

    -- Decrement the degree of a node, save it if it is now a source
    decDegree :: Node -> InDegGraph #-> (InDegGraph, Unrestricted (Maybe Node))
    decDegree node dag =
      checkSource node (HMap.alter dag (\(Just (n,d)) -> Just (n,d-1)) node)


-- Given a list of nodes, determines which are sources
findSources :: [Node] -> InDegGraph #-> (InDegGraph, Unrestricted [Node])
findSources nodes dag =
  second (Data.fmap catMaybes) (mapAccum checkSource nodes dag)


-- | Check if a node is a source, and if so return it
checkSource :: Node -> InDegGraph #-> (InDegGraph, Unrestricted (Maybe Node))
checkSource node dag = HMap.lookup dag node & \case
  (dag, Unrestricted Nothing) -> (dag, Unrestricted Nothing)
  (dag, Unrestricted (Just (xs,0))) ->  (dag, Unrestricted (Just node))
  (dag, Unrestricted (Just (xs,n))) -> (dag, Unrestricted Nothing)


mapAccum ::
  (a -> b #-> (b, Unrestricted c)) -> [a] -> b #-> (b, Unrestricted [c])
mapAccum f [] b =  (b, Unrestricted [])
mapAccum f (x:xs) b = mapAccum f xs b & \case
  (b, Unrestricted cs) -> second (Data.fmap (:cs)) (f x b)

