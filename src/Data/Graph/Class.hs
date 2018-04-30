{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-|
Module      : Main
Description : Yet Another Haskell Graph Library
Copyright   : (c) Anindo, 2018
                  Bhavin, 2018
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Data.Graph.Class where

import Data.List (minimumBy, delete)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Bimap as B
import qualified Data.Set as S
import qualified Data.FingerTree.PSQueue as PSQ
import Control.Arrow ((&&&))
import Test.HUnit

type Vertex = Int
type Weight = Double
data Edge a = Edge (a,a) Weight

class (Eq a, Ord a) => Graph g a where
    empty :: g a

    vertex :: g a -> a -> Vertex
    node :: g a -> Vertex -> a
    vertices :: g a -> [Vertex]

    nodes :: g a -> [a]
    edges :: g a -> [Edge a]

    createEdge :: g a -> Edge a -> g a
    deleteEdge :: g a -> Edge a -> g a

    createNode :: g a -> a -> g a
    deleteNode :: g a -> a -> g a

    createFromEdges :: [Edge a] -> g a
    createFromNodes :: [a] -> g a

    neighbors :: g a -> a -> [a]
    singleton :: a -> g a
    map :: (Eq b, Ord b) => (a -> b) -> g a -> g b
    transpose :: g a -> g a

    weight :: g a -> a -> a -> Double


type NodeIndex a   = B.Bimap Vertex a
type AdjacencyList = M.Map Vertex (S.Set (Vertex, Weight))

data DGraph a = DGraph (NodeIndex a) Vertex AdjacencyList
        deriving (Show)

instance (Eq a, Ord a) => Graph DGraph a where
    empty = DGraph B.empty 0 M.empty

    nodes (DGraph nodeIndex _ _) = B.elems nodeIndex
    vertices (DGraph nodeIndex _ _) = B.keys nodeIndex

    node (DGraph nodeIndex _ _) vertexNo = nodeIndex B.! vertexNo
    vertex (DGraph nodeIndex _ _) node = nodeIndex B.!> node

    edges (DGraph nodeIndex _ adjList) = M.foldrWithKey f [] adjList
        where f vertex neighbours xs = (fx vertex neighbours) ++ xs
              fx vertex neighbours = Prelude.map (\(d,w) -> Edge (nodeIndex B.! vertex, nodeIndex B.! d) w) (S.elems neighbours)

    createEdge g (Edge (src, dest) w) =
                (DGraph nodeIndex vertexNo adjList')
        where (DGraph nodeIndex vertexNo adjList) = createNode (createNode g src) dest
              (src', dest')  = (nodeIndex B.!> src, nodeIndex B.!> dest)
              adjList' = M.insertWith S.union src' (S.singleton (dest', w)) adjList

    deleteEdge g@(DGraph nodeIndex vertexNo adjList) (Edge (src,dest) w) =
            (DGraph nodeIndex vertexNo adjList')
        where
            adjList'= case (B.lookupR src nodeIndex,  B.lookupR dest nodeIndex) of
                            (Just src', Just dest') -> M.adjust (S.delete (dest', w)) src' adjList
                            _ -> adjList

    createNode g@(DGraph nodeIndex vertexNo adjList) n =
        case B.lookupR n nodeIndex of
            Nothing -> DGraph (B.insert vertexNo n nodeIndex) (vertexNo+1) adjList
            _       -> g

    deleteNode g@(DGraph nodeIndex vertexNo adjList) n =
        case (B.lookupR n nodeIndex) of
            Nothing -> g
            Just v ->
                (DGraph (B.delete v nodeIndex) vertexNo (M.map (S.filter (\(d, w) -> d /= v)) (M.delete v adjList)))

    createFromEdges es = foldl createEdge empty es

    createFromNodes ns = foldl createNode empty ns

    neighbors (DGraph nM _ eM) n = ns
          where
            ns  = case (B.lookupR n nM) of
                    Nothing -> []
                    Just v  -> case (eM M.!? v) of
                                 Nothing -> []
                                 Just s  -> [(nM B.! e) | (e,w) <- S.elems s]

    singleton a = createNode empty a

    map f (DGraph n v a) = (DGraph (B.mapR f n) v a)

    transpose g@(DGraph n v a) = (DGraph n v a')
        where
        a' = foldr (\(Edge (src, dest) w) m -> M.insertWith S.union (n B.!> dest) (S.singleton ((n B.!> src), w)) m) M.empty (edges g)

    weight g@(DGraph n v a) s d = w
            where
            w = case (B.lookupR s n,  B.lookupR d n) of
                        (Just src', Just dest') -> case M.lookup src' a of
                                                      Nothing -> 1/0
                                                      Just sv -> snd (head (S.toList (S.filter (\(d', w) -> dest' == d') sv)))
                        _ -> 1/0


-------------------------------------------------------------------------
--                                                                      -
--      Algorithms
--                                                                      -
-------------------------------------------------------------------------

------------------------------------------------------------
-- Algorithm 1: Breadth first search
------------------------------------------------------------

bfsFold :: (Graph g a) => g a -> b -> [a]           -- source nodes
                                   -> (a -> b -> b) -- visit node
                                   -> (a -> b -> b) -- revisit node
                                   -> b
bfsFold graph acc queue fv fr = bfsFold' graph acc queue fv fr S.empty

bfsFold' :: (Graph g a) => g a -> b -> [a]           -- source nodes
                                    -> (a -> b -> b) -- visit node
                                    -> (a -> b -> b) -- revisit node
                                    -> S.Set a       -- visited nodes
                                    -> b
bfsFold' graph acc queue fv fr vSet =
  case queue of
    []     -> acc
    (x:xs) -> case (S.member x vSet) of
                True  -> bfsFold' graph (fr x acc) xs fv fr vSet
                False -> bfsFold' graph (fv x acc) (xs ++ (neighbors graph x)) fv fr (S.insert x vSet)

-- Returns list of nodes visited in bfs order
bfs :: (Graph g a) => g a -> [a] -> [a]
bfs g sx = bfsFold g [] sx (\x acc -> acc ++ [x]) (\_ -> id)

-- A list of nodes reachable in BFS order from a given node.
reachableBFS :: (Graph g a) => g a -> a -> [a]
reachableBFS g x = bfs g [x]


------------------------------------------------------------
-- Algorithm 2: Depth first search
------------------------------------------------------------

dfsFold :: (Graph g a) => g a -> b -> [a]           -- source nodes
                                   -> (a -> b -> b) -- visit node
                                   -> (a -> b -> b) -- revisit node
                                   -> b
dfsFold graph acc queue fv fr = dfsFold' graph acc queue fv fr S.empty

dfsFold' :: (Graph g a) => g a -> b -> [a]           -- source nodes
                                    -> (a -> b -> b) -- visit node
                                    -> (a -> b -> b) -- revisit node
                                    -> S.Set a       -- visited nodes
                                    -> b
dfsFold' graph acc queue fv fr vSet =
  case queue of
    []     -> acc
    (x:xs) -> case (S.member x vSet) of
                True  -> dfsFold' graph (fr x acc) xs fv fr vSet
                False -> dfsFold' graph (fv x acc) ((neighbors graph x) ++ xs) fv fr (S.insert x vSet)


-- | Returns list of nodes visited in DFS
dfs :: (Graph g a) => g a -> [a] -> [a]
dfs g sx = dfsFold g [] sx (\x acc -> acc ++ [x]) (\_ acc -> acc)

-- Returns list of nodes visited in preorder
preOrd :: (Graph g a) => g a -> [a] -> [a]
preOrd = dfs

-- | Generates nodes in the reverse of a topological sort.
postOrd :: (Graph g a) => g a -> [a]
postOrd = reverse.topoSort

-- A list of nodes reachable in DFS order from a given node.
reachableDFS :: (Graph g a) => g a -> a -> [a]
reachableDFS g x = dfs g [x]


------------------------------------------------------------
-- Algorithm 3: Closest first Search
------------------------------------------------------------

dijkstra :: (Graph g a) => (a -> a -> Double) -> g a -> a -> [(a, Double)]
dijkstra w g s = go (decrease qini s 0)
  where
    qini = PSQ.fromList [(n PSQ.:-> (1/0)) | n <- nodes g]

    --decrease :: PSQ.PSQ k p -> k -> p -> PSQ.PSQ k p
    decrease q k p = case (PSQ.lookup k q) of
                       Nothing -> q
                       Just p' -> if (p < p') then PSQ.adjust (\_-> p) k q else q

    --go :: PSQ.PSQ k p -> [(k, p)]
    go q = case (PSQ.minView q) of
             Just (n1 PSQ.:-> d1, q') -> (n1, d1): (go (foldr (\(n, d) acc -> decrease acc n d) q' ns ))
                 where
                   ns = [(n2, d1 + (w n1 n2)) | n2 <- neighbors g n1]
             Nothing              -> []



-- Returns list of nodes visited in closest first order
cfs :: (Graph g a) => g a -> a -> [a]
cfs g s = Prelude.map fst (dijkstra (weight g) g s)


------------------------------------------------------------
-- Algorithm 4: Topological Sort
------------------------------------------------------------

--A topological sort of the graph. The order is partially specified by the condition
-- that a vertex i precedes j whenever j is reachable from i but not vice versa.
topoSort :: (Graph g a) => g a -> [a]
topoSort g = dfsR (nodes g) [] S.empty
    where
      newN x vSet = filter (\n -> not (S.member n vSet)) (neighbors g x)
      dfsR [] acc _  = acc
      dfsR (s:sx) acc vSet = case (elem s acc) of
                               True  -> dfsR sx acc vSet
                               False -> case (newN s vSet) of
                                          []     -> dfsR sx (s:acc) (S.insert s vSet)
                                          (x:xs) -> dfsR ((x:xs) ++ (s:sx)) acc (S.insert s vSet)



------------------------------------------------------------
-- Algorithm 5: Graph Cycles
------------------------------------------------------------

isTree :: (Graph g a) => g a -> Bool
isTree = undefined --acyclic and components == 1?


-- Returns True if the graph is bipartite, False otherwise
bipartite :: (Graph g a) => g a -> Bool
bipartite g = case (dfsFold g ini (nodes g) fv fr) of
                Nothing -> False
                Just _  -> True
        where
          --ini :: (M.Map a Int, Int)
          ini = Just (M.empty, 0)

          --fv :: (Ord a) => a -> (Bool, M.Map a Int, Int) -> (Bool, M.Map a Int, Int)
          fv = \n acc -> case acc of
                           Nothing      -> Nothing
                           Just (cM, c) -> Just (M.insert n c cM, 1- c)

          --fr :: (Ord a) => a -> (Bool, M.Map a Int, Int) -> (Bool, M.Map a Int, Int)
          fr = \n acc -> case acc of
                           Nothing          -> Nothing
                           Just acc@(cM, c) -> if (cM M.! n == c) then Just acc else Nothing


------------------------------------------------------------
-- Algorithm 6: Strongly Connected Components
------------------------------------------------------------

--The strongly connected components of a graph.
scc :: (Graph g a) => g a -> [g a]
scc = undefined
--scc g = dfs g (reverse (dfs (transpose g) (nodes g)))


--Is the second vertex reachable from the first?
path :: (Graph g a) => g a -> a -> a -> Bool
path g x y = bfsFold g False [x] (\n acc -> acc || n == y) (\_ -> id)


--The connected components of a graph. Two vertices are connected if there is a path
-- between them, traversing edges in either direction.
components :: (Graph g a) => g a -> [g a] --since we do not have tree rep, else [Tree t a]
components = undefined


--The biconnected components of a graph. An undirected graph is
-- biconnected if the deletion of any vertex leaves it connected.
bcc :: (Graph g a) => g a -> [g a]
bcc = undefined

------------------------------------------------------------
-- Algorithm 7: Minimum Spanning Tree
------------------------------------------------------------

prim :: (Graph g a, Ord b) => g a -> (a -> a -> b) -> [(a,a,b)]
prim g w = prim' [n] ns []
    where
      (n:ns) = nodes g
      --prim' :: [a] -> [a] -> [(a,a,b)] -> [(a,a,b)]
      prim' cx [] mst = mst
      prim' cx nx mst = prim' (n2:cx) (delete n2 nx) (e:mst)
          where
            es            = [(n1, n2, (w n1 n2)) | n1 <- cx, n2 <- (neighbors g n1), elem n2 nx]
            e@(n1, n2, d) = minimumBy (comparing (\(_,_,d) -> d)) es


------------------------------------------------------------
--  Unit Tests
------------------------------------------------------------

graph1 :: DGraph Int
graph1 =  createFromEdges [(Edge (1,2) 1), (Edge (1,3) 1), (Edge (1,4) 1),
                             (Edge (2,5) 1), (Edge (2,6) 1),
                             (Edge (4,7) 1), (Edge (4,8) 1),
                             (Edge (5,9) 1), (Edge (5,10) 1),
                             (Edge (7,11) 1), (Edge (7,12) 1)]

graph2 :: DGraph Int
graph2 =  createFromEdges [(Edge (5,7) 1), (Edge (7,3) 1), (Edge (3,11) 1),
                           (Edge (11,8) 1), (Edge (8,2) 1),
                           (Edge (2, 9) 1),(Edge (2,10) 1)]

cyclicG :: DGraph Int
cyclicG =  createFromEdges [(Edge (1,2) 1), (Edge (2,3) 4), (Edge (3,4) 3),
                           (Edge (4,5) 2), (Edge (5,6) 1), (Edge (2,5) 1),
                           (Edge (6,2) 1)]

-- bipartiteG ::

bfsT :: Test
bfsT = "BFS" ~: TestList [
             bfs graph1 [1] ~?= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
             bfs graph1 [1, 5, 7] ~?= [1, 5, 7, 2, 3, 4, 9, 10, 11, 12, 6, 8]
          ]

dfsT :: Test
dfsT = "BFS" ~: TestList [
             dfs graph1 [1] ~?= [1, 2, 5, 9, 10, 6, 3, 4, 7, 11, 12, 8],
             dfs graph1 [5, 7, 1] ~?= [5, 9, 10, 7, 11, 12, 1, 2, 6, 3, 4, 8]
          ]

reachableT :: Test
reachableT = "Reachable" ~: TestList [
             reachableBFS graph1 1 ~?= [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
             reachableBFS graph1 5 ~?= [5, 9, 10],
             reachableDFS graph1 1 ~?= [1, 2, 5, 9, 10, 6, 3, 4, 7, 11, 12, 8],
             reachableDFS graph1 5 ~?= [5, 9, 10]
          ]

topoSortT :: Test
topoSortT = "Topologcal Sort" ~: TestList [
             topoSort graph2 ~?= [5, 7, 3, 11, 8, 2, 10, 9]]

dijkstraT :: Test
dijkstraT = "dijkstra" ~: TestList [
             dijkstra (weight cyclicG) cyclicG 2 ~?= [(2,0.0),(5,1.0),(6,2.0),(3,4.0),(4,7.0),(1,1/0)]
             ]


tests :: IO ()
tests = do
          runTestTT $ TestList [bfsT, dfsT, reachableT, topoSortT, dijkstraT]
          return ()


main :: IO ()
main = undefined