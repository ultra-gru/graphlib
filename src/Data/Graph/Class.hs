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

type Vertex = Int

type NodeIndex a   = B.Bimap Vertex a
type AdjacencyList = M.Map Vertex (S.Set Vertex)

data Edge a = Edge (a,a)

class (Eq a, Ord a) => Graph g a where
    empty :: g a

    --TODO: vertex, node, vertices functions to be removed
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
    --TODO: Implement weight method in DGraph
    weight :: g a -> a -> a -> Double


data DGraph a = DGraph (NodeIndex a) Vertex AdjacencyList

instance (Eq a, Ord a) => Graph DGraph a where
    empty = DGraph B.empty 0 M.empty

    nodes (DGraph nodeIndex _ _) = B.elems nodeIndex
    vertices (DGraph nodeIndex _ _) = B.keys nodeIndex

    node (DGraph nodeIndex _ _) vertexNo = nodeIndex B.! vertexNo
    vertex (DGraph nodeIndex _ _) node = nodeIndex B.!> node

    edges (DGraph nodeIndex _ adjList) = M.foldrWithKey f [] adjList
        where f vertex neighbours xs = (fx vertex neighbours) ++ xs
              fx vertex neighbours = Prelude.map (\n -> Edge (nodeIndex B.! vertex, nodeIndex B.! n)) (S.elems neighbours)

    createEdge g (Edge (src, dest)) =
                (DGraph nodeIndex vertexNo adjList')
        where (DGraph nodeIndex vertexNo adjList) = createNode (createNode g src) dest
              (src', dest')  = (nodeIndex B.!> src, nodeIndex B.!> dest)
              adjList' = M.insertWith S.union src' (S.singleton dest') adjList

    deleteEdge g@(DGraph nodeIndex vertexNo adjList) (Edge (src,dest)) =
            (DGraph nodeIndex vertexNo adjList')
        where
            adjList'= case (B.lookupR src nodeIndex,  B.lookupR dest nodeIndex) of
                            (Just src', Just dest') -> M.adjust (S.delete dest') src' adjList
                            _ -> adjList

    createNode g@(DGraph nodeIndex vertexNo adjList) n =
        case B.lookupR n nodeIndex of
            Nothing -> DGraph (B.insert vertexNo n nodeIndex) (vertexNo+1) adjList
            _       -> g

    deleteNode g@(DGraph nodeIndex vertexNo adjList) n =
        case (B.lookupR n nodeIndex) of
            Nothing -> g
            Just v ->
                (DGraph (B.delete v nodeIndex) vertexNo (M.map (S.delete v) (M.delete v adjList)))

    createFromEdges es = foldl createEdge empty es

    createFromNodes ns = foldl createNode empty ns

    neighbors (DGraph nM _ eM) n = ns
          where
            ns  = case (B.lookupR n nM) of
                    Nothing -> []
                    Just v  -> case (eM M.!? v) of
                                 Nothing -> []
                                 Just s  -> [(nM B.! e) | e <- S.elems s]

    singleton a = createNode empty a

    map f (DGraph n v a) = (DGraph (B.mapR f n) v a)

    transpose g@(DGraph n v a) = (DGraph n v a')
        where
        a' = foldr (\(Edge (src, dest)) m -> M.insertWith S.union (n B.!> dest) (S.singleton (n B.!> src)) m) M.empty (edges g)


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


-- Returns list of nodes visited in DFS
dfs :: (Graph g a) => g a -> [a] -> [a]
dfs g sx = dfsFold g [] sx (\x acc -> acc ++ [x]) (\_ acc -> acc)

-- Returns list of nodes visited in preorder
preordering :: (Graph g a) => g a -> [a] -> [a]
preordering = dfs

-- Returns list of nodes visited in preorder
postordering :: (Graph g a) => g a -> [a] -> [a]
postordering g sx = dfsFold g [] sx (\x acc -> x:acc) (\_ acc -> acc)


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
topologicalSort :: (Graph g a) => g a -> [a]
topologicalSort g = reverse (postordering g (nodes g))


------------------------------------------------------------
-- Algorithm 5: Graph Cycles
------------------------------------------------------------

-- Returns True if the graph is acyclic, False otherwise
acyclic :: (Graph g a) => g a -> Bool
acyclic g = bfsFold g True (nodes g) (\_ -> id) (\_ _ -> False)

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
      prim' cx nx mst = prim' (n2:nx) (delete n2 nx) (e:mst)
          where
            es            = [(n1, n2, (w n1 n2)) | n1 <- nx, n2 <- (neighbors g n1)]
            e@(n1, n2, d) = minimumBy (comparing (\(_,_,d) -> d)) es


main :: IO ()
main = undefined