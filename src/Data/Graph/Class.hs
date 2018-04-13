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

import qualified Data.Map as M
import qualified Data.Bimap as B
import qualified Data.Set as S
import Control.Arrow ((&&&))

type Vertex = Int

type NodeIndex a   = B.Bimap Vertex a
type AdjacencyList = M.Map Vertex (S.Set Vertex)

data Edge a = Edge (a,a)

class (Eq a, Ord a) => Graph g a where
    empty :: g a

    vertex :: g a -> a -> Vertex
    node :: g a -> Vertex -> a

    nodes :: g a -> [a]
    vertices :: g a -> [Vertex]
    edges :: g a -> [Edge a]

    createEdge :: g a -> Edge a -> g a
    deleteEdge :: g a -> Edge a -> g a

    createNode :: g a -> a -> g a
    deleteNode :: g a -> a -> g a

    createFromEdges :: [Edge a] -> g a
    createFromNodes :: [a] -> g a

    neighbors :: g a -> a -> [a]


data DGraph a = DGraph (NodeIndex a) Vertex AdjacencyList

instance (Eq a, Ord a) => Graph DGraph a where
    empty = DGraph B.empty 0 M.empty

    nodes (DGraph nodeIndex _ _) = B.elems nodeIndex
    vertices (DGraph nodeIndex _ _) = B.keys nodeIndex

    node (DGraph nodeIndex _ _) vertexNo = nodeIndex B.! vertexNo
    vertex (DGraph nodeIndex _ _) node = nodeIndex B.!> node

    edges (DGraph nodeIndex _ adjList) = M.foldrWithKey f [] adjList
        where f vertex neighbours xs = (fx vertex neighbours) ++ xs
              fx vertex neighbours = map (\n -> Edge (nodeIndex B.! vertex, nodeIndex B.! n)) (S.elems neighbours)

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
bfs g sx = bfsFold g [] sx (\x acc -> acc ++ [x]) (\_ acc -> acc)


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

-- Returns list of nodes visited in dfs order
dfs :: (Graph g a) => g a -> [a] -> [a]
dfs g sx = dfsFold g [] sx (\x acc -> acc ++ [x]) (\_ acc -> acc)


------------------------------------------------------------
-- Algorithm 3: Topological Sort
------------------------------------------------------------

--A topological sort of the graph. The order is partially specified by the condition
-- that a vertex i precedes j whenever j is reachable from i but not vice versa.
topologicalSort :: (Graph g a) => g a -> [a]
topologicalSort g = dfs g (nodes g)


------------------------------------------------------------
-- Algorithm 4: Strongly Connected Components
------------------------------------------------------------

--The strongly connected components of a graph.
--scc :: (Graph g a) => g a -> [g a]
--scc g = dfs (transpose g) (dfs g (nodes g))




-- A list of nodes reachable from a given node.
reachable :: (Graph g a) => g a -> a -> [a]
reachable g x = bfsFold g [] [x] (\n acc -> n:acc) (\_ -> id)

--Is the second vertex reachable from the first?
path :: (Graph g a) => g a -> a -> a -> Bool
path g x y = bfsFold g False [x] (\n acc -> acc || n == y) (\_ -> id)

isCyclic :: (Graph g a) => g a -> [a] -> Bool
isCyclic g sx = bfsFold g False sx (\_ -> id) (\_ _ -> True)

--A topological sort of the graph. The order is partially specified by the condition
-- that a vertex i precedes j whenever j is reachable from i but not vice versa.
topSort :: (Graph g a) => g a -> [a]
topSort = undefined

--The connected components of a graph. Two vertices are connected if there is a path
-- between them, traversing edges in either direction.
components :: (Graph g a) => g a -> [g a] --since we do not have tree rep, else [Tree t a]
components = undefined



--The biconnected components of a graph. An undirected graph is
-- biconnected if the deletion of any vertex leaves it connected.
bcc :: (Graph g a) => g a -> [g a]
bcc = undefined

--The graph obtained by reversing all edges.
transposeG :: (Graph g a) => g a -> g a
transposeG = undefined


--How to do this?
----------------------------------------------------------------
--A table of the count of edges from each node.
--outdegree :: Graph -> Table Int

--indegree :: Graph -> Table Int
--A table of the count of edges into each node.




main :: IO ()
main = undefined