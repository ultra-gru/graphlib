{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph.Algorithm.BreadthFirstSearch
-- Breadth-first search
----------------------------------------------------------------------------

module Data.Graph.Algorithms.BreadthFirstSearch
  ( bfs
) where
import Data.Graph.Class
import Data.Graph.Algorithms.Traversals
import Data.Set as S


-- Invoked on every vertex before the start of the search.
--initV :: a -> b -> b

-- Invoked on each vertex as it is removed from the queue
--examineV :: a -> b -> b

-- When vertex is put into the queue
--discoverV :: g a -> a -> g a

-- Invoked on out-edge after vertex is removed from queue
--examineE :: Edge a -> b -> b

-- unvisited edge
--treeEdge :: g a -> Edge a -> g a

-- visited edge
--nonTreeEdge :: g a -> Edge a -> g a

-- inqueue nonTreeEdge
--grayTarget :: g a -> Edge a -> g a

-- dequeued nonTreeEdge
--blackTarget :: g a -> Edge a -> g a

-- before blacking a edge
--finishVertex :: g a -> a ->  g a


bfsFold :: (Graph g a) => g a -> b -> [a]           -- source nodes
                                   -> (a -> b -> b) -- visit node
                                   -> (a -> b -> b) -- revisit node
                                   -> b
bfsFold graph acc queue fv fr = bfsFold' graph acc queue fv fr S.empty

bfsFold' :: (Graph g a) => g a -> b -> [a]           -- source nodes
                                    -> (a -> b -> b) -- visit node
                                    -> (a -> b -> b) -- revisit node
                                    -> S.Set a   -- visited nodes
                                    -> b
bfsFold' graph acc queue fv fr vSet =
  case queue of
    []     -> acc
    (x:xs) -> case (S.member x vSet) of
                True  -> bfsFold' graph (fr x acc) xs fv fr vSet
                False -> bfsFold' graph (fv x acc) (xs ++ (neighbors graph x)) fv fr (S.insert x vSet)

--
bfs :: (Graph g a) => g a -> [a] -> [a]
bfs g sx = bfsFold g [] sx (\x acc -> acc ++ [x]) (\_ acc -> acc)


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

--The strongly connected components of a graph.
scc :: (Graph g a) => g a -> [g a]
scc = undefined

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



