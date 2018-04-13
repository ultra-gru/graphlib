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









