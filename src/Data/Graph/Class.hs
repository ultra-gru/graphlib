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


main :: IO ()
main = undefined