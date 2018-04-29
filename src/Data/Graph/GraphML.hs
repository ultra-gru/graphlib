{-# LANGUAGE Arrows, NoMonomorphismRestriction, FlexibleContexts #-}
{-|
Module      : Data.Graph.GraphML
Description : Read and write GraphML files.
Copyright   : (c) Anindo, 2018
                  Bhavin, 2018
License     : GPL-3
Maintainer  : as3601@rit.edu, bns8487@rit.edu
Stability   : experimental
Portability : POSIX

Read and write GraphML files.
-}
module Data.Graph.GraphML where

import Text.XML.HXT.Core
import qualified Data.Graph as DataGraph
import qualified Data.Graph.Class as GraphLib

data Graph = Graph
  { graphId :: String,
    nodes :: [String],
    edges :: [(String, String)]
  }

tag name = deep (isElem >>> hasName name)

extractEdge = tag "edge" >>>
  proc e -> do
      source <- getAttrValue "source" -< e
      target <- getAttrValue "target" -< e
      returnA -< (source, target)

extractNode = tag "node" >>>
  proc n -> do
      nodeId <- getAttrValue "id" -< n
      returnA -< nodeId

extractGraph = tag "graph" >>>
  proc g -> do
      graphId <- getAttrValue "id" -< g
      nodes <- listA extractNode -< g
      edges <- listA extractEdge -< g
      returnA -< Graph{graphId=graphId, nodes=nodes, edges=edges}

readGraphMLString :: String -> IO [GraphLib.DGraph String]
readGraphMLString xml = do
    graphs <- runX (readString [withValidate no] xml >>> extractGraph)
    return (map mkGraph graphs)

readGraphMLFile :: String -> IO [GraphLib.DGraph String]
readGraphMLFile file = do
    graphs <- runX (readDocument [withValidate no] file >>> extractGraph)
    return (map mkGraph graphs)

writeGraphMLFile :: GraphLib.DGraph String -> String -> IO [XmlTree]
writeGraphMLFile g file = do
      runX (root [] [(graphToXmlTree g)] >>>
                     writeDocument [withIndent yes] file)

mkGraph :: Graph -> GraphLib.DGraph String
mkGraph g = g'
    where
    g' = foldl (GraphLib.createNode) g'' (nodes g)
    g'' = (GraphLib.createFromEdges (map (\e -> GraphLib.Edge e) (edges g)))

graphToXmlTree g = mkelem "graphml" mkSchemaAttr
                                    [ mkelem "graph" [ sattr "edgedefault" "directed"]
                                        (map mkNodeTag (GraphLib.nodes g)
                                        ++
                                        map mkEdgeTag (GraphLib.edges g))
                                    ]
            where
            mkNodeTag n = mkelem "node" [ sattr "id" n ] []
            mkEdgeTag e@(GraphLib.Edge (source, destination)) = mkelem "edge" [ sattr "source" source , sattr "target" destination] []
            mkSchemaAttr = [ sattr "xmlns" "http://graphml.graphdrawing.org/xmlns",
                             sattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance",
                             sattr "xsi:schemaLocation" ("http://graphml.graphdrawing.org/xmlns" ++
                                                        "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd" ++
                                                        "http://graphml.graphdrawing.org/xmlns ") ]


main :: IO ()
main = do
    graphs <- runX (readDocument [withValidate no] "graphml2.xml" >>> extractGraph)
    let graphLibGraphs = map mkGraph graphs
    writeGraphMLFile (head graphLibGraphs) "graphml.xml"
    print graphLibGraphs


