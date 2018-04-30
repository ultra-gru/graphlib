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
    edges :: [(String, String, Double)]
  }

tag name = deep (isElem >>> hasName name)

extractEdge [] = tag "edge" >>>
  proc e -> do
      source <- getAttrValue "source" -< e
      target <- getAttrValue "target" -< e
      returnA -< (source, target, 1.0)

extractEdge [id] = tag "edge" >>>
  proc e -> do
      source <- getAttrValue "source" -< e
      target <- getAttrValue "target" -< e
      weight <- extractWeight id -< e
      returnA -< (source, target, weight)

-- fname <- getText <<< getChildren <<< deep (hasName "fname") -< x

extractWeight id = tag "data" >>> hasAttrValue "key" (\v -> v == id) >>>
    proc k -> do
        w <- getText <<< getChildren -< k
        returnA -< (read w)::Double

weight = (==) "weight"

getWeightKey attr =
  (getAttrValue attr >>> isA weight >>> arr Just)
    `orElse` (constA Nothing)

-- <key id="d1" for="edge" attr.name="weight" attr.type="double"/>
-- <data key="d1">1.1</data>

extractKey for key = tag "graphml" >>> tag "key" >>> hasAttrValue "attr.name" (\v -> v == key)
                       >>> hasAttrValue "for" (\v -> v == for) >>>
    proc k -> do
        id <- getAttrValue "id" -< k
        returnA -< id

extractNode = tag "node" >>>
  proc n -> do
      nodeId <- getAttrValue "id" -< n
      returnA -< nodeId

extractGraph keys = tag "graph" >>>
  proc g -> do
      graphId <- getAttrValue "id" -< g
      nodes <- listA extractNode -< g
      edges <- listA (extractEdge keys) -< g
      returnA -< Graph{graphId=graphId, nodes=nodes, edges=edges}

readGraphMLString :: String -> IO [GraphLib.DGraph String]
readGraphMLString xml = do
    keys <- runX (readString [withValidate no] "graphml2.xml" >>> extractKey "edge" "weight")
    graphs <- runX (readString [withValidate no] xml >>> extractGraph [])
    return (map mkGraph graphs)

readGraphMLFile :: String -> IO [GraphLib.DGraph String]
readGraphMLFile file = do
    keys <- runX (readDocument [withValidate no] "graphml2.xml" >>> extractKey "edge" "weight")
    graphs <- runX (readDocument [withValidate no] file >>> extractGraph keys)
    return (map mkGraph graphs)

writeGraphMLFile :: GraphLib.DGraph String -> String -> IO [XmlTree]
writeGraphMLFile g file = do
      runX (root [] [(graphToXmlTree g)] >>>
                     writeDocument [withIndent yes] file)

mkGraph :: Graph -> GraphLib.DGraph String
mkGraph g = g'
    where
    g' = foldl (GraphLib.createNode) g'' (nodes g)
    g'' = (GraphLib.createFromEdges (map (\(s,t,w) -> GraphLib.Edge (s,t) w) (edges g)))

graphToXmlTree g = mkelem "graphml" mkSchemaAttr
                                    [ mkelem "key" [ sattr "id" "d0", sattr "for" "edge", sattr "attr.name" "weight", sattr "attr.type" "double" ] [],
                                      mkelem "graph" [ sattr "edgedefault" "directed"]
                                        (map mkNodeTag (GraphLib.nodes g)
                                        ++
                                        map (mkEdgeTag "d0") (GraphLib.edges g))
                                    ]
            where
            mkNodeTag n = mkelem "node" [ sattr "id" n ] []
            mkEdgeTag key e@(GraphLib.Edge (source, destination) weight) = mkelem "edge" [ sattr "source" source , sattr "target" destination]
                                                                                         [ mkelem "data" [ sattr "key" key] [txt (show weight)]]
            mkSchemaAttr = [ sattr "xmlns" "http://graphml.graphdrawing.org/xmlns",
                             sattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance",
                             sattr "xsi:schemaLocation" ("http://graphml.graphdrawing.org/xmlns" ++
                                                        "http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd" ++
                                                        "http://graphml.graphdrawing.org/xmlns ") ]


main :: IO ()
main = do
    keys <- runX (readDocument [withValidate no] "graphml2.xml" >>> extractKey "edge" "weight")
    graphs <- runX (readDocument [withValidate no] "graphml2.xml" >>> extractGraph keys)
    let graphLibGraphs = map mkGraph graphs
    writeGraphMLFile (head graphLibGraphs) "graphml.xml"
    print graphLibGraphs


