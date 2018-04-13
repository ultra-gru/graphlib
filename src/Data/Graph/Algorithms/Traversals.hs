{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Graph.Algorithms.Traversals where

import Data.Graph.Class

class BFSVisitor g a where
