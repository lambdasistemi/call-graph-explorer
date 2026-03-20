-- | Build a 'Graph' from nodes and edges, computing
-- | adjacency maps.
module Graph.Build
  ( buildGraph
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Graph.Types (Edge, Graph, Node, NodeId)

-- | Build a graph from a list of nodes and edges.
-- | Computes forward and backward adjacency maps.
buildGraph :: Array Node -> Array Edge -> Graph
buildGraph nodes edges =
  { nodes: nodeMap
  , edges
  , forward: foldl addForward initAdj edges
  , backward: foldl addBackward initAdj edges
  }
  where
  nodeMap = foldl
    (\m n -> Map.insert n.id n m)
    Map.empty
    nodes

  nodeIds = Map.keys nodeMap

  initAdj = foldl
    (\m k -> Map.insert k Set.empty m)
    Map.empty
    nodeIds

  addForward m e =
    Map.alter
      ( \s ->
          Just
            ( Set.insert e.target
                (fromMaybe Set.empty s)
            )
      )
      e.source
      m

  addBackward m e =
    Map.alter
      ( \s ->
          Just
            ( Set.insert e.source
                (fromMaybe Set.empty s)
            )
      )
      e.target
      m
