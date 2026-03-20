-- | Core graph types for the call graph explorer.
module Graph.Types
  ( NodeId
  , ModuleName
  , NodeKind(..)
  , Node(..)
  , Edge(..)
  , Graph(..)
  , emptyGraph
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)

-- | Unique identifier for a node.
type NodeId = String

-- | Module name from calligraphy.
type ModuleName = String

-- | Classification of a node by its Haskell entity.
data NodeKind
  = Module
  | Type
  | Function
  | Constructor
  | Field

derive instance eqNodeKind :: Eq NodeKind
derive instance ordNodeKind :: Ord NodeKind

instance showNodeKind :: Show NodeKind where
  show Module = "module"
  show Type = "type"
  show Function = "function"
  show Constructor = "constructor"
  show Field = "field"

-- | A node in the call graph.
type Node =
  { id :: NodeId
  , label :: String
  , kind :: NodeKind
  , module_ :: ModuleName
  }

-- | A directed edge in the call graph.
type Edge =
  { source :: NodeId
  , target :: NodeId
  , isTypeEdge :: Boolean
  }

-- | The full graph: nodes, edges, and adjacency.
type Graph =
  { nodes :: Map NodeId Node
  , edges :: Array Edge
  , forward :: Map NodeId (Set NodeId)
  , backward :: Map NodeId (Set NodeId)
  }

-- | An empty graph.
emptyGraph :: Graph
emptyGraph =
  { nodes: Map.empty
  , edges: []
  , forward: Map.empty
  , backward: Map.empty
  }
