-- | Convert a Graph to Cytoscape.js JSON elements.
module Graph.Cytoscape
  ( toElements
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.String (Pattern(..))
import Data.String as String
import Foreign (Foreign, unsafeToForeign)
import Graph.Types (Edge, Graph, Node)

-- | Convert a graph to a Cytoscape.js elements
-- | array (Foreign). Adds module compound nodes
-- | as parents.
toElements :: Graph -> Foreign
toElements graph = unsafeToForeign
  (moduleEls <> nodeEls <> edgeEls)
  where
  allNodes = Array.fromFoldable
    (Map.values graph.nodes)

  moduleNames = Array.nub
    $ Array.filter (_ /= "")
    $ map _.module_ allNodes

  moduleEls = map mkModuleEl moduleNames

  nodeEls = map mkNodeEl allNodes

  edgeEls = map mkEdgeEl graph.edges

mkModuleEl :: String -> Foreign
mkModuleEl modName = unsafeToForeign
  { group: "nodes"
  , data:
      { id: modId modName
      , label: modName
      }
  , classes: "module"
  }

mkNodeEl :: Node -> Foreign
mkNodeEl node = unsafeToForeign
  { group: "nodes"
  , data:
      { id: node.id
      , label: node.label
      , kind: show node.kind
      , module: node.module_
      , parent:
          if node.module_ == "" then ""
          else modId node.module_
      }
  , classes: show node.kind
  }

mkEdgeEl :: Edge -> Foreign
mkEdgeEl edge = unsafeToForeign
  { group: "edges"
  , data:
      { id: "e_" <> edge.source <> "_"
          <> edge.target
      , source: edge.source
      , target: edge.target
      }
  , classes:
      if edge.isTypeEdge then "type-edge"
      else ""
  }

modId :: String -> String
modId name = "mod_"
  <> String.joinWith "_"
    (String.split (Pattern ".") name)
