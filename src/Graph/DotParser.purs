-- | Parse calligraphy DOT output into typed graph
-- | nodes and edges (pure PureScript, no FFI).
module Graph.DotParser
  ( parseDot
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Graph.Types (Edge, Node, NodeKind(..))

-- | Parse a DOT string from calligraphy into arrays
-- | of nodes and edges.
parseDot
  :: String
  -> { nodes :: Array Node, edges :: Array Edge }
parseDot dotString =
  let
    result = Array.foldl parseLine
      { nodes: [], edges: [], currentModule: "" }
      (String.split (Pattern "\n") dotString)
  in
    { nodes: result.nodes
    , edges: result.edges
    }

type ParseState =
  { nodes :: Array Node
  , edges :: Array Edge
  , currentModule :: String
  }

parseLine :: ParseState -> String -> ParseState
parseLine state line =
  let
    trimmed = String.trim line
  in
    fromMaybe state
      ( tryParseLabel trimmed state
          `alt` tryParseNode trimmed state
          `alt` tryParseEdge trimmed state
      )

alt :: forall a. Maybe a -> Maybe a -> Maybe a
alt (Just x) _ = Just x
alt Nothing y = y

tryParseLabel
  :: String -> ParseState -> Maybe ParseState
tryParseLabel line state = do
  re <- hush
    (regex "^label=\"([^\"]+)\"" noFlags)
  ms <- match re line
  label <- join (NEA.index ms 1)
  pure state { currentModule = label }

tryParseNode
  :: String -> ParseState -> Maybe ParseState
tryParseNode line state = do
  re <- hush
    ( regex
        "node_(\\d+)\\s*\\[label=\"([^\"]+)\"(?:,shape=(\\w+))?"
        noFlags
    )
  ms <- match re line
  numStr <- join (NEA.index ms 1)
  label <- join (NEA.index ms 2)
  let
    shape = fromMaybe "ellipse"
      (join (NEA.index ms 3))
    isRounded =
      String.contains (Pattern "rounded") line
    kind = classifyShape shape isRounded
  pure state
    { nodes = Array.snoc state.nodes
        { id: "node_" <> numStr
        , label
        , kind
        , module_: state.currentModule
        }
    }

tryParseEdge
  :: String -> ParseState -> Maybe ParseState
tryParseEdge line state = do
  re <- hush
    ( regex
        "\"node_(\\d+)\"\\s*->\\s*\"node_(\\d+)\""
        noFlags
    )
  ms <- match re line
  srcNum <- join (NEA.index ms 1)
  tgtNum <- join (NEA.index ms 2)
  let
    isDashed =
      String.contains (Pattern "style=dashed") line
        && String.contains
          (Pattern "arrowhead=none")
          line
  -- dashed + arrowhead=none = parent-child tree
  if isDashed then Nothing
  else do
    let
      isDotted =
        String.contains
          (Pattern "style=dotted")
          line
      isBack =
        String.contains (Pattern "dir=back") line
      source =
        if isBack then "node_" <> tgtNum
        else "node_" <> srcNum
      target =
        if isBack then "node_" <> srcNum
        else "node_" <> tgtNum
    pure state
      { edges = Array.snoc state.edges
          { source, target, isTypeEdge: isDotted }
      }

classifyShape :: String -> Boolean -> NodeKind
classifyShape "octagon" _ = Type
classifyShape "box" true = Field
classifyShape "box" false = Constructor
classifyShape _ _ = Function
