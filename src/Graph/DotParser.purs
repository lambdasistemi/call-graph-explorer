-- | Parse calligraphy DOT output into typed graph
-- | nodes and edges (pure PureScript, no FFI).
-- |
-- | Handles multiline labels from --show-line:
-- |   node_56 [label="composedFollowing
-- |   L257",shape=ellipse,style="filled"];
module Graph.DotParser
  ( parseDot
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (hush)
import Data.Int as Int
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
    -- Join continuation lines (L\d+") back onto
    -- the previous line
    joined = joinContinuations
      (String.split (Pattern "\n") dotString)
    result = Array.foldl parseLine
      { nodes: []
      , edges: []
      , currentModule: ""
      }
      joined
  in
    { nodes: result.nodes
    , edges: result.edges
    }

-- | Join lines that are label continuations
-- | (start with L followed by digits) back onto
-- | the previous line.
joinContinuations
  :: Array String -> Array String
joinContinuations lines =
  Array.foldl step { acc: [], prev: "" } lines
    # finalize
  where
  step { acc, prev } line =
    let
      trimmed = String.trim line
    in
      if isContinuation trimmed then
        { acc, prev: prev <> " " <> trimmed }
      else if prev == "" then
        { acc, prev: line }
      else
        { acc: Array.snoc acc prev
        , prev: line
        }

  isContinuation s =
    String.take 1 s == "L"
      && not
        ( String.contains (Pattern "label=") s
        )

  finalize { acc, prev } =
    if prev == "" then acc
    else Array.snoc acc prev

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
        "node_(\\d+)\\s*\\[label=\"([^\"]+)"
        noFlags
    )
  ms <- match re line
  numStr <- join (NEA.index ms 1)
  rawLabel <- join (NEA.index ms 2)
  let
    -- rawLabel may be "name L123" after joining
    { name, lineNum } = parseLabel rawLabel
    shape = extractShape line
    isRounded =
      String.contains (Pattern "rounded") line
    kind = classifyShape shape isRounded
  pure state
    { nodes = Array.snoc state.nodes
        { id: "node_" <> numStr
        , label: name
        , kind
        , module_: state.currentModule
        , line: lineNum
        }
    }

-- | Extract the name and optional line number
-- | from a label like "composedFollowing L257"
-- | or just "composedFollowing".
parseLabel
  :: String
  -> { name :: String, lineNum :: Maybe Int }
parseLabel raw =
  case
    hush (regex "^(.+?)\\s+L(\\d+)" noFlags)
      >>= \re -> match re raw
    of
    Just ms ->
      let
        name = fromMaybe raw
          (join (NEA.index ms 1))
        num = join (NEA.index ms 2)
          >>= Int.fromString
      in
        { name, lineNum: num }
    Nothing -> { name: raw, lineNum: Nothing }

-- | Extract shape from a DOT node line.
extractShape :: String -> String
extractShape line =
  case
    hush (regex "shape=(\\w+)" noFlags)
      >>= \re -> match re line
    of
    Just ms ->
      fromMaybe "ellipse"
        (join (NEA.index ms 1))
    Nothing -> "ellipse"

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
      String.contains
        (Pattern "style=dashed")
        line
        && String.contains
          (Pattern "arrowhead=none")
          line
  if isDashed then Nothing
  else do
    let
      isDotted =
        String.contains
          (Pattern "style=dotted")
          line
      isBack =
        String.contains
          (Pattern "dir=back")
          line
      source =
        if isBack then "node_" <> tgtNum
        else "node_" <> srcNum
      target =
        if isBack then "node_" <> srcNum
        else "node_" <> tgtNum
    pure state
      { edges = Array.snoc state.edges
          { source
          , target
          , isTypeEdge: isDotted
          }
      }

classifyShape :: String -> Boolean -> NodeKind
classifyShape "octagon" _ = Type
classifyShape "box" true = Field
classifyShape "box" false = Constructor
classifyShape _ _ = Function
