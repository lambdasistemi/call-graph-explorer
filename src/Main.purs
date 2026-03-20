-- | Entry point for the call-graph explorer.
module Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class
  ( decodeJson
  )
import Data.Bifunctor (lmap)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff.Class
  ( class MonadAff
  , liftAff
  )
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage as WS

import FFI.Cytoscape as Cy
import FFI.Highlight as HL
import FFI.Resize as Resize
import GitHub as GH
import Graph.Build (buildGraph)
import Graph.Cytoscape (toElements)
import Graph.DotParser as Dot
import Graph.Operations
  ( neighborhood
  , subgraph
  )
import Graph.Types
  ( Graph
  , ModuleName
  , NodeId
  , NodeKind
  , emptyGraph
  )

-- | A saved repository entry.
type RepoEntry =
  { owner :: String
  , repo :: String
  , ref :: String
  , token :: String
  }

data Action
  = Initialize
  | SetRepo String
  | SetToken String
  | SetRef String
  | AddRepo
  | LoadRepo RepoEntry
  | RemoveRepo RepoEntry
  | NodeTapped NodeId
  | Focus
  | ShowAll
  | IncDepth
  | DecDepth
  | FitAll
  | SelectFromHistory NodeId
  | RemoveFromHistory NodeId
  | ClearHistory

type State =
  { repoInput :: String
  , refInput :: String
  , tokenInput :: String
  , repos :: Array RepoEntry
  , activeRepo :: Maybe RepoEntry
  , config :: GH.Config
  , fullGraph :: Graph
  , selectedNode :: Maybe NodeId
  , selectedLabel :: Maybe String
  , selectedKind :: Maybe NodeKind
  , selectedModule :: Maybe ModuleName
  , sourceCode :: Maybe String
  , history ::
      Array
        { nodeId :: NodeId
        , label :: String
        , kind :: NodeKind
        }
  , focusDepth :: Int
  , focused :: Boolean
  , loading :: Boolean
  , error :: Maybe String
  }

initialState :: forall i. i -> State
initialState _ =
  { repoInput: ""
  , refInput: "main"
  , tokenInput: ""
  , repos: []
  , activeRepo: Nothing
  , config:
      { owner: ""
      , repo: ""
      , ref: "main"
      , token: ""
      }
  , fullGraph: emptyGraph
  , selectedNode: Nothing
  , selectedLabel: Nothing
  , selectedKind: Nothing
  , selectedModule: Nothing
  , sourceCode: Nothing
  , history: []
  , focusDepth: 1
  , focused: false
  , loading: false
  , error: Nothing
  }

component
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
    }

render
  :: forall m
   . State
  -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.div
        [ HP.id "toolbar" ]
        [ case state.selectedNode of
            Nothing -> HH.text ""
            Just _ ->
              if state.focused then
                HH.button
                  [ HE.onClick \_ -> ShowAll
                  ]
                  [ HH.text "Show All" ]
              else
                HH.button
                  [ HE.onClick \_ -> Focus ]
                  [ HH.text "Focus" ]
        , if state.focused then
            HH.span
              [ HP.id "depth-ctrl" ]
              [ HH.button
                  [ HE.onClick \_ -> DecDepth
                  ]
                  [ HH.text "-" ]
              , HH.span_
                  [ HH.text
                      ( "Depth: "
                          <> show
                            state.focusDepth
                      )
                  ]
              , HH.button
                  [ HE.onClick \_ -> IncDepth
                  ]
                  [ HH.text "+" ]
              ]
          else HH.text ""
        , HH.button
            [ HE.onClick \_ -> FitAll ]
            [ HH.text "Fit" ]
        ]
    , case state.error of
        Nothing -> HH.text ""
        Just err ->
          HH.div
            [ HP.id "error-bar" ]
            [ HH.text err ]
    , HH.div [ HP.id "main" ]
        [ HH.div
            [ HP.id "repo-panel" ]
            [ renderRepoForm state
            , renderRepoList state
            ]
        , HH.div [ HP.id "cy" ]
            [ if
                Map.isEmpty
                  state.fullGraph.nodes
              then
                HH.p
                  [ HP.id "placeholder" ]
                  [ HH.text
                      "Add a repo to get \
                      \started."
                  ]
              else HH.text ""
            ]
        , HH.div
            [ HP.id "resize-handle" ]
            []
        , HH.div
            [ HP.id "sidebar" ]
            [ renderHistory state
            , renderSidebar state
            ]
        ]
    ]

renderRepoForm
  :: forall m
   . State
  -> H.ComponentHTML Action () m
renderRepoForm state =
  HH.div
    [ HP.id "repo-form" ]
    [ HH.input
        [ HP.placeholder "owner/repo"
        , HP.value state.repoInput
        , HE.onValueInput SetRepo
        ]
    , HH.input
        [ HP.placeholder "ref (main)"
        , HP.value state.refInput
        , HE.onValueInput SetRef
        ]
    , HH.input
        [ HP.placeholder "token"
        , HP.type_ HP.InputPassword
        , HP.value state.tokenInput
        , HE.onValueInput SetToken
        ]
    , HH.button
        [ HE.onClick \_ -> AddRepo
        , HP.disabled state.loading
        ]
        [ HH.text
            if state.loading then "..."
            else "Add"
        ]
    ]

renderRepoList
  :: forall m
   . State
  -> H.ComponentHTML Action () m
renderRepoList state =
  HH.div
    [ HP.id "repo-list" ]
    ( map (renderRepoItem state) state.repos )

renderRepoItem
  :: forall m
   . State
  -> RepoEntry
  -> H.ComponentHTML Action () m
renderRepoItem state entry =
  let
    isActive = state.activeRepo == Just entry
    label = entry.owner <> "/" <> entry.repo
      <> "@" <> entry.ref
  in
    HH.div
      [ HP.class_
          ( HH.ClassName
              ( "repo-item"
                  <> if isActive then " active"
                    else ""
              )
          )
      ]
      [ HH.span
          [ HP.class_
              (HH.ClassName "repo-label")
          , HE.onClick \_ -> LoadRepo entry
          ]
          [ HH.text label ]
      , HH.button
          [ HP.class_
              (HH.ClassName "repo-remove")
          , HE.onClick \_ -> RemoveRepo entry
          ]
          [ HH.text "x" ]
      ]

renderHistory
  :: forall m
   . State
  -> H.ComponentHTML Action () m
renderHistory state =
  if Array.null state.history then
    HH.text ""
  else
    HH.div
      [ HP.id "history" ]
      [ HH.div
          [ HP.id "history-header" ]
          [ HH.span_
              [ HH.text "Selection history"
              ]
          , HH.button
              [ HE.onClick \_ ->
                  ClearHistory
              ]
              [ HH.text "Clear" ]
          ]
      , HH.div
          [ HP.id "history-list" ]
          ( map renderHistoryItem
              state.history
          )
      ]

renderHistoryItem
  :: forall m
   . { nodeId :: NodeId
     , label :: String
     , kind :: NodeKind
     }
  -> H.ComponentHTML Action () m
renderHistoryItem item =
  HH.div
    [ HP.class_
        (HH.ClassName "history-item")
    ]
    [ HH.span
        [ HP.class_
            ( HH.ClassName
                ( "kind-dot kind-"
                    <> show item.kind
                )
            )
        ]
        []
    , HH.span
        [ HP.class_
            (HH.ClassName "history-label")
        , HE.onClick \_ ->
            SelectFromHistory item.nodeId
        ]
        [ HH.text item.label ]
    , HH.button
        [ HP.class_
            (HH.ClassName "history-remove")
        , HE.onClick \_ ->
            RemoveFromHistory item.nodeId
        ]
        [ HH.text "x" ]
    ]

renderSidebar
  :: forall m
   . State
  -> H.ComponentHTML Action () m
renderSidebar state =
  case state.selectedLabel of
    Nothing ->
      HH.p_
        [ HH.text
            "Click a node to see \
            \its source code."
        ]
    Just label ->
      HH.div_
        [ HH.h3_
            [ HH.text label ]
        , case state.selectedKind of
            Nothing -> HH.text ""
            Just kind ->
              HH.span
                [ HP.class_
                    ( HH.ClassName
                        ( "kind-badge kind-"
                            <> show kind
                        )
                    )
                ]
                [ HH.text (show kind) ]
        , case state.selectedModule of
            Nothing -> HH.text ""
            Just mod_ ->
              HH.p
                [ HP.class_
                    ( HH.ClassName
                        "module-path"
                    )
                ]
                [ HH.text mod_ ]
        , case state.sourceCode of
            Nothing -> HH.text ""
            Just src ->
              HH.pre
                [ HP.id "source-code" ]
                [ HH.code
                    [ HP.class_
                        ( HH.ClassName
                            "language-haskell"
                        )
                    ]
                    [ HH.text src ]
                ]
        ]

subscribeTaps
  :: forall o m
   . MonadAff m
  => H.HalogenM State Action () o m Unit
subscribeTaps = do
  { emitter, listener } <- liftEffect
    HS.create
  liftEffect $ Cy.onNodeTap
    \nodeId -> HS.notify listener
      (NodeTapped nodeId)
  void $ H.subscribe emitter

saveParam :: String -> String -> Effect Unit
saveParam key value = do
  w <- window
  s <- localStorage w
  WS.setItem ("cge-" <> key) value s

loadParam :: String -> Effect (Maybe String)
loadParam key = do
  w <- window
  s <- localStorage w
  WS.getItem ("cge-" <> key) s

-- | Save repo list as JSON to localStorage.
saveRepos :: Array RepoEntry -> Effect Unit
saveRepos repos =
  saveParam "repos" (stringify (encodeJson repos))

-- | Restore repo list from localStorage.
restoreRepos :: Effect (Array RepoEntry)
restoreRepos = do
  raw <- loadParam "repos"
  pure $ case raw of
    Nothing -> []
    Just json ->
      case decodeRepos json of
        Right repos -> repos
        Left _ -> []

decodeRepos
  :: String
  -> Either String (Array RepoEntry)
decodeRepos json =
  jsonParser json >>= \j ->
    lmap show
      (decodeJson j :: Either _ (Array RepoEntry))

parseOwnerRepo
  :: String
  -> Maybe
       { owner :: String
       , repo :: String
       }
parseOwnerRepo s =
  case split (Pattern "/") s of
    [ owner, repo ]
      | owner /= "" && repo /= "" ->
          Just { owner, repo }
    _ -> Nothing

handleAction
  :: forall o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    liftEffect (Cy.initCytoscape "cy")
    liftEffect Resize.initResize
    repos <- liftEffect restoreRepos
    H.modify_ _ { repos = repos }

  SetRepo value ->
    H.modify_ _ { repoInput = value }

  SetToken value ->
    H.modify_ _ { tokenInput = value }

  SetRef value ->
    H.modify_ _ { refInput = value }

  AddRepo -> do
    state <- H.get
    case parseOwnerRepo state.repoInput of
      Nothing -> H.modify_ _
        { error = Just
            "Enter owner/repo format"
        }
      Just { owner, repo } -> do
        let
          entry =
            { owner
            , repo
            , ref: state.refInput
            , token: state.tokenInput
            }
          exists = Array.any
            ( \r ->
                r.owner == owner
                  && r.repo == repo
                  && r.ref == entry.ref
            )
            state.repos
          newRepos =
            if exists then state.repos
            else Array.snoc state.repos entry
        H.modify_ _
          { repos = newRepos
          , repoInput = ""
          , refInput = "main"
          , error = Nothing
          }
        liftEffect (saveRepos newRepos)
        handleAction (LoadRepo entry)

  LoadRepo entry -> do
    let
      cfg =
        { owner: entry.owner
        , repo: entry.repo
        , ref: entry.ref
        , token: entry.token
        }
    H.modify_ _
      { config = cfg
      , activeRepo = Just entry
      , loading = true
      , error = Nothing
      }
    result <- liftAff
      (GH.fetchFile cfg "call-graph.dot")
    case result of
      Left err -> H.modify_ _
        { loading = false
        , error = Just err
        }
      Right dot -> do
        let
          parsed = Dot.parseDot dot
          graph = buildGraph
            parsed.nodes
            parsed.edges
        liftEffect $ Cy.setElements
          (toElements graph)
        subscribeTaps
        H.modify_ _
          { fullGraph = graph
          , selectedNode = Nothing
          , selectedLabel = Nothing
          , selectedKind = Nothing
          , selectedModule = Nothing
          , sourceCode = Nothing
          , history = []
          , focused = false
          , loading = false
          , error = Nothing
          }

  RemoveRepo entry -> do
    state <- H.get
    let
      newRepos = Array.filter
        ( \r ->
            not
              ( r.owner == entry.owner
                  && r.repo == entry.repo
                  && r.ref == entry.ref
              )
        )
        state.repos
    H.modify_ _ { repos = newRepos }
    liftEffect (saveRepos newRepos)

  NodeTapped nodeId -> do
    state <- H.get
    let
      node = Map.lookup nodeId
        state.fullGraph.nodes
      mod_ = map _.module_ node
      nodeLine = node >>= _.line
    liftEffect $ Cy.markRoot nodeId
    let
      newHistory = case node of
        Just n ->
          addToHistory
            { nodeId
            , label: n.label
            , kind: n.kind
            }
            state.history
        Nothing -> state.history
    H.modify_ _
      { selectedNode = Just nodeId
      , selectedLabel = map _.label node
      , selectedKind = map _.kind node
      , selectedModule = mod_
      , sourceCode = Nothing
      , history = newHistory
      }
    case mod_ of
      Just path | path /= "" -> do
        cfg <- H.gets _.config
        result <- liftAff
          (GH.fetchFile cfg path)
        case result of
          Right src -> do
            H.modify_ _
              { sourceCode = Just
                  ( extractSnippet
                      nodeLine
                      src
                  )
              }
            liftEffect HL.highlightCode
          Left _ -> pure unit
      _ -> pure unit

  Focus -> do
    state <- H.get
    case state.selectedNode of
      Nothing -> pure unit
      Just root -> do
        let
          keep = neighborhood
            state.focusDepth
            root
            state.fullGraph
          sub = subgraph keep
            state.fullGraph
        liftEffect $ Cy.setElements
          (toElements sub)
        liftEffect $ Cy.markRoot root
        H.modify_ _ { focused = true }

  ShowAll -> do
    state <- H.get
    liftEffect $ Cy.setElements
      (toElements state.fullGraph)
    case state.selectedNode of
      Nothing -> pure unit
      Just root ->
        liftEffect $ Cy.markRoot root
    H.modify_ _ { focused = false }

  IncDepth -> do
    state <- H.get
    let d = min 5 (state.focusDepth + 1)
    H.modify_ _ { focusDepth = d }
    when state.focused do
      handleAction Focus

  DecDepth -> do
    state <- H.get
    let d = max 1 (state.focusDepth - 1)
    H.modify_ _ { focusDepth = d }
    when state.focused do
      handleAction Focus

  FitAll -> liftEffect Cy.fitAll

  SelectFromHistory nodeId ->
    handleAction (NodeTapped nodeId)

  RemoveFromHistory nodeId ->
    H.modify_ \s -> s
      { history = Array.filter
          (\i -> i.nodeId /= nodeId)
          s.history
      }

  ClearHistory ->
    H.modify_ _ { history = [] }

-- | Add a node to history, avoiding duplicates.
-- | Most recent at the top.
addToHistory
  :: { nodeId :: NodeId
     , label :: String
     , kind :: NodeKind
     }
  -> Array
       { nodeId :: NodeId
       , label :: String
       , kind :: NodeKind
       }
  -> Array
       { nodeId :: NodeId
       , label :: String
       , kind :: NodeKind
       }
addToHistory item history =
  Array.cons item
    ( Array.filter
        (\i -> i.nodeId /= item.nodeId)
        history
    )

-- | Extract a code snippet around the given line.
-- | Shows 3 lines before and continues until the
-- | next top-level definition (line starting at
-- | column 0 with a non-space character) or 30
-- | lines, whichever comes first.
extractSnippet :: Maybe Int -> String -> String
extractSnippet Nothing src = src
extractSnippet (Just targetLine) src =
  let
    allLines = String.split (Pattern "\n") src
    -- 0-indexed start, show 5 lines before
    start = max 0 (targetLine - 6)
    afterTarget = Array.drop targetLine allLines
    -- Find end: next top-level def or 50 lines
    bodyLen = fromMaybe 50
      ( Array.findIndex isTopLevel
          (Array.drop 1 afterTarget)
          <#> (_ + 1)
      )
    end = min (Array.length allLines)
      (targetLine + bodyLen)
    snippet = Array.slice start end allLines
  in
    String.joinWith "\n" snippet

-- | A line is a top-level definition if it starts
-- | with a non-space, non-empty character and is
-- | not a comment or pragma.
isTopLevel :: String -> Boolean
isTopLevel line =
  case SCU.charAt 0 line of
    Nothing -> false
    Just c ->
      c /= ' '
        && c /= '\t'
        && c /= '-'
        && c /= '{'
        && c /= '\n'

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
