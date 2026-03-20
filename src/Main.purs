-- | Entry point for the call-graph explorer.
module Main
  ( main
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
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

import FFI.Cytoscape as Cy
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

data Action
  = Initialize
  | SetRepo String
  | SetToken String
  | SetRef String
  | LoadGraph
  | NodeTapped NodeId
  | Focus
  | ShowAll
  | IncDepth
  | DecDepth
  | FitAll

type State =
  { config :: GH.Config
  , fullGraph :: Graph
  , selectedNode :: Maybe NodeId
  , selectedLabel :: Maybe String
  , selectedKind :: Maybe NodeKind
  , selectedModule :: Maybe ModuleName
  , sourceCode :: Maybe String
  , focusDepth :: Int
  , focused :: Boolean
  , loading :: Boolean
  , error :: Maybe String
  }

initialState :: forall i. i -> State
initialState _ =
  { config:
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
        [ HH.input
            [ HP.placeholder "owner/repo"
            , HP.value
                ( state.config.owner
                    <> "/"
                    <> state.config.repo
                )
            , HE.onValueInput SetRepo
            , HP.id "repo-input"
            ]
        , HH.input
            [ HP.placeholder "branch or SHA"
            , HP.value state.config.ref
            , HE.onValueInput SetRef
            , HP.id "ref-input"
            ]
        , HH.input
            [ HP.placeholder "GitHub token"
            , HP.type_ HP.InputPassword
            , HP.value state.config.token
            , HE.onValueInput SetToken
            , HP.id "token-input"
            ]
        , HH.button
            [ HE.onClick \_ -> LoadGraph
            , HP.disabled state.loading
            ]
            [ HH.text
                if state.loading then
                  "Loading..."
                else "Load"
            ]
        , case state.selectedNode of
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
        [ HH.div [ HP.id "cy" ]
            [ if
                Map.isEmpty
                  state.fullGraph.nodes
              then
                HH.p
                  [ HP.id "placeholder" ]
                  [ HH.text
                      "Enter a GitHub repo \
                      \that contains a \
                      \call-graph.dot file."
                  ]
              else HH.text ""
            ]
        , HH.div
            [ HP.id "sidebar" ]
            [ renderSidebar state ]
        ]
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
                    (HH.ClassName "module-path")
                ]
                [ HH.text mod_ ]
        , case state.sourceCode of
            Nothing -> HH.text ""
            Just src ->
              HH.pre
                [ HP.id "source-code" ]
                [ HH.code_ [ HH.text src ] ]
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

parseOwnerRepo
  :: String -> Maybe { owner :: String, repo :: String }
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
  Initialize ->
    liftEffect (Cy.initCytoscape "cy")

  SetRepo value ->
    case parseOwnerRepo value of
      Just { owner, repo } ->
        H.modify_ \s -> s
          { config = s.config
              { owner = owner
              , repo = repo
              }
          }
      Nothing ->
        H.modify_ \s -> s
          { config = s.config
              { owner = value
              , repo = ""
              }
          }

  SetToken value ->
    H.modify_ \s -> s
      { config = s.config
          { token = value }
      }

  SetRef value ->
    H.modify_ \s -> s
      { config = s.config
          { ref = value }
      }

  LoadGraph -> do
    state <- H.get
    H.modify_ _
      { loading = true, error = Nothing }
    result <- liftAff $ GH.fetchFile
      state.config
      "call-graph.dot"
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
          , focused = false
          , loading = false
          , error = Nothing
          }

  NodeTapped nodeId -> do
    state <- H.get
    let
      node = Map.lookup nodeId
        state.fullGraph.nodes
      mod_ = map _.module_ node
    liftEffect $ Cy.markRoot nodeId
    H.modify_ _
      { selectedNode = Just nodeId
      , selectedLabel = map _.label node
      , selectedKind = map _.kind node
      , selectedModule = mod_
      , sourceCode = Nothing
      }
    -- Fetch source file from GitHub
    case mod_ of
      Just path | path /= "" -> do
        cfg <- H.gets _.config
        result <- liftAff
          (GH.fetchFile cfg path)
        case result of
          Right src ->
            H.modify_ _ { sourceCode = Just src }
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

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
