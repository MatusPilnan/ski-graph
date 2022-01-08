module Menus.Menus exposing (..)

import Dict exposing (Dict)
import Geometry as Geom
import Graph
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Icons
import Json.Decode

type ContextMenuVariant
  = EdgeContextMenu Graph.Edge
  | VertexContextMenu Graph.Vertex

type MainMenuView
  = Default
  | EdgeList
  | VertexList
  | EdgeDetail Graph.Edge
  | VertexDetail Graph.Vertex
  | ConfirmDeleteVertex Graph.Vertex
  | ConfirmDeleteEdge Graph.Edge


type alias ContextMenu =
  { position : Geom.ViewportPoint
  , variant : ContextMenuVariant
  }


type alias Model =
  { sideMenuShown : Bool
  , backgroundOpacity : Float
  , mainMenuView : MainMenuView
  , highlightedEdges : Dict Graph.EdgeID Bool
  , highlightedVertices : Dict Graph.VertexID Bool
  , contextMenu : Maybe ContextMenu
  , editingEdgeTitle : Maybe (Graph.EdgeID, String)
  , editingVertexTitle : Maybe (Graph.VertexID, String)
  }

init : Model
init =
  { sideMenuShown = False
  , backgroundOpacity = 1
  , mainMenuView = Default
  , contextMenu = Nothing
  , highlightedEdges = Dict.empty
  , highlightedVertices = Dict.empty
  , editingEdgeTitle = Nothing
  , editingVertexTitle = Nothing
  }

type MenuMsg
  = SetMenuShown Bool
  | SetBackgroundOpacity Float
  | LeaveGraph
  | SetMainMenuView MainMenuView
  | SetEdgeHighlighted Graph.EdgeID Bool
  | SetVertexHighlighted Graph.EdgeID Bool
  | SetEdgeTitle Graph.Edge String
  | SetVertexTitle Graph.Vertex String
  | SetEditingEdgeTitle (Maybe (Graph.EdgeID, String))
  | SetEditingVertexTitle (Maybe (Graph.VertexID, String))
  | DeleteVertex Graph.Vertex
  | DeleteEdge Graph.Edge

update : Model -> MenuMsg -> (Model, Cmd MenuMsg)
update model msg =
  case msg of
    SetMenuShown shown ->
      ({ model | sideMenuShown = shown }, Cmd.none )

    SetBackgroundOpacity opacity ->
      ({ model | backgroundOpacity = max 0 <| min 1 opacity }, Cmd.none)

    LeaveGraph ->
      ({ model | sideMenuShown = False }, Cmd.none )

    SetMainMenuView mainMenuView ->
      ({ model | mainMenuView = mainMenuView }, Cmd.none )

    SetEdgeHighlighted edgeID highlighted ->
      ( { model
        | highlightedEdges = Dict.insert edgeID highlighted model.highlightedEdges
        }
      , Cmd.none
      )

    SetVertexHighlighted vertexId highlighted ->
      ( { model
        | highlightedVertices = Dict.insert vertexId highlighted model.highlightedVertices
        }
      , Cmd.none
      )

    SetEdgeTitle _ _ ->
      ({ model | editingEdgeTitle = Nothing}, Cmd.none)

    SetVertexTitle _ _ ->
      ({ model | editingVertexTitle = Nothing}, Cmd.none)

    SetEditingEdgeTitle newValue ->
      ( { model | editingEdgeTitle = newValue }
      , Cmd.none
      )

    SetEditingVertexTitle newValue ->
      ( { model | editingVertexTitle = newValue }
      , Cmd.none
      )

    DeleteVertex _ ->
      ( { model | mainMenuView = VertexList }
      , Cmd.none
      )

    DeleteEdge _ ->
      ( { model | mainMenuView = EdgeList }
      , Cmd.none
      )




menuPane : Model -> Graph.Graph -> Html.Html MenuMsg
menuPane model graph =
  Html.div
  [ Attr.class "fixed top-0 bottom-0 w-96 w-full transition-all bg-primary shadow-md z-20"
  , Attr.classList
    [ ("left-0", model.sideMenuShown)
    , ("-left-96", not model.sideMenuShown)
    ]
  ]
  [ Html.div
    [ Attr.class "bg-primary text-secondary p-4 rounded-br-md shadow-md w-max absolute top-0 left-0" ]
    [ Html.h1
      [ Attr.class "text-xl font-bold inline-block w-96 pr-8" ]
      [ Html.text graph.title ]
    , Html.button
      [ Attr.class "inline-block"
      , Events.onClick <| SetMenuShown <| not model.sideMenuShown
      ]
      [ Icons.menu ]
    ]
  , Html.div
    [ Attr.class "w-full h-full pt-16 pb-16 px-2 text-white" ]
    [ menuView model graph
    ]
  , Html.div
    [ Attr.class "flex shadow-inner absolute bottom-0 left-0 right-0" ]
    [ Html.button
      [ Attr.class "px-6 py-3 shadow-inner text-secondary font-bold transition-colors hover:bg-secondary hover:text-primary"
      , Events.onClick LeaveGraph
      ]
      [ Html.text "Leave graph" ]
    ]
  ]

menuView : Model -> Graph.Graph -> Html.Html MenuMsg
menuView model graph =
  Html.div
  [ Attr.class "w-full h-full flex flex-col pt-2" ] <|
  case model.mainMenuView of
    Default ->
      defaultView model
    EdgeList ->
      edgesListView model <| Dict.values graph.edges
    VertexList ->
      verticesListView model <| Dict.values graph.vertices
    EdgeDetail edge ->
      []
    VertexDetail vertex ->
      []
    ConfirmDeleteVertex vertex ->
      confirmDeleteVertexView vertex graph
    ConfirmDeleteEdge edge ->
      confirmDeleteEdgeView edge


defaultView model =
  [ Html.button
    [ Attr.class "flex justify-between h-12 w-full items-center transition-colors hover:bg-blue-500 px-2 rounded-md font-light"
    , Events.onClick <| SetMainMenuView EdgeList
    ]
    [ Html.span [] [ Html.text "Edges" ]
    , Icons.chevronRight
    ]
  , Html.button
    [ Attr.class "flex justify-between h-12 w-full items-center transition-colors hover:bg-blue-500 px-2 rounded-md font-light"
    , Events.onClick <| SetMainMenuView VertexList
    ]
    [ Html.span [] [ Html.text "Vertices" ]
    , Icons.chevronRight
    ]
  , Html.div
    [ Attr.class "h-12 flex flex-col justify-center px-2" ]
    [ Html.label
      [ Attr.class "font-light flex justify-between" ]
      [ Html.span [] [ Html.text "Background opacity"]
      , Html.span [] [ Html.text <| (String.fromInt <| round <| 100 * model.backgroundOpacity) ++ "%" ]
      ]
    , Html.input
      [ Attr.type_ "range"
      , Attr.class "w-full block mt-1"
      , Attr.max "1"
      , Attr.min "0"
      , Attr.step "0.01"
      , Attr.value <| String.fromFloat model.backgroundOpacity
      , Events.onInput <| SetBackgroundOpacity << Maybe.withDefault 1 << String.toFloat
      ]
      []
    ]
  ]


edgesListView : Model -> List Graph.Edge -> List (Html.Html MenuMsg)
edgesListView model edges =
   let (lifts, runs) = List.partition (\edge -> edge.edgeType == Graph.Lift) edges in
  [ Html.button
    [ Attr.class "flex w-full items-center transition-colors hover:bg-blue-500 px-2 py-3 mb-2 rounded-md font-light"
    , Events.onClick <| SetMainMenuView Default
    ]
    [ Icons.chevronLeft
    , Html.h2
      [ Attr.class "font-bold text-lg text-secondary"
      ]
      [ Html.text "Edges" ]
    ]
  , Html.ul
    [ Attr.class "overflow-y-auto pl-4" ]
    [ Html.li
      [ Attr.class "" ]
      [ Html.h3
        [ Attr.class "font-bold text-secondary" ]
        [ Html.text "Ski lifts" ]
      , Html.ul
        []
        <| List.map (edgeInListView model) <| List.sortWith Graph.titleComparator lifts
      ]
    , Html.li
      [ Attr.class "" ]
      [ Html.h3
       [ Attr.class "font-bold text-secondary" ]
       [ Html.text "Ski runs" ]
      , Html.ul
        []
        <| List.map (edgeInListView model) <| List.sortWith Graph.titleComparator runs
      ]
    ]
  ]

edgeInListView : Model -> Graph.Edge -> Html.Html MenuMsg
edgeInListView model edge =
  Html.li
  [ Attr.class "grid grid-cols-12 gap-2" ] <|
  ( case edge.edgeType of
      Graph.SkiRun skiRunType -> [ Html.div [ Attr.class "bg-white rounded-full self-center h-5 w-5 p-1" ] [ Icons.skiRunIcon <| Graph.skiRunColorCode skiRunType ] ]
      _ -> []
  ) ++
  ( let
      withoutIcon = edge.edgeType == Graph.Lift || edge.edgeType == Graph.Unfinished
      title =
        [ Html.h3
          [ Attr.class "col-span-8 transition-all border border-primary rounded-md pl-1 hover:border-blue-500"
          , Attr.classList
            [ ("col-start-2", withoutIcon)
            ]
          , Events.onClick <| SetEditingEdgeTitle <| Just (edge.id, Maybe.withDefault ("edge-" ++ String.fromInt edge.id) <| edge.title)
          ]
          [ Html.text <| Maybe.withDefault ("edge-" ++ String.fromInt edge.id) <| edge.title ]
        ]
    in
    case model.editingEdgeTitle of
      Just (id, newTitle) ->
        if id == edge.id then
        [ Html.input
          [ Attr.class "col-span-8 pl-1 text-black rounded-md bg-white w-full"
          , Attr.value <| newTitle
          , Attr.classList
            [ ("col-start-2", withoutIcon)
            ]
          , Events.onInput <| (\t -> SetEditingEdgeTitle <| Just (edge.id, t))
          , Events.onBlur  <| SetEdgeTitle edge newTitle
          , Events.on "keyup" (Json.Decode.andThen (\keyCode -> if keyCode == 13 then Json.Decode.succeed <| SetEdgeTitle edge newTitle else Json.Decode.fail "Not Enter.") Events.keyCode)
          ]
          []
        ] else title
      _ -> title
  ) ++
  [ let highlighted = Maybe.withDefault False <| Dict.get edge.id model.highlightedEdges in
    Html.button
    [ Attr.class "transition-colors hover:text-white"
    , Attr.classList
      [ ("text-blue-200", not highlighted)
      , ("text-secondary", highlighted)
      ]
    , Events.onClick <| SetEdgeHighlighted edge.id <| not highlighted
    ]
    [ Icons.highlight ]
  , Html.button
    [ Attr.class "transition-colors hover:text-red-600"
    , Events.onClick <| SetMainMenuView <| ConfirmDeleteEdge edge
    ]
    [ Icons.remove ]
  , Html.button
    [ Attr.class "transition-colors hover:text-secondary" ]
    [ Icons.detail ]
  ]




verticesListView : Model -> List Graph.Vertex -> List (Html.Html MenuMsg)
verticesListView model vertices =
   let (stations, crossroads) = List.partition (\vertex -> vertex.vertexType == Graph.LiftStation) vertices in
  [ Html.button
    [ Attr.class "flex w-full items-center transition-colors hover:bg-blue-500 px-2 py-3 mb-2 rounded-md font-light"
    , Events.onClick <| SetMainMenuView Default
    ]
    [ Icons.chevronLeft
    , Html.h2
      [ Attr.class "font-bold text-lg text-secondary"
      ]
      [ Html.text "Vertices" ]
    ]
  , Html.ul
    [ Attr.class "overflow-y-auto pl-4" ]
    [ Html.li
      [ Attr.class "" ]
      [ Html.h3
        [ Attr.class "font-bold text-secondary" ]
        [ Html.text "Lift stations" ]
      , Html.ul
        []
        <| List.map (vertexInListView model) <| List.sortWith Graph.titleComparator stations
      ]
    , Html.li
      [ Attr.class "" ]
      [ Html.h3
       [ Attr.class "font-bold text-secondary" ]
       [ Html.text "Ski run X-ings" ]
      , Html.ul
        []
        <| List.map (vertexInListView model) <| List.sortWith Graph.titleComparator crossroads
      ]
    ]
  ]


vertexInListView : Model -> Graph.Vertex -> Html.Html MenuMsg
vertexInListView model vertex =
  Html.li
  [ Attr.class "grid grid-cols-12 gap-2" ] <|
  ( case model.editingVertexTitle of
     Just (id, newTitle) ->
       if id == vertex.id then
        [ Html.input
          [ Attr.class "col-span-9 pl-1 text-black rounded-md bg-white w-full"
          , Attr.value newTitle
          , Events.onInput <| (\t -> SetEditingVertexTitle <| Just (vertex.id, t))
          , Events.onBlur  <| SetVertexTitle vertex newTitle
          , Events.on "keyup" (Json.Decode.andThen (\keyCode -> if keyCode == 13 then Json.Decode.succeed <| SetVertexTitle vertex newTitle else Json.Decode.fail "Not Enter.") Events.keyCode)
          ]
          []
        ] else []
     _ ->
      [ Html.h3
        [ Attr.class "col-span-9 transition-all border border-primary rounded-md pl-1 hover:border-blue-500"
        , Events.onClick <| SetEditingVertexTitle <| Just (vertex.id, Maybe.withDefault ("vertex-" ++ String.fromInt vertex.id) <| vertex.title)
        ]
        [ Html.text <| Maybe.withDefault ("vertex-" ++ String.fromInt vertex.id) <| vertex.title ]
      ]
    ) ++
  [ let highlighted = Maybe.withDefault False <| Dict.get vertex.id model.highlightedVertices in
    Html.button
    [ Attr.class "transition-colors hover:text-white"
    , Attr.classList
      [ ("text-blue-200", not highlighted)
      , ("text-secondary", highlighted)
      ]
    , Events.onClick <| SetVertexHighlighted vertex.id <| not highlighted
    ]
    [ Icons.highlight ]
  , Html.button
    [ Attr.class "transition-colors hover:text-red-600"
    , Events.onClick <| SetMainMenuView <| ConfirmDeleteVertex vertex
    ]
    [ Icons.remove ]
  , Html.button
    [ Attr.class "transition-colors hover:text-secondary" ]
    [ Icons.detail ]
  ]

confirmDeleteEdgeView : Graph.Edge -> List (Html.Html MenuMsg)
confirmDeleteEdgeView edge =
  confirmDeleteView (SetMainMenuView EdgeList) (DeleteEdge edge) [] [ edge ]

confirmDeleteVertexView : Graph.Vertex -> Graph.Graph -> List (Html.Html MenuMsg)
confirmDeleteVertexView vertex graph =
  confirmDeleteView (SetMainMenuView VertexList) (DeleteVertex vertex) [ vertex ] (Graph.findConnectedEdges vertex graph)

confirmDeleteView : MenuMsg -> MenuMsg -> List Graph.Vertex -> List Graph.Edge -> List (Html.Html MenuMsg)
confirmDeleteView onCancel onConfirm vertices edges =
  [ Html.div
    [ Attr.class "grid grid-cols-2 gap-4" ] <|
    [ Html.button
      [ Attr.class "col-span-2 flex w-full items-center transition-colors hover:bg-blue-500 px-2 py-3 mb-2 rounded-md font-light"
      , Events.onClick onConfirm
      ]
      [ Icons.chevronLeft
      , Html.h2
        [ Attr.class "font-bold text-lg text-secondary"
        ]
        [ Html.text "Are you sure?" ]
      ]
    ] ++ (
      let
        nv = List.length vertices
        ne = List.length edges
      in
      case (vertices, edges) of
        ([], []) -> []
        (_, []) ->
          [ Html.p
            [ Attr.class "col-span-2 font-light text-white" ]
            [ Html.text <| "You are about to delete " ++ String.fromInt nv ++ if nv > 1 then " vertices:" else " vertex:" ]
          , Html.ul
            [ Attr.class "col-span-2 font-bold pl-2" ]
            <| List.map (\v -> Html.li [] [ Html.text <| Maybe.withDefault ("vertex-" ++ String.fromInt v.id) <| v.title ]) vertices
          ]
        ([], _) ->
          [ Html.p
            [ Attr.class "col-span-2 font-light text-white" ]
            [ Html.text <| "You are about to delete " ++ String.fromInt ne ++ if ne > 1 then " edges:" else " edge:" ]
          , Html.ul
            [ Attr.class "col-span-2 font-bold pl-2" ]
            <| List.map (\e -> Html.li [] [ Html.text <| Maybe.withDefault ("edge-" ++ String.fromInt e.id) <| e.title ]) edges
          ]
        (_, _) ->
          [ Html.p
            [ Attr.class "col-span-2 font-light text-white" ]
            [ Html.text <| "You are about to delete " ++ String.fromInt nv ++ if nv > 1 then " vertices:" else " vertex:" ]
          , Html.ul
            [ Attr.class "col-span-2 font-bold pl-2" ]
            <| List.map (\v -> Html.li [] [ Html.text <| Maybe.withDefault ("vertex-" ++ String.fromInt v.id) <| v.title ]) vertices
          , Html.p
            [ Attr.class "col-span-2 font-light text-white" ]
            [ Html.text <| "This will also delete the following " ++ String.fromInt ne ++ " connected" ++ if ne > 1 then " edges:" else " edge:" ]
          , Html.ul
            [ Attr.class "col-span-2 font-bold pl-2" ]
            <| List.map (\e -> Html.li [] [ Html.text <| Maybe.withDefault ("edge-" ++ String.fromInt e.id) <| e.title ]) edges
          ]
    ) ++
    [ Html.button
      [ Attr.class "rounded-md border-secondary border transition-colors text-secondary hover:bg-secondary hover:text-primary"
      , Events.onClick onCancel
      ]
      [ Html.text "Cancel" ]
    , Html.button
      [ Attr.class "rounded-md border transition-colors bg-secondary text-primary hover:bg-primary hover:text-secondary border hover:border-secondary"
      , Events.onClick onConfirm
      ]
      [ Html.text "Confirm" ]
    ]
  ]

isVertexTitleEdited : Model -> Graph.Vertex -> Bool
isVertexTitleEdited model vertex =
  case model.editingVertexTitle of
    Just (id, _) -> id == vertex.id
    _ -> False

isEdgeTitleEdited : Model -> Graph.Edge -> Bool
isEdgeTitleEdited model edge =
  case model.editingEdgeTitle of
    Just (id, _) -> id == edge.id
    _ -> False
