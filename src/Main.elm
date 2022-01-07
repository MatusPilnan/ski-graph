port module Main exposing (..)

import Animator
import Browser
import Canvas.Settings
import Canvas.Settings.Advanced
import Canvas.Settings.Line
import Canvas.Texture
import Color
import Dict
import Geometry as Geom
import Graph
import GraphUtils as GU
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Canvas
import Icons
import Json.Decode as D
import Loading exposing (defaultConfig)
import Menus.Menus as Menus
import Messages exposing (Msg(..))
import Model exposing (..)
import Random
import Requests
import Saves
import UUID
import Utils



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


init : Flags -> (Model, Cmd Msg)
init flags =
  ( { currentGraph = Nothing
    , animations =
      { expandedPoint = Animator.init Nothing
      , highlightedEdge = Animator.init 0
      }
    , texture = Nothing
    , width = flags.width
    , height = flags.height
    , vertexCounter = 0
    , edgeCounter = 0
    , mousePosition = Geom.Point 0 0
    , mouseDown = False
    , hasMovedWhileMouseDown = False
    , mouseDownStartPosition = Geom.Point 0 0
    , mapFieldVisible = False
    , mapFieldInput = flags.savedBackground
    , mapFieldState = Loading
    , drawingEdge = Nothing
    , activeEdgeDrawingMode = Graph.Lift
    , localGraphIndex = Saves.graphIndexFromJson Graph.Local flags.localGraphIndex
    , remoteGraphIndex = Dict.empty
    , selectedGraphIndexEntry = Nothing
    , baseUrl = flags.baseUrl
    , menu = Menus.init
    } |> Saves.loadGraphFromJsonToModel flags.graphJson
  , Requests.fetchGraphIndex flags.baseUrl
  )



-- UPDATE

port saveToLocalStorage : (String, String) -> Cmd msg
port dimensionsChanged : ((Float, Float) -> msg) -> Sub msg
port loadLocalGraph : String -> Cmd msg
port importLocalGraph : (String -> msg) -> Sub msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      (model, Cmd.none)
    AnimationFrame newTime ->
      ( { model | animations = Animator.update newTime animator model.animations}
      , Cmd.none
      )
    TextureLoaded texture ->
      case texture of
        Just t ->
          ( { model
            | texture = Just t
            , currentGraph = Maybe.map (Graph.setBackground model.mapFieldInput) model.currentGraph
            , mapFieldState = Loaded
            }
          , saveToLocalStorage ("background", model.mapFieldInput)
          ) |> saveModel
        Nothing ->
          ( { model
            | mapFieldState = Invalid
            }
          , Cmd.none
          )

    MouseUp mouseEvent ->
      (model, Cmd.none)
      |> checkVertexCreation mouseEvent
      |> checkEndDrawing mouseEvent
      |> checkConnectDrawing mouseEvent
      |> saveModel

    MouseDown mouseEvent ->
      ( model, Cmd.none )
      |> checkToStartDrag mouseEvent
      |> checkToStartDrawing mouseEvent

    MouseMove mouseEvent ->
      (model, Cmd.none)
      |> setModelMousePosition mouseEvent
      |> checkMouseEventForPointHover mouseEvent
      |> checkModelDragging mouseEvent
      |> checkDrawing mouseEvent


    SetMapFieldVisible bool ->
      ( { model
        | mapFieldVisible = bool
        , mapFieldInput = if not bool && String.isEmpty model.mapFieldInput then Graph.getBackground model.currentGraph else model.mapFieldInput
        , mapFieldState = if not bool && String.isEmpty model.mapFieldInput then Loading else model.mapFieldState
        }
      , Cmd.none
      )

    TrySettingBackground string ->
      ( { model | mapFieldInput = string, mapFieldState = Loading}, Cmd.none)

    DimensionsChanged (width, height) ->
      ( model, Cmd.none)
      |> adaptToNewDimensions width height

    ZoomChanged delta ->
      let
        w = Maybe.withDefault model.width <| Maybe.map (\t -> (Canvas.Texture.dimensions t).width ) model.texture
        h = Maybe.withDefault model.height <| Maybe.map (\t -> (Canvas.Texture.dimensions t).height ) model.texture
        zoomBefore = (Graph.getZoom model.currentGraph)
        zoomAfter = max ((Graph.getZoom model.currentGraph) - ( delta / 1000) ) (max ( model.width / w ) ( model.height / h ) )
      in
      ( { model
        | currentGraph = Maybe.map
          ( Graph.setPosition
            ( GU.constrainBackgroundToCanvas { model | currentGraph = Maybe.map (Graph.setZoom zoomAfter) model.currentGraph }
                <| Geom.subPoints model.mousePosition
                <| Geom.mulPoint (Geom.canvasPointToBackgroundPoint model.mousePosition (Graph.getPosition model.currentGraph) zoomBefore) zoomAfter
            ) << Graph.setZoom zoomAfter
          ) model.currentGraph
        }
      , Cmd.none
      ) |> saveModel

    MouseLeave ->
      ( { model | mouseDown = False }, Cmd.none )

    SetActiveEdgeType edgeType ->
      ( { model | activeEdgeDrawingMode = edgeType}, Cmd.none )

    DownloadCurrentGraph ->
      ( model
      , Cmd.batch
        [ saveGraphAndIndex model
        ]
      )

    CreateNewGraph uuid ->
      case uuid of
        Nothing ->
          ( model
          , Random.generate (CreateNewGraph << Just << UUID.toString) UUID.generator
          )

        Just string ->
          let
            newGraph = (let g = Graph.init in { g | id = string } )
            newIndex = Dict.insert string (Graph.GraphIndexEntry newGraph.title string string Graph.Local) model.localGraphIndex
          in
          ( { model | currentGraph = Just newGraph
            , vertexCounter = 0
            , edgeCounter = 0
            , localGraphIndex = newIndex
            }
          , Cmd.batch
            [ saveGraph model.currentGraph
            , saveGraph <| Just newGraph
            , saveGraphIndex newIndex
            ]
          )


    LoadExistingGraph ->
      ( model
      , case model.selectedGraphIndexEntry of
          Nothing -> Cmd.none
          Just indexEntry ->
            case indexEntry.location of
              Graph.Remote -> Requests.fetchGraph model.baseUrl indexEntry.path
              Graph.Local -> loadLocalGraph indexEntry.id
      )

    SelectGraphFromIndex selection ->
      ( { model | selectedGraphIndexEntry = selection }, (saveToLocalStorage ("selected", Maybe.withDefault "" <| Maybe.map .id selection)))

    LoadGraphIndex maybeResult ->
      case maybeResult of
        Nothing -> (model, Cmd.none)
        Just (Result.Ok index) -> ({ model | remoteGraphIndex = Dict.union model.remoteGraphIndex index }, Cmd.none)
        Just (Err _) -> (model, Cmd.none)

    SetCurrentGraph graph ->
      ( { model
        | currentGraph = Just <| Graph.calculateVertexTypes graph
        , vertexCounter = Graph.getNextVertexId graph
        , edgeCounter = Graph.getNextEdgeId graph
        }
      , saveGraph model.currentGraph)

    UpdateMenu menuMsg ->
      case menuMsg of
        Menus.LeaveGraph ->
          ({ model | currentGraph = Nothing }, Cmd.none)
        _ ->
          Tuple.mapBoth (\menu -> { model | menu = menu}) (\cmd -> Cmd.map UpdateMenu cmd) <| Menus.update model.menu menuMsg




saveGraphAndIndex : Model -> Cmd Msg
saveGraphAndIndex model =
  Cmd.batch [ saveGraph model.currentGraph, saveGraphIndex model.localGraphIndex ]


saveModel : (Model, Cmd Msg) -> (Model, Cmd Msg)
saveModel (model, cmd) =
  (model, Cmd.batch [ cmd, saveGraphAndIndex model ])

saveGraph : Maybe Graph.Graph -> Cmd Msg
saveGraph graph =
  case graph of
    Nothing -> Cmd.none
    Just g ->
      saveToLocalStorage (g.id, Saves.graphToJson 0 g)


saveGraphIndex graphIndex =
  saveToLocalStorage ("graph-index", Saves.graphIndexToJson graphIndex)


checkMouseEventForPointHover : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkMouseEventForPointHover event (model, cmd) =
  ({ model | animations = animateHoveredPoint model.animations <| getHoveringVertex model event }, cmd )

adaptToNewDimensions width height (model, cmd) =
  let
    w = Maybe.withDefault width <| Maybe.map (\t -> (Canvas.Texture.dimensions t).width ) model.texture
    h = Maybe.withDefault height <| Maybe.map (\t -> (Canvas.Texture.dimensions t).height ) model.texture
    zoomAfter = max (Graph.getZoom model.currentGraph) (max ( width / w ) ( height / h ) )
    newModel = { model | width = width, height = height }
  in
  ( { newModel
    | currentGraph = Maybe.map
      ( Graph.setPosition
        ( GU.constrainBackgroundToCanvas newModel
          <| Graph.getPosition model.currentGraph
        ) << Graph.setZoom zoomAfter
      ) model.currentGraph
    }
  , cmd
  )

setModelMousePosition : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
setModelMousePosition event (model, cmd) =
  ({ model | mousePosition = event.position }, cmd )


checkModelDragging : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkModelDragging event (model, cmd) =
  if model.mouseDown && (not <| Utils.maybeHasValue model.drawingEdge) then
    let
      new = Geom.addPoints event.movement <| Graph.getPosition model.currentGraph
    in
    ( { model
      | hasMovedWhileMouseDown = True
      , currentGraph = Graph.updateGraphProperty Graph.setPosition (GU.constrainBackgroundToCanvas model new) model.currentGraph
      }
    , cmd
    ) else (model,cmd)

checkVertexCreation : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkVertexCreation event (model, cmd) =
  let
    condition = model.hasMovedWhileMouseDown || Utils.maybeHasValue model.drawingEdge || event.button /= Primary
  in
  ( { model
    | currentGraph =
      if condition
      then model.currentGraph
      else Graph.updateGraphProperty (Graph.addVertex model.vertexCounter) (Geom.canvasPointToBackgroundPoint event.position (Graph.getPosition model.currentGraph) (Graph.getZoom model.currentGraph)) model.currentGraph
    , vertexCounter =
      if condition
      then model.vertexCounter
      else model.vertexCounter + 1
    , mouseDown = False
    }
  , cmd
  )

checkToStartDrag : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkToStartDrag event (model, cmd) =
  ( if event.button == Primary then
    { model
    | mouseDown = True
    , hasMovedWhileMouseDown = False
    , mouseDownStartPosition = event.position
    , mapFieldVisible = False
    } else model
  , cmd
  )

checkEndDrawing : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkEndDrawing event (model, cmd) =
  ( if event.button == Secondary then
    { model
    | drawingEdge = Nothing
    } else model
  , cmd
  )


checkDrawing : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkDrawing event (model, cmd) =
  ( case (event.button, model.mouseDown, model.drawingEdge) of
    (Primary, True, Just edge) ->
      { model
      | drawingEdge = Just { edge | points = List.append edge.points [ Geom.canvasPointToBackgroundPoint event.position (Graph.getPosition model.currentGraph) (Graph.getZoom model.currentGraph) ] }
      }
    (_, _, _) -> model
  , cmd
  )

checkConnectDrawing : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkConnectDrawing event (model, cmd) =
  let maybeVertex = getHoveringVertex model event in
  ( case (event.button, maybeVertex, model.drawingEdge) of
    (Primary, Just vertex, Just edge) ->
      if vertex /= edge.start
      then connectEdgeToVertex model edge vertex
      else model
    (Primary, Nothing, Just edge) ->
      case getHoveringEdge model event of
        Nothing ->
          { model | drawingEdge = Just { edge | points = edge.points ++ [ Geom.canvasPointToBackgroundPoint event.position (Graph.getPosition model.currentGraph) (Graph.getZoom model.currentGraph) ] }
          }
        Just (targetEdge, point) ->
          connectEdgeToEdge model edge targetEdge point
    (_, _, _) -> model
  , cmd
  )


checkToStartDrawing : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkToStartDrawing event (model, cmd) =
  case (event.button, getHoveringVertex model event, model.drawingEdge) of
    ( Primary, Just vertex, Nothing ) ->
      let edgeId = String.fromInt <| model.edgeCounter + 1 in
      ( { model | drawingEdge = Just <| Graph.Edge (model.edgeCounter + 1) ( Just <| "Edge " ++ edgeId) vertex Nothing Graph.Unfinished vertex.position vertex.position []
        }
      , cmd
      )
    (Primary, Nothing, Nothing) ->
        case getHoveringEdge model event of
          Nothing -> (model, cmd)
          Just (edge, point) ->
            case (model.activeEdgeDrawingMode, edge.edgeType) of
              (Graph.SkiRun _, Graph.SkiRun _) ->
                let
                  edgeId = String.fromInt <| model.edgeCounter + 1
                  vertex = Graph.Vertex -1 Nothing Graph.LiftStation point
                in
                ( { model | drawingEdge = Just <| Graph.Edge (model.edgeCounter + 1) ( Just <| "Edge " ++ edgeId) vertex Nothing Graph.Unfinished vertex.position vertex.position []
                  }
                , cmd
                )
              (_, _) -> (model, cmd)
    (_, _, _) -> (model, cmd)


getHoveringVertex : Model -> MouseEvent -> Maybe Graph.Vertex
getHoveringVertex model event =
  List.head <| List.filter (\v -> Geom.mouseOverPoint (Graph.getPosition model.currentGraph) (Graph.getZoom model.currentGraph) event.position v.position ) <| Graph.getVerticesList model.currentGraph


getHoveringEdge : Model -> MouseEvent -> Maybe (Graph.Edge, Geom.Point)
getHoveringEdge model event =
  List.head <| List.filterMap (\edge -> Maybe.map (\point -> (edge, point)) <| GU.mouseOverEdge (Graph.getPosition model.currentGraph) (Graph.getZoom model.currentGraph) event.position edge) <| Graph.getEdgesList model.currentGraph

getEdgeOnPosition : Model -> Geom.Point -> Maybe (Graph.Edge, Geom.Point)
getEdgeOnPosition model position =
  List.head <| List.filterMap (\edge -> Maybe.map (\point -> (edge, point)) <| GU.pointOverEdge position (Graph.getZoom model.currentGraph) edge) <| Graph.getEdgesList model.currentGraph

getEdgeOnPositionFromGraph : Graph.Graph -> Geom.Point -> Maybe Graph.Edge
getEdgeOnPositionFromGraph graph position =
  List.head <| List.filter (\edge -> Utils.maybeHasValue <| GU.pointOverEdge position graph.zoom edge) <| Graph.getEdgesList <| Just graph


connectEdgeToVertex : Model -> Graph.Edge -> Graph.Vertex -> Model
connectEdgeToVertex model edge vertex =
  { model | drawingEdge = Nothing
  , edgeCounter = if edge.start.id == -1 then model.edgeCounter + 2 else model.edgeCounter + 1
  , vertexCounter = if edge.start.id == -1 then model.vertexCounter + 1 else model.vertexCounter
  , currentGraph = Graph.updateGraphProperty
    ( \newEdge ->
      ( if newEdge.start.id == -1
        then
          GU.splitEdge
          (Maybe.withDefault newEdge <| Maybe.map Tuple.first <| getEdgeOnPosition model newEdge.start.position)
          newEdge.start.position (model.edgeCounter + 2) (model.vertexCounter)
        else identity
      )
      >> Graph.addEdge (GU.calculateEdgeBoundingBox { newEdge | start = (let s = newEdge.start in { s | id = if s.id == -1 then model.vertexCounter else s.id}) })
      >> Graph.calculateVertexTypes
    )
    ( GU.calculateEdgeBoundingBox
      { edge | end = Just vertex
      , edgeType = model.activeEdgeDrawingMode
      }
    ) model.currentGraph
  }

connectEdgeToEdge : Model -> Graph.Edge -> Graph.Edge -> Geom.Point -> Model
connectEdgeToEdge model edge targetEdge point =
  if point == edge.start.position then model else
  { model | drawingEdge = Nothing
  , edgeCounter = if edge.start.id == -1 then model.edgeCounter + 3 else model.edgeCounter + 2
  , vertexCounter = if edge.start.id == -1 then model.vertexCounter + 2 else model.vertexCounter + 1
  , currentGraph = Graph.updateGraphProperty
    ( \newEdge graph ->
      Graph.calculateVertexTypes
      <| ( \updatedGraph ->
        let
          decision =
            ( (newEdge.start.id) == -1
            , getEdgeOnPositionFromGraph updatedGraph newEdge.start.position
            )
          action splitEdge splitPoint =
            GU.splitEdge splitEdge splitPoint (model.edgeCounter + 3) (model.vertexCounter + 1) updatedGraph
            |> Graph.addEdge (GU.calculateEdgeBoundingBox { newEdge | start = (let s = newEdge.start in { s | id = if s.id == -1 then model.vertexCounter + 1 else s.id}) })
        in
        ( case decision of
            (True, Just splitEdge) ->
              action splitEdge newEdge.start.position
            (_, _) ->
              updatedGraph
        )
      )
      <| GU.splitEdge targetEdge point (model.edgeCounter + 2) (model.vertexCounter)
      <| Graph.addEdge (GU.calculateEdgeBoundingBox newEdge) graph
    )
    ({ edge | end = Just <| Graph.Vertex (model.vertexCounter) Nothing Graph.LiftStation point, edgeType = model.activeEdgeDrawingMode })
    model.currentGraph
  }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ dimensionsChanged DimensionsChanged
    , importLocalGraph (\json -> Maybe.withDefault Noop <| Maybe.map SetCurrentGraph <| Saves.graphFromJson <| Just json)
    , Animator.toSubscription AnimationFrame model.animations animator
    ]


-- ANIMATOR
animator : Animator.Animator Animations
animator =
  Animator.animator
    |> Animator.watching .expandedPoint (\newPoint model -> { model | expandedPoint = newPoint })
    |> Animator.watching .highlightedEdge (\newValue model -> { model | highlightedEdge = newValue })

animateHoveredPoint : Animations -> Maybe Graph.Vertex -> Animations
animateHoveredPoint animations newHover =
  { animations | expandedPoint = animations.expandedPoint |> Animator.go Animator.quickly newHover }



-- VIEW


view : Model -> Html.Html Msg
view model =
  Html.main_
  [ Attr.class "h-screen w-screen overflow-hidden relative" ]
  <| case model.currentGraph of
      Nothing ->
        graphSelectionView model
      Just graph ->
        (Html.map UpdateMenu <| Menus.menuPane model.menu graph) :: (canvasView model)

graphSelectionView : Model -> List (Html.Html Msg)
graphSelectionView model =
  [ Html.div
    [ Attr.class "h-full w-full flex flex-col justify-center items-center" ]
    [ Html.button
      [ Attr.class "transition-colors shadow-md rounded-md bg-primary text-secondary px-4 py-2 hover:bg-secondary hover:text-primary font-bold uppercase mb-8"
      , Events.onClick <| CreateNewGraph Nothing
      ]
      [ Html.text "Create a new graph" ]
    , Html.p [ Attr.class "text-center my-8" ] [ Html.text "or select one of existing graphs:" ]
    , Html.div
      [ Attr.class "flex" ]
      [ Html.select
        [ Attr.class "transition-all border-2 border-primary rounded-md min-w-[15rem]"
        , Events.on "change" <| D.map (\s -> SelectGraphFromIndex <| Saves.graphIndexEntryFromString s ) <| D.at ["target", "value"] D.string
        ]
        <| Html.option [ Attr.value "" ] [ Html.text "No graph selected..." ]
        ::
        ( List.map
          ( \e ->
            Html.option
            [ Attr.value (Saves.graphIndexEntryToString 0 e) ]
            [ Html.text <| e.title ++ if e.location == Graph.Local then " (Local)" else " (Remote)" ]
          ) <| List.append (Dict.values model.localGraphIndex) <| Dict.values model.remoteGraphIndex)
      , ( let disabled = not <| Utils.maybeHasValue model.selectedGraphIndexEntry in
          Html.button
          [ Attr.class "transition-colors rounded-md font-bold uppercase ml-2 px-4 py-2"
          , Attr.classList
            [ ("bg-gray-200 text-gray-400", disabled)
            , ("bg-primary text-secondary hover:bg-secondary hover:text-primary shadow-md", not disabled)
            ]
          , Events.onClick <| if disabled then Noop else LoadExistingGraph
          , Attr.disabled disabled
          ]
          [ Html.text "Load" ]
        )
      ]
    ]
  ]


canvasView : Model -> List (Html.Html Msg)
canvasView model =
  [ Canvas.toHtmlWith
    { width = ceiling model.width
    , height = ceiling model.height
    , textures = [ Canvas.Texture.loadFromImageUrl model.mapFieldInput TextureLoaded ]
    }
    [ Attr.class "h-full"
    , Events.on "mousedown" <| mouseDecoder MouseDown
    , Events.on "mouseup" <| mouseDecoder MouseUp
    , Events.on "mouseleave" <| mouseDecoder (\_ -> MouseLeave)
    , Events.on "mousemove" <| mouseDecoder MouseMove
    , Events.on "wheel" <| scrollDecoder ZoomChanged
    ]
    [ Canvas.group
      [ Canvas.Settings.Advanced.transform
      (
        let
          pos = Graph.getPosition model.currentGraph
          zoom = Graph.getZoom model.currentGraph
        in
        [ Canvas.Settings.Advanced.translate pos.x pos.y
        , Canvas.Settings.Advanced.scale zoom zoom
        ]
      )
      ]
      <| addBackground model
      [ Canvas.group []
        <| (List.map (edgeView model) <| Graph.getEdgesList model.currentGraph)
        ++ (List.map (vertexView model) <| Graph.getVerticesList model.currentGraph)
        ++
        case model.drawingEdge of
          Nothing -> []
          Just edge ->
            [ edgeView model edge ]
      ]
    ]
  , mapField model
  , modeSelectionButtons model.activeEdgeDrawingMode
  , saveMapButtons model
  ]

pointToCanvasLibPoint point =
  (point.x, point.y)

mouseDecoder msg =
  D.map5
    ( \offsetX offsetY movementX movementY button ->
      msg <| MouseEvent (Geom.Point offsetX offsetY) (Geom.Point movementX movementY)
      <| case button of
          0 -> Primary
          1 -> Wheel
          2 -> Secondary
          _ -> Other
    )
    ( D.field "offsetX" D.float)
    ( D.field "offsetY" D.float)
    ( D.field "movementX" D.float)
    ( D.field "movementY" D.float)
    ( D.field "button" D.int)

scrollDecoder msg =
   D.map msg <| D.field "deltaY" D.float

vertexView : Model -> Graph.Vertex -> Canvas.Renderable
vertexView model vertex =
  Canvas.group
  []
  [ ( case vertex.vertexType of
      Graph.LiftStation ->
        Canvas.shapes
        []
        [ Canvas.circle
          ( vertex.position.x, vertex.position.y)
          ( vertexPointSize model vertex Geom.liftStationPointSize 1 1.5 )
        ]
      Graph.SkiRunFork percentages ->
        Canvas.group
        []
        <| Tuple.first
        <| List.foldl
          ( \(skiRunType, percentage) (renderables, start) ->
            ( renderables ++
              [ renderPieSlice
                ( Graph.skiRunColor <| Graph.skiRunTypeFromString skiRunType )
                vertex.position
                ( vertexPointSize model vertex Geom.skiRunConnectionPointSize 1 1.5 )
                (degrees <| 360 * (min 1 start))
                (degrees <| 360 * (min 1 <| start + percentage))
              ]
            , start + percentage
            )
          ) ([], 0)
        <| Dict.toList percentages
    )
  , Canvas.shapes
    [ Canvas.Settings.fill <| if vertexEdgeDrawingCondition model vertex then Color.green else Color.white ]
    [ Canvas.circle
      ( vertex.position.x, vertex.position.y)
      ( vertexPointSize model vertex (if (vertex.vertexType) == Graph.LiftStation then Geom.liftStationPointSize else Geom.skiRunConnectionPointSize) 0 1 )
    ]
  ]

vertexPointSize : Model -> Graph.Vertex -> Float -> Float -> Float -> Float
vertexPointSize model vertex baseRadius baseMultiplier hoverMultiplier =
  ( max 0
    ( if vertexExpansionCondition model vertex
      then Animator.move model.animations.expandedPoint <| vertexAnimation baseRadius baseMultiplier hoverMultiplier
      else if vertexEdgeDrawingCondition model vertex
      then hoverMultiplier * baseRadius
      else baseMultiplier * baseRadius
    ) / (Graph.getZoom model.currentGraph)
  )


vertexAnimation baseRadius baseMultiplier hoverMultiplier state =
  Animator.at <|
  case state of
    Nothing -> baseMultiplier * baseRadius
    Just _ -> hoverMultiplier * baseRadius


vertexExpansionCondition : Model -> Graph.Vertex -> Bool
vertexExpansionCondition model vertex =
  (Animator.current model.animations.expandedPoint == Just vertex)
  || (Animator.arrived model.animations.expandedPoint == Just vertex)
  || (Animator.previous model.animations.expandedPoint == Just vertex)


vertexEdgeDrawingCondition : Model -> Graph.Vertex -> Bool
vertexEdgeDrawingCondition model vertex =
  ( Maybe.map .start model.drawingEdge == Just vertex)


edgeView : Model -> Graph.Edge -> Canvas.Renderable
edgeView model edge =
  Canvas.group
  [] <|
  [ Canvas.shapes
    (edgeStyle edge.edgeType (Graph.getZoom model.currentGraph)) <|
    [ edgeToPath model edge
    ] ++ (
      let tempVertex point = Canvas.circle (pointToCanvasLibPoint point) (Geom.skiRunConnectionPointSize / Graph.getZoom model.currentGraph) in
      case (model.activeEdgeDrawingMode, edge.edgeType, GU.mouseOverEdge (Graph.getPosition model.currentGraph) (Graph.getZoom model.currentGraph) model.mousePosition edge) of
        (Graph.SkiRun _, Graph.SkiRun _, Just point) ->
          [ tempVertex point ]
        (_, _, _) ->
          if edge.start.id == -1 then [ tempVertex edge.start.position ] else []
    )
  ] ++ (
    if Maybe.withDefault False <| Dict.get edge.id model.menu.highlightedEdges
    then
      [ Canvas.shapes
        [ Canvas.Settings.Line.lineWidth <| Geom.lineWidth * 0.5 / (Graph.getZoom model.currentGraph)
        --, Canvas.Settings.Line.lineCap Canvas.Settings.Line.RoundCap
        , Canvas.Settings.Line.lineJoin Canvas.Settings.Line.RoundJoin
        , Canvas.Settings.stroke <| Color.fromRgba { red = 253 / 256, green = 224 / 256, blue = 71 / 256, alpha = 1 }
        , Canvas.Settings.Line.lineDash <|
          let
            zoom = ( Graph.getZoom model.currentGraph )
            dash =
              Animator.move
              model.animations.highlightedEdge
              (\_ ->
                Animator.loop Animator.verySlowly <| Animator.wrap -1 1
              )
            line x = (x * 10) / zoom
            gap x = (x * 15) / zoom
          in
          if dash > 0 then
            if dash > 0.999
            then
            [ line dash
            , gap dash
            ]
            else
            [ line 0
            , gap dash
            , line 1
            , gap <| 1 - dash
            ]
          else
            if dash < -0.999
            then
            [ line 0
            , gap 1
            , line 1
            , gap 0
            ]
            else
            [ line <| 1 - (abs dash)
            , gap 1
            , line <| abs dash
            , gap 0
            ]
        ]
        [ edgeToPath model edge ]
      ]
    else
      []
  )

edgeToPath : Model -> Graph.Edge -> Canvas.Shape
edgeToPath model edge =
  Canvas.path (pointToCanvasLibPoint edge.start.position)
  <| List.map
    (Canvas.lineTo << pointToCanvasLibPoint)
    <| edge.points
    ++ [ case edge.end of
           Nothing -> Geom.canvasPointToBackgroundPoint model.mousePosition (Graph.getPosition model.currentGraph) (Graph.getZoom model.currentGraph)
           Just vertex -> vertex.position
       ]

edgeStyle : Graph.EdgeType -> Float -> List Canvas.Settings.Setting
edgeStyle edgeType zoom =
  [ Canvas.Settings.Line.lineWidth <| Geom.lineWidth / zoom
  , Canvas.Settings.Line.lineCap Canvas.Settings.Line.RoundCap
  , Canvas.Settings.Line.lineJoin Canvas.Settings.Line.RoundJoin
  ] ++
  case edgeType of
    Graph.SkiRun skiRunType ->
      case skiRunType of
        Graph.SkiRoute -> [ Canvas.Settings.stroke (Graph.skiRunColor skiRunType), Canvas.Settings.Line.lineDash [10 / zoom, 15 / zoom] ]
        _ -> [ Canvas.Settings.stroke (Graph.skiRunColor skiRunType) ]
    Graph.Lift -> [ Canvas.Settings.stroke Color.black ]
    Graph.Unfinished -> [ Canvas.Settings.stroke Color.green ]


addBackground : Model -> List Canvas.Renderable -> List Canvas.Renderable
addBackground model renderables =
  case model.texture of
    Nothing -> [ Canvas.clear (0, 0) model.width model.height ]
    Just t ->
      [ Canvas.clear (0, 0) ((Canvas.Texture.dimensions t).width) ((Canvas.Texture.dimensions t).height)
      , Canvas.texture
        [ Canvas.Settings.Advanced.alpha model.menu.backgroundOpacity ]
        (0, 0)
        t
      ] ++ renderables



mapField model =
  Html.div
    [ Attr.class "fixed bottom-4 right-4 flex items-center"
    ] <| (
    case model.mapFieldState of
      Loading ->
        [ Loading.render
          Loading.Sonar -- LoaderType
          { defaultConfig | color = "#60a5fa" } -- Config
          Loading.On -- LoadingState
        ]
      Loaded -> []
      Invalid ->
        [ Html.div [ Attr.class "text-red-600" ] [ Icons.errorIcon ]
        ]
     ) ++
    [ Html.input
      [ Attr.classList
        [ ("max-w-0", not model.mapFieldVisible)
        , ("max-w-xs border-2 px-4 py-2", model.mapFieldVisible)
        , ("border-red-600", model.mapFieldState == Invalid)
        , ("border-primary", model.mapFieldState /= Invalid)
        ]
      , Attr.class "w-max rounded-full shadow-lg transition-color focus:border-blue-600 focus-visible:border-blue-600 transition-all mx-2"
      , Attr.value model.mapFieldInput
      , Events.onInput <| TrySettingBackground
      ]
      []
    , Html.button
      [ Attr.class "rounded-full transition-all shadow-md hover:shadow-lg bg-blue-400 hover:bg-white text-yellow-300 hover:text-primary p-4 text-center p-3 h-12 w-12"
      , Events.onClick <| SetMapFieldVisible <| not model.mapFieldVisible
      ]
      [ Icons.mapIcon ]
    ]


modeSelectionButtons selected =
  let btn = modeSelectionButton selected in
  Html.div
  [ Attr.class "fixed right-0 bottom-24 group" ]
  [ Html.div
    [ Attr.class "flex flex-col items-end mr-4 group-hover:mr-6 transition-all" ]
    [ btn Graph.Lift
    , btn <| Graph.SkiRun Graph.Easy
    , btn <| Graph.SkiRun Graph.Medium
    , btn <| Graph.SkiRun Graph.Difficult
    , btn <| Graph.SkiRun Graph.SkiRoute
    ]
  , Html.div
    [ Attr.class "transition-all bg-primary text-white py-2 pl-1 mb-4 rounded-tl-md rounded-bl-md absolute h-max bottom-0 -right-full group-hover:right-0 text-xs [writing-mode:vertical-rl] [text-orientation:mixed]" ]
    [ Html.text "Icons made by ", Html.a
      [ Attr.href "https://www.flaticon.com/authors/freepik"
      , Attr.title "Freepik"
      , Attr.class "text-secondary"
      ]
      [ Html.text "Freepik" ]
    , Html.text " from ", Html.a
      [ Attr.href "https://www.flaticon.com/"
      , Attr.class "text-secondary"
      , Attr.title "Flaticon"
      ]
      [ Html.text "www.flaticon.com" ]
    ]
  ]

modeSelectionButton : Graph.EdgeType -> Graph.EdgeType -> Html.Html Msg
modeSelectionButton selected edgeType =
  let
    active = selected == edgeType
    icon =
      case edgeType of
        Graph.SkiRun skiRunType ->
          Icons.skiRunIcon <| Graph.skiRunColorCode skiRunType
        Graph.Lift -> Icons.liftIcon
        Graph.Unfinished -> Html.text ""

  in
  Html.button
  [ Attr.class "rounded-full shadow-lg transition-all text-center mb-4 hover:h-12 hover:w-12 hover:p-3 hover:shadow-xl"
  , Attr.classList
    [ ("p-3 h-12 w-12 bg-blue-400", active)
    , ("p-2 h-8 w-8 bg-white", not active)
    ]
  , Events.onClick <| if active then Noop else SetActiveEdgeType edgeType
  ]
  [ icon ]

saveMapButtons : Model -> Html.Html Msg
saveMapButtons model =
  Html.div
  [ Attr.class "fixed top-4 right-4" ]
  [ Html.a
    [ Attr.class "h-12 w-12 rounded-full transition-all shadow-md hover:shadow-lg bg-blue-400 hover:bg-white text-yellow-300 hover:text-primary p-3 text-center block"
    , Attr.href <| "data:text/plain;charset=utf-8," ++ (Maybe.withDefault "" <| Maybe.map (Saves.graphToJson 2) model.currentGraph)
    , Attr.download "graph.json"
    , Events.onClick DownloadCurrentGraph
    ]
    [ Icons.saveIcon ]
  ]


renderPieSlice : Color.Color -> Geom.Point -> Float -> Float -> Float -> Canvas.Renderable
renderPieSlice color center radius startAngle endAngle =
  Canvas.shapes [ Canvas.Settings.fill color ]
    [ Canvas.path (pointToCanvasLibPoint center)
      [ Canvas.lineTo ( center.x + radius * cos startAngle, center.y + radius * sin startAngle )
      , Canvas.lineTo ( center.x + radius * cos endAngle, center.y + radius * sin endAngle )
      , Canvas.lineTo (pointToCanvasLibPoint center)
      ]
    , Canvas.arc
      (pointToCanvasLibPoint center)
      radius
      { startAngle = startAngle
      , endAngle = endAngle
      , clockwise = True
      }
    ]

