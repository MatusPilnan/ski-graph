port module Main exposing (..)

import Animator
import Browser
import Canvas.Settings
import Canvas.Settings.Advanced
import Canvas.Settings.Line
import Canvas.Texture
import Color
import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Canvas
import Icons
import Json.Decode as D
import Loading exposing (defaultConfig)
import Time



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Vertex =
  { id : String
  , title : Maybe String
  , position : Point
  }

type SkiRunType
  = Easy
  | Medium
  | Difficult
  | SkiRoute


type EdgeType
  = SkiRun SkiRunType
  | Lift
  | Unfinished

type alias Edge =
  { id : String
  , title : Maybe String
  , start : Vertex
  , end : Maybe Vertex
  , edgeType : EdgeType
  , points : List Point
  }

type BackgroundState = Ok | Loading | Invalid

type alias Animations =
  { expandedPoint : Animator.Timeline (Maybe Vertex)
  }


-- MODEL


type alias Model =
  { background : String
  , animations : Animations
  , texture :  Maybe Canvas.Texture.Texture
  , vertices : Dict String Vertex
  , edges : Dict String Edge
  , width : Float
  , height : Float
  , vertexCounter : Int
  , edgeCounter : Int
  , position : Point
  , mousePosition : Point
  , mouseDown : Bool
  , hasMovedWhileMouseDown : Bool
  , mouseDownStartPosition : Point
  , mapFieldVisible : Bool
  , mapFieldInput : String
  , mapFieldState : BackgroundState
  , zoom : Float
  , drawingEdge : Maybe Edge
  , activeEdgeDrawingMode : EdgeType
  }

type MouseButton
  = Primary
  | Secondary
  | Wheel
  | Other


type alias MouseEvent =
  { position : Point
  , movement : Point
  , button : MouseButton
  }

type alias ScrollEvent =
  { deltaX : Float
  }

type alias Point = { x : Float, y : Float}
type alias CanvasPoint = { x : Float, y : Float}

type alias DragEvent =
  { movementX : Float
  , movementY : Float
  }


type alias Flags =
  { width : Float
  , height : Float
  , savedBackground : String
  }

init : Flags -> (Model, Cmd Msg)
init flags =
  ( { background = ""
    , animations =
      { expandedPoint = Animator.init Nothing
      }
    , texture = Nothing
    , vertices = Dict.empty
    , edges = Dict.empty
    , width = flags.width
    , height = flags.height
    , vertexCounter = 0
    , edgeCounter = 0
    , position = Point 0 0
    , mousePosition = Point 0 0
    , mouseDown = False
    , hasMovedWhileMouseDown = False
    , mouseDownStartPosition = Point 0 0
    , mapFieldVisible = False
    , mapFieldInput = flags.savedBackground
    , mapFieldState = Loading
    , zoom = 1
    , drawingEdge = Nothing
    , activeEdgeDrawingMode = Lift
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = Noop
  | TextureLoaded (Maybe Canvas.Texture.Texture)
  | MouseDown MouseEvent
  | MouseUp MouseEvent
  | MouseMove MouseEvent
  | MouseLeave
  | SetMapFieldVisible Bool
  | TrySettingBackground String
  | DimensionsChanged (Float, Float)
  | ZoomChanged Float
  | AnimationFrame Time.Posix
  | SetActiveEdgeType EdgeType

port saveToLocalStorage : (String, String) -> Cmd msg
port dimensionsChanged : ((Float, Float) -> msg) -> Sub msg

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
            , background = model.mapFieldInput
            , mapFieldState = Ok
            }
          , saveToLocalStorage ("background", model.mapFieldInput)
          )
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
        , mapFieldInput = if not bool && String.isEmpty model.mapFieldInput then model.background else model.mapFieldInput
        , mapFieldState = if not bool && String.isEmpty model.mapFieldInput then Loading else model.mapFieldState
        }
      , Cmd.none
      )

    TrySettingBackground string ->
      ( { model | mapFieldInput = string, mapFieldState = Loading}, Cmd.none)

    DimensionsChanged (width, height) ->
      let
        w = Maybe.withDefault width <| Maybe.map (\t -> (Canvas.Texture.dimensions t).width ) model.texture
        h = Maybe.withDefault height <| Maybe.map (\t -> (Canvas.Texture.dimensions t).height ) model.texture
        zoomAfter = max (model.zoom) (max ( width / w ) ( height / h ) )
        newModel = { model | width = width, height = height }
      in
      ( { newModel | zoom = zoomAfter
        , position = constrainBackgroundToCanvas newModel newModel.position
        }
      , Cmd.none
      )

    ZoomChanged delta ->
      let
        w = Maybe.withDefault model.width <| Maybe.map (\t -> (Canvas.Texture.dimensions t).width ) model.texture
        h = Maybe.withDefault model.height <| Maybe.map (\t -> (Canvas.Texture.dimensions t).height ) model.texture
        zoomBefore = model.zoom
        zoomAfter = max (model.zoom - ( delta / 1000) ) (max ( model.width / w ) ( model.height / h ) )
      in
      ( { model | zoom = zoomAfter
        , position = constrainBackgroundToCanvas { model | zoom = zoomAfter}
          <| subPoints model.mousePosition
          <| mulPoint (canvasPointToBackgroundPoint model.mousePosition model.position zoomBefore) zoomAfter
        }
      , Cmd.none
      )

    MouseLeave ->
      ( { model | mouseDown = False }, Cmd.none )

    SetActiveEdgeType edgeType ->
      ( { model | activeEdgeDrawingMode = edgeType}, Cmd.none )


checkMouseEventForPointHover : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkMouseEventForPointHover event (model, cmd) =
  ({ model | animations = animateHoveredPoint model.animations <| getHoveringVertex model event }, cmd )



setModelMousePosition : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
setModelMousePosition event (model, cmd) =
  ({ model | mousePosition = event.position }, cmd )


checkModelDragging : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkModelDragging event (model, cmd) =
  if model.mouseDown && (not <| maybeHasValue model.drawingEdge) then
    let
      new = addPoints event.movement model.position
    in
    ( { model
      | hasMovedWhileMouseDown = True
      , position = constrainBackgroundToCanvas model new
      }
    , cmd
    ) else (model,cmd)

checkVertexCreation : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkVertexCreation event (model, cmd) =
  let
    id = String.fromInt model.vertexCounter
    condition = model.hasMovedWhileMouseDown || maybeHasValue model.drawingEdge || event.button /= Primary
  in
  ( { model
    | vertices =
      if condition
      then model.vertices
      else Dict.insert id ( Vertex id Nothing <| canvasPointToBackgroundPoint event.position model.position model.zoom ) model.vertices
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
      | drawingEdge = Just { edge | points = List.append edge.points [ canvasPointToBackgroundPoint event.position model.position model.zoom ] }
      }
    (_, _, _) -> model
  , cmd
  )

checkConnectDrawing : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkConnectDrawing event (model, cmd) =
  let maybeVertex = getHoveringVertex model event in
  ( case (event.button, maybeVertex, model.drawingEdge) of
    (Primary, Just vertex, Just edge) ->
      if vertex /= edge.start then
      { model | drawingEdge = Nothing
      , edgeCounter = model.edgeCounter + 1
      , edges = Dict.insert edge.id { edge | end = Just vertex, edgeType = model.activeEdgeDrawingMode } model.edges
      } else model
    (Primary, Nothing, Just edge) ->
      { model | drawingEdge = Just { edge | points = edge.points ++ [ canvasPointToBackgroundPoint event.position model.position model.zoom ] }
      }
    (_, _, _) -> model
  , cmd
  )


checkToStartDrawing : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkToStartDrawing event (model, cmd) =
  case (event.button, getHoveringVertex model event, model.drawingEdge) of
    ( Primary, Just vertex, Nothing ) ->
      let edgeId = String.fromInt <| model.edgeCounter + 1 in
      ( { model | drawingEdge = Just <| Edge edgeId ( Just <| "Edge " ++ edgeId) vertex Nothing Unfinished []
        }
      , cmd
      )
    (_, _, _) -> (model, cmd)


getHoveringVertex : Model -> MouseEvent -> Maybe Vertex
getHoveringVertex model event =
  List.head <| List.filter (\v -> mouseOverPoint model.position model.zoom event.position v.position ) <| Dict.values model.vertices

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ dimensionsChanged DimensionsChanged
    , Animator.toSubscription AnimationFrame model.animations animator
    ]


-- ANIMATOR
animator : Animator.Animator Animations
animator =
  Animator.animator
    |> Animator.watching
      .expandedPoint
      (\newPoint model -> { model | expandedPoint = newPoint })

animateHoveredPoint : Animations -> Maybe Vertex -> Animations
animateHoveredPoint animations newHover =
  { animations | expandedPoint = animations.expandedPoint |> Animator.go Animator.quickly newHover }



-- VIEW


view : Model -> Html.Html Msg
view model =
  Html.main_
  [ Attr.class "h-screen w-screen overflow-hidden" ]
  [ Canvas.toHtmlWith
    { width = ceiling model.width
    , height = ceiling model.height
    , textures = [ Canvas.Texture.loadFromImageUrl model.mapFieldInput TextureLoaded ]
    }
    [ Attr.class "h-full w-full"
    , Events.on "mousedown" <| mouseDecoder MouseDown
    , Events.on "mouseup" <| mouseDecoder MouseUp
    , Events.on "mouseleave" <| mouseDecoder (\_ -> MouseLeave)
    , Events.on "mousemove" <| mouseDecoder MouseMove
    , Events.on "wheel" <| scrollDecoder ZoomChanged
    ]
    [ Canvas.group
      [ Canvas.Settings.Advanced.transform
        [ Canvas.Settings.Advanced.translate model.position.x model.position.y
        , Canvas.Settings.Advanced.scale model.zoom model.zoom
        ]
      ]
      <| addBackground model.width model.height (0, 0) model.texture
      [ Canvas.group []
        <| (List.map (edgeView model) <| Dict.values model.edges)
        ++ (List.map (vertexView model) <| Dict.values model.vertices)
        ++
        case model.drawingEdge of
          Nothing -> []
          Just edge ->
            [ edgeView model edge ]
      ]
    ]
  , mapField model
  , modeSelectionButtons model.activeEdgeDrawingMode
  ]

pointToCanvasLibPoint point =
  (point.x, point.y)

mouseDecoder msg =
  D.map5
    ( \offsetX offsetY movementX movementY button ->
      msg <| MouseEvent (Point offsetX offsetY) (Point movementX movementY)
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

vertexView : Model -> Vertex -> Canvas.Renderable
vertexView model vertex =
  Canvas.group
  []
  [ Canvas.shapes
    []
    [ Canvas.circle
      ( vertex.position.x, vertex.position.y)
      (
        ( if vertexExpansionCondition model vertex
          then Animator.move model.animations.expandedPoint vertexMovementAnimation
          else if vertexEdgeDrawingCondition model vertex
          then 1.5 * pointSize
          else pointSize
        ) / model.zoom
      )
    ]
  , Canvas.shapes
    [ Canvas.Settings.fill <| if vertexEdgeDrawingCondition model vertex then Color.green else Color.white ]
    [ Canvas.circle
      ( vertex.position.x, vertex.position.y)
      (
       ( if vertexExpansionCondition model vertex
         then Animator.linear model.animations.expandedPoint vertexFillAnimation
         else if vertexEdgeDrawingCondition model vertex
        then pointSize
        else 0
       ) / model.zoom
      )
    ]
  ]

vertexMovementAnimation : Maybe Vertex -> Animator.Movement
vertexMovementAnimation state =
  Animator.at <|
  case state of
    Nothing -> pointSize
    Just _ -> 1.5 * pointSize

vertexFillAnimation : Maybe Vertex -> Animator.Movement
vertexFillAnimation state =
  Animator.at <|
  case state of
    Nothing -> 0
    Just _ -> pointSize

vertexExpansionCondition : Model -> Vertex -> Bool
vertexExpansionCondition model vertex =
  (Animator.current model.animations.expandedPoint == Just vertex)
  || (Animator.arrived model.animations.expandedPoint == Just vertex)
  || (Animator.previous model.animations.expandedPoint == Just vertex)


vertexEdgeDrawingCondition : Model -> Vertex -> Bool
vertexEdgeDrawingCondition model vertex =
  ( Maybe.map .start model.drawingEdge == Just vertex)


edgeView : Model -> Edge -> Canvas.Renderable
edgeView model edge =
  Canvas.shapes
  (edgeStyle edge.edgeType model.zoom)
  [ Canvas.path (pointToCanvasLibPoint edge.start.position)
    <| List.map
      (Canvas.lineTo << pointToCanvasLibPoint)
      <| edge.points
      ++ [ case edge.end of
             Nothing -> canvasPointToBackgroundPoint model.mousePosition model.position model.zoom
             Just vertex -> vertex.position
         ]
  ]


edgeStyle : EdgeType -> Float -> List Canvas.Settings.Setting
edgeStyle edgeType zoom =
  [ Canvas.Settings.Line.lineWidth <| lineWidth / zoom
  , Canvas.Settings.Line.lineCap Canvas.Settings.Line.RoundCap
  , Canvas.Settings.Line.lineJoin Canvas.Settings.Line.RoundJoin
  ] ++
  case edgeType of
    SkiRun skiRunType ->
      case skiRunType of
        Easy -> [ Canvas.Settings.stroke Color.blue ]
        Medium -> [ Canvas.Settings.stroke Color.red ]
        Difficult -> [ Canvas.Settings.stroke Color.black ]
        SkiRoute -> [ Canvas.Settings.stroke Color.red, Canvas.Settings.Line.lineDash [5, 5] ]
    Lift -> [ Canvas.Settings.stroke Color.black ]
    Unfinished -> [ Canvas.Settings.stroke Color.green ]


addBackground width height position texture renderables =
  case texture of
    Nothing -> renderables
    Just t ->
      [ Canvas.clear (0, 0) width height, Canvas.texture [] position t ] ++ renderables


constrainBackgroundToCanvas : Model -> Point -> Point
constrainBackgroundToCanvas model new =
  let
    w = Maybe.withDefault model.width <| Maybe.map (\t -> (Canvas.Texture.dimensions t).width ) model.texture
    h = Maybe.withDefault model.height <| Maybe.map (\t -> (Canvas.Texture.dimensions t).height ) model.texture
  in
  Point
    (min 0 <| max (0 - w * model.zoom + model.width) new.x )
    (min 0 <| max (0 - h * model.zoom + model.height) new.y )

canvasPointToBackgroundPoint canvasPoint backgroundPosition zoomLevel =
  mulPoint (subPoints canvasPoint backgroundPosition) <| 1 / zoomLevel

backgroundPointToCanvasPoint backgroundPoint backgroundPosition zoomLevel =
  addPoints backgroundPosition <| mulPoint backgroundPoint zoomLevel

pointSize = 10
lineWidth = 5

mouseOverPoint backgroundPosition zoomLevel mousePosition point =
  let a = backgroundPointToCanvasPoint point backgroundPosition zoomLevel in
    pointSize >= (pointToLength <| subPoints a mousePosition)

pointToLength point =
   sqrt <| point.x^2 + point.y^2


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
      Ok -> []
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
      [ Attr.class "rounded-full shadow-lg bg-blue-400 text-yellow-300 p-4 text-center"
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
    [ btn Lift
    , btn <| SkiRun Easy
    , btn <| SkiRun Medium
    , btn <| SkiRun Difficult
    , btn <| SkiRun SkiRoute
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

modeSelectionButton : EdgeType -> EdgeType -> Html.Html Msg
modeSelectionButton selected edgeType =
  let
    active = selected == edgeType
    icon =
      case edgeType of
        SkiRun skiRunType ->
          Icons.skiRunIcon <|
          case skiRunType of
            Easy -> "#0000ff"
            Medium -> "#ff0000"
            Difficult -> "#000000"
            SkiRoute -> "#890202"
        Lift -> Icons.liftIcon
        Unfinished -> Html.text ""

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


addPoints : Point -> Point -> Point
addPoints a b =
  Point (a.x + b.x) (a.y + b.y)

subPoints : Point -> Point -> Point
subPoints a b =
  Point (a.x - b.x) (a.y - b.y)

mulPoint : Point -> Float -> Point
mulPoint point coef =
  Point (point.x * coef) (point.y * coef)


maybeHasValue : Maybe a -> Bool
maybeHasValue m =
  case m of
    Nothing -> False
    Just _ -> True
