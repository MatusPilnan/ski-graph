port module Main exposing (..)

import Animator
import Browser
import Canvas.Settings
import Canvas.Settings.Advanced
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

type alias Edge =
  { id : String
  , title : Maybe String
  , start : Vertex
  , end : Vertex
  , edgeType : EdgeType
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

    MouseDown mouseEvent ->
      ( model, Cmd.none )
      |> checkToStartDrag mouseEvent

    MouseMove mouseEvent ->
      (model, Cmd.none)
      |> setModelMousePosition mouseEvent
      |> checkMouseEventForPointHover mouseEvent
      |> checkModelDragging mouseEvent


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


checkMouseEventForPointHover : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkMouseEventForPointHover event (model, cmd) =
  let
    newHovered = List.head <| List.filter (\v -> mouseOverPoint model.position model.zoom event.position v.position ) <| Dict.values model.vertices
  in
  ({ model | animations = animateHoveredPoint model.animations newHovered }, cmd )



setModelMousePosition : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
setModelMousePosition event (model, cmd) =
  ({ model | mousePosition = event.position }, cmd )


checkModelDragging : MouseEvent -> (Model, Cmd Msg) -> (Model, Cmd Msg)
checkModelDragging event (model, cmd) =
  if model.mouseDown then
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
  in
  ( { model
    | vertices =
      if model.hasMovedWhileMouseDown
      then model.vertices
      else Dict.insert id ( Vertex id Nothing <| canvasPointToBackgroundPoint event.position model.position model.zoom ) model.vertices
    , vertexCounter =
      if model.hasMovedWhileMouseDown
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
      [ Canvas.group [] <| List.map (vertexView model) <| Dict.values model.vertices
      ]
    ]
  , mapField model
  ]

mouseDecoder msg =
  D.map5
    ( \offsetX offsetY movementX movementY button ->
      msg <| MouseEvent (Point offsetX offsetY) (Point movementX movementY)
      <| case button of
          0 -> Primary
          1 -> Secondary
          2 -> Wheel
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
        ( if (Animator.current model.animations.expandedPoint == Just vertex)
            || (Animator.arrived model.animations.expandedPoint == Just vertex)
            || (Animator.previous model.animations.expandedPoint == Just vertex)
          then Animator.move model.animations.expandedPoint vertexMovementAnimation
          else pointSize
        ) / model.zoom
      )
    ]
  , Canvas.shapes
    [ Canvas.Settings.fill Color.white ]
    [ Canvas.circle
      ( vertex.position.x, vertex.position.y)
      (
       ( if (Animator.current model.animations.expandedPoint == Just vertex)
           || (Animator.arrived model.animations.expandedPoint == Just vertex)
           || (Animator.previous model.animations.expandedPoint == Just vertex)
         then Animator.linear model.animations.expandedPoint vertexFillAnimation
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

addPoints : Point -> Point -> Point
addPoints a b =
  Point (a.x + b.x) (a.y + b.y)

subPoints : Point -> Point -> Point
subPoints a b =
  Point (a.x - b.x) (a.y - b.y)

mulPoint : Point -> Float -> Point
mulPoint point coef =
  Point (point.x * coef) (point.y * coef)
