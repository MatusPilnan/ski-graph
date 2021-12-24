port module Main exposing (..)

import Browser
import Canvas.Settings.Advanced
import Canvas.Texture
import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Canvas
import Icons
import Json.Decode as D
import Loading exposing (defaultConfig)



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

type EdgeType
  = Run
  | Lift

type alias Edge =
  { id : String
  , title : Maybe String
  , start : Vertex
  , end : Vertex
  , edgeType : EdgeType
  }

type BackgroundState = Ok | Loading | Invalid

-- MODEL


type alias Model =
  { background : String
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


type alias MouseEvent =
  { position : Point
  , movement : Point
  }

type alias ScrollEvent =
  { deltaX : Float
  }

type alias Point = { x : Float, y : Float}

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

port saveToLocalStorage : (String, String) -> Cmd msg
port dimensionsChanged : ((Float, Float) -> msg) -> Sub msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      (model, Cmd.none)
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
      let
        id = String.fromInt model.vertexCounter
      in
      ( { model
        | vertices =
          if model.hasMovedWhileMouseDown
          then model.vertices
          else Dict.insert id ( Vertex id Nothing <| canvasPointToBackgroundPoint mouseEvent.position model.position model.zoom ) model.vertices
        , vertexCounter =
          if model.hasMovedWhileMouseDown
          then model.vertexCounter
          else model.vertexCounter + 1
        , mouseDown = False
        }
      , Cmd.none
      )

    MouseDown mouseEvent ->
      ( { model
        | mouseDown = True
        , hasMovedWhileMouseDown = False
        , mouseDownStartPosition = mouseEvent.position
        , mapFieldVisible = False
        }
      , Cmd.none
      )

    MouseMove mouseEvent ->
      let newModel = { model | mousePosition = mouseEvent.position } in
      if model.mouseDown then
      let
        new = addPoints mouseEvent.movement model.position
      in
      ( { newModel
        | hasMovedWhileMouseDown = True
        , position = constrainBackgroundToCanvas model new
        }
      , Cmd.none
      ) else (newModel, Cmd.none)

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





-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  dimensionsChanged DimensionsChanged



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
      [ Canvas.shapes [] <| List.map (\v -> Canvas.circle ( v.position.x, v.position.y) (10 / model.zoom) ) <| Dict.values model.vertices
      ]
    ]
  , Html.div
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
  ]

mouseDecoder msg =
  D.map4 (\offsetX offsetY movementX movementY -> msg <| MouseEvent (Point offsetX offsetY) (Point movementX movementY))
    ( D.field "offsetX" D.float)
    ( D.field "offsetY" D.float)
    ( D.field "movementX" D.float)
    ( D.field "movementY" D.float)

scrollDecoder msg =
   D.map msg <| D.field "deltaY" D.float


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
  mulPoint (addPoints backgroundPoint backgroundPosition) zoomLevel

addPoints : Point -> Point -> Point
addPoints a b =
  Point (a.x + b.x) (a.y + b.y)

subPoints : Point -> Point -> Point
subPoints a b =
  Point (a.x - b.x) (a.y - b.y)

mulPoint : Point -> Float -> Point
mulPoint point coef =
  Point (point.x * coef) (point.y * coef)
