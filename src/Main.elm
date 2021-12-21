module Main exposing (..)

import Browser
import Canvas.Settings.Advanced
import Canvas.Texture
import Dict exposing (Dict)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Canvas
import Json.Decode as D
import Canvas.Settings



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
  , mouseDown : Bool
  , hasMovedWhileMouseDown : Bool
  , mouseDownStartPosition : Point
  }


type alias ClickEvent =
  { offsetX : Float
  , offsetY : Float
  , movementX : Float
  , movementY : Float
  }

type alias Point = { x : Float, y : Float}

type alias DragEvent =
  { movementX : Float
  , movementY : Float
  }


type alias Flags =
  { width : Float
  , height : Float
  }

init : Flags -> (Model, Cmd Msg)
init flags =
  ( { background = "https://vcdn.bergfex.at/images/resized/5f/b66b2c0f20ebed5f_e600fdabdbc8f004@2x.jpg"
    , texture = Nothing
    , vertices = Dict.empty
    , edges = Dict.empty
    , width = flags.width
    , height = flags.height
    , vertexCounter = 0
    , edgeCounter = 0
    , position = Point 0 0
    , mouseDown = False
    , hasMovedWhileMouseDown = False
    , mouseDownStartPosition = Point 0 0
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = Noop
  | TextureLoaded (Maybe Canvas.Texture.Texture)
  | MouseDown Point
  | MouseUp Point
  | MouseMove Point
  | CanvasDragged Point


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      (model, Cmd.none)
    TextureLoaded texture ->
      ({ model | texture = texture}, Cmd.none)

    MouseUp point ->
      let
        id = String.fromInt model.vertexCounter
      in
      ( { model
        | vertices =
          if model.hasMovedWhileMouseDown
          then model.vertices
          else Dict.insert id ( Vertex id Nothing (subPoints point model.position) ) model.vertices
        , vertexCounter =
          if model.hasMovedWhileMouseDown
          then model.vertexCounter
          else model.vertexCounter + 1
        , mouseDown = False
        }
      , Cmd.none
      )

    MouseDown point ->
      ( { model
        | mouseDown = True
        , hasMovedWhileMouseDown = False
        , mouseDownStartPosition = point
        }
      , Cmd.none
      )

    MouseMove movement ->
      if model.mouseDown then
      let
        new = addPoints movement model.position
        w = Maybe.withDefault model.width <| Maybe.map (\t -> (Canvas.Texture.dimensions t).width ) model.texture
        h = Maybe.withDefault model.height <| Maybe.map (\t -> (Canvas.Texture.dimensions t).height ) model.texture
       in
      ( { model
        | hasMovedWhileMouseDown = True
        , position = Point
          (min 0 <| max (0 - w + model.width) new.x )
          (min 0 <| max (0 - h + model.height) new.y )
        }
      , Cmd.none
      ) else (model, Cmd.none)

    CanvasDragged point -> let _ = Debug.log "drag" point in (model, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
  Html.main_
  [ Attr.class "h-screen w-screen" ]
  [ Canvas.toHtmlWith
    { width = ceiling model.width
    , height = ceiling model.height
    , textures = [ Canvas.Texture.loadFromImageUrl model.background TextureLoaded ]
    }
    [ Attr.class "h-full w-full overflow-hidden"
    --, Attr.draggable "true"
    , Events.on "mousedown" <| mouseDecoder MouseDown
    , Events.on "mouseup" <| mouseDecoder MouseUp
    , Events.on "mousemove" <| moveDecoder MouseMove
    --, Events.on "drag" dragDecoder
    ]
    [ Canvas.group
      [ Canvas.Settings.Advanced.transform
        [ Canvas.Settings.Advanced.translate model.position.x model.position.y
        ]
      ]
      <| addBackground (0, 0) model.texture
      [ Canvas.shapes [] <| List.map (\v -> Canvas.circle ( v.position.x, v.position.y) 10 ) <| Dict.values model.vertices
      ]
    ]
  ]

mouseDecoder msg =
  D.map2 (\x y -> msg <| Point x y)
    ( D.field "offsetX" D.float)
    ( D.field "offsetY" D.float)

moveDecoder msg =
  D.map2 (\x y -> msg <| Point x y)
    ( D.field "movementX" D.float)
    ( D.field "movementY" D.float)


addBackground position texture renderables =
  case texture of
    Nothing -> renderables
    Just t -> [ Canvas.texture [] position t ] ++ renderables


addPoints : Point -> Point -> Point
addPoints a b =
  Point (a.x + b.x) (a.y + b.y)

subPoints : Point -> Point -> Point
subPoints a b =
  Point (a.x - b.x) (a.y - b.y)
