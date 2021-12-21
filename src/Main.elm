module Main exposing (..)

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
  , mapFieldVisible : Bool
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
    , mapFieldVisible = False
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
  | SetMapFieldVisible Bool


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

    SetMapFieldVisible bool ->
      ( { model | mapFieldVisible = bool}, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Html.Html Msg
view model =
  Html.main_
  [ Attr.class "h-screen w-screen overflow-hidden" ]
  [ Canvas.toHtmlWith
    { width = ceiling model.width
    , height = ceiling model.height
    , textures = [ Canvas.Texture.loadFromImageUrl model.background TextureLoaded ]
    }
    [ Attr.class "h-full w-full"
    , Events.on "mousedown" <| mouseDecoder MouseDown
    , Events.on "mouseup" <| mouseDecoder MouseUp
    , Events.on "mousemove" <| moveDecoder MouseMove
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
  , Html.div
    [ Attr.class "fixed bottom-4 right-4 flex items-center" ]
    [ Html.input
      [ Attr.classList
        [("max-w-0", not model.mapFieldVisible)
        ,("max-w-xs border-2 px-4 py-2", model.mapFieldVisible)
        ]
      , Attr.class "w-max rounded-full shadow-lg border-primary transition-all mr-2"
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
