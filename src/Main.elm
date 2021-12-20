module Main exposing (..)

import Browser
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
  , x : Int
  , y : Int
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
  , width : Int
  , height : Int
  , vertexCounter : Int
  , edgeCounter : Int
  }


type alias ClickEvent =
  { offsetX : Int
  , offsetY : Int
  }


type alias Flags =
  { width : Int
  , height : Int
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
    }
  , Cmd.none
  )



-- UPDATE


type Msg
  = Noop
  | TextureLoaded (Maybe Canvas.Texture.Texture)
  | CanvasClicked ClickEvent


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      (model, Cmd.none)
    TextureLoaded texture ->
      ({ model | texture = texture}, Cmd.none)

    CanvasClicked clickEvent ->
      let id = String.fromInt model.vertexCounter in
      ( { model
        | vertices = Dict.insert id ( Vertex id Nothing clickEvent.offsetX clickEvent.offsetY ) model.vertices
        , vertexCounter = model.vertexCounter + 1
        }
      , Cmd.none
      )




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
    { width = Maybe.withDefault model.width <| Maybe.map (\t -> ceiling (Canvas.Texture.dimensions t).width ) model.texture
    , height = Maybe.withDefault model.height <| Maybe.map (\t -> ceiling (Canvas.Texture.dimensions t).height ) model.texture
    , textures = [ Canvas.Texture.loadFromImageUrl model.background TextureLoaded ]
    }
    [ Attr.class "h-full w-full m-4"
    , Events.on "click" clickDecoder
    ]
    <| addBackground model.texture
      [ Canvas.shapes [] <| List.map (\v -> Canvas.circle (toFloat v.x, toFloat v.y) 10 ) <| Dict.values model.vertices ]
  ]

clickDecoder =
  D.map2 (\x y -> CanvasClicked <| ClickEvent (ceiling x) (ceiling y))
    ( D.field "offsetX" D.float)
    ( D.field "offsetY" D.float)



addBackground texture renderables =
  case texture of
    Nothing -> renderables
    Just t -> [ Canvas.texture [] (0,0) t ] ++ renderables
