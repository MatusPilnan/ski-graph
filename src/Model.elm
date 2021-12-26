module Model exposing (..)

import Animator
import Canvas.Texture
import Dict exposing (Dict)


type alias Vertex =
  { id : Int
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
  { id : Int
  , title : Maybe String
  , start : Vertex
  , end : Maybe Vertex
  , edgeType : EdgeType
  , points : List Point
  }

type BackgroundState = Loaded | Loading | Invalid

type alias Animations =
  { expandedPoint : Animator.Timeline (Maybe Vertex)
  }


-- MODEL


type alias Model =
  { background : String
  , animations : Animations
  , texture :  Maybe Canvas.Texture.Texture
  , vertices : Dict Int Vertex
  , edges : Dict Int Edge
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
  , graphJson : Maybe String
  }
