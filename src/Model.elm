module Model exposing (..)

import Animator
import Canvas.Texture
import Dict exposing (Dict)
import Graph


type BackgroundState = Loaded | Loading | Invalid

type alias Animations =
  { expandedPoint : Animator.Timeline (Maybe Graph.Vertex)
  }


-- MODEL


type alias Model =
  { currentGraph : Maybe Graph.Graph
  , graphIndex : Dict Graph.GraphID Graph.GraphIndexEntry
  , selectedGraphIndexEntryId : Maybe Graph.GraphID
  , animations : Animations
  , texture :  Maybe Canvas.Texture.Texture
  , width : Float
  , height : Float
  , vertexCounter : Int
  , edgeCounter : Int
  , mousePosition : Graph.Point
  , mouseDown : Bool
  , hasMovedWhileMouseDown : Bool
  , mouseDownStartPosition : Graph.Point
  , mapFieldVisible : Bool
  , mapFieldInput : String
  , mapFieldState : BackgroundState
  , drawingEdge : Maybe Graph.Edge
  , activeEdgeDrawingMode : Graph.EdgeType
  , baseUrl : String
  }

type MouseButton
  = Primary
  | Secondary
  | Wheel
  | Other


type alias MouseEvent =
  { position : Graph.Point
  , movement : Graph.Point
  , button : MouseButton
  }

type alias ScrollEvent =
  { deltaX : Float
  }

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
  , baseUrl : String
  }
