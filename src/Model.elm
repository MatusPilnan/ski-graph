module Model exposing (..)

import Animator
import Canvas.Texture
import Dict exposing (Dict)
import Geometry as Geom
import Graph
import Menus.Menus as Menus


type BackgroundState = Loaded | Loading | Invalid

type alias Animations =
  { expandedPoint : Animator.Timeline (Maybe Graph.Vertex)
  , highlightedEdge : Animator.Timeline Float
  }


-- MODEL


type alias Model =
  { currentGraph : Maybe Graph.Graph
  , localGraphIndex : Dict Graph.GraphID Graph.GraphIndexEntry
  , remoteGraphIndex : Dict Graph.GraphID Graph.GraphIndexEntry
  , selectedGraphIndexEntry : Maybe Graph.GraphIndexEntry
  , animations : Animations
  , texture :  Maybe Canvas.Texture.Texture
  , width : Float
  , height : Float
  , vertexCounter : Int
  , edgeCounter : Int
  , mousePosition : Geom.ViewportPoint
  , mouseDown : Bool
  , hasMovedWhileMouseDown : Bool
  , mouseDownStartPosition : Geom.ViewportPoint
  , mapFieldVisible : Bool
  , mapFieldInput : String
  , mapFieldState : BackgroundState
  , drawingEdge : Maybe Graph.Edge
  , activeEdgeDrawingMode : Graph.EdgeType
  , baseUrl : String
  , menu : Menus.Model
  }

type MouseButton
  = Primary
  | Secondary
  | Wheel
  | Other


type alias MouseEvent =
  { position : Geom.ViewportPoint
  , movement : Geom.ViewportPoint
  , button : MouseButton
  }

type alias ScrollEvent =
  { deltaX : Float
  }


type alias DragEvent =
  { movementX : Float
  , movementY : Float
  }


type alias Flags =
  { width : Float
  , height : Float
  , savedBackground : String
  , graphJson : Maybe String
  , selectedGraphID : Maybe String
  , baseUrl : String
  , localGraphIndex : Maybe String
  }
