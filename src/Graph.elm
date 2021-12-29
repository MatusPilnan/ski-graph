module Graph exposing (..)

import Dict exposing (Dict)
type alias VertexID = Int
type alias EdgeID = Int
type alias Point = { x : Float, y : Float}

type alias Vertex =
  { id : VertexID
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
  { id : EdgeID
  , title : Maybe String
  , start : Vertex
  , end : Maybe Vertex
  , edgeType : EdgeType
  , points : List Point
  }

type alias Graph =
  { title : String
  , id : String
  , background : String
  , vertices : Dict VertexID Vertex
  , edges : Dict EdgeID Edge
  , backgroundPosition : Point
  , zoom : Float
  }

updateGraphProperty : (a -> Graph -> Graph) -> a -> Maybe Graph -> Maybe Graph
updateGraphProperty func new graph =
  Maybe.map (func new) graph


setBackground : String -> Graph -> Graph
setBackground new graph =
  { graph | background = new }

setZoom : Float -> Graph -> Graph
setZoom new graph =
  { graph | zoom = new }

setPosition : Point -> Graph -> Graph
setPosition new graph =
  { graph | backgroundPosition = new }

addVertex : VertexID -> Point -> Graph -> Graph
addVertex id position graph =
  { graph | vertices = Dict.insert id (Vertex id Nothing position) graph.vertices }

addEdge : Edge -> Graph -> Graph
addEdge edge graph =
  { graph | edges = Dict.insert edge.id edge graph.edges }

getBackground : Maybe Graph -> String
getBackground graph =
  getProperty .background "" graph

getZoom : Maybe Graph -> Float
getZoom graph =
  getProperty .zoom 1 graph

getPosition : Maybe Graph -> Point
getPosition graph =
  getProperty .backgroundPosition zeroPoint graph

getVertices : Maybe Graph -> Dict VertexID Vertex
getVertices graph =
  getProperty .vertices Dict.empty graph


getVerticesList : Maybe Graph -> List Vertex
getVerticesList graph =
  Dict.values <| getVertices graph

getEdges : Maybe Graph -> Dict EdgeID Edge
getEdges graph =
  getProperty .edges Dict.empty graph

getEdgesList : Maybe Graph -> List Edge
getEdgesList graph =
  Dict.values <| getEdges graph

getProperty : (Graph -> b) -> b -> Maybe Graph -> b
getProperty property default graph =
  Maybe.withDefault default <| Maybe.map property graph


zeroPoint : Point
zeroPoint = Point 0 0
