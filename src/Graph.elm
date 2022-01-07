module Graph exposing (..)

import Color
import Dict exposing (Dict)
import Geometry exposing (Point)
type alias VertexID = Int
type alias EdgeID = Int
type alias GraphID = String

type alias SkiRunForkPercentages = Dict String Float


type VertexType
  = LiftStation
  | SkiRunFork SkiRunForkPercentages


type alias Vertex =
  { id : VertexID
  , title : Maybe String
  , vertexType : VertexType
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
  , boundBoxTopLeft : Point
  , boundBoxBottomRight : Point
  , points : List Point
  }

type alias Graph =
  { title : String
  , id : GraphID
  , background : String
  , vertices : Dict VertexID Vertex
  , edges : Dict EdgeID Edge
  , backgroundPosition : Point
  , zoom : Float
  }

type GraphLocation = Local | Remote


type alias GraphIndexEntry =
  { title : String
  , path : String
  , id : GraphID
  , location : GraphLocation
  }

edgeTypeToString : EdgeType -> String
edgeTypeToString edgeType =
  case edgeType of
    SkiRun skiRunType ->
      String.append "skirun." <| skiRunTypeToString skiRunType
    Lift -> "lift"
    Unfinished -> "unfinished"

skiRunTypeToString : SkiRunType -> String
skiRunTypeToString skiRunType =
  case skiRunType of
    Easy -> "easy"
    Medium -> "medium"
    Difficult -> "difficult"
    SkiRoute -> "ski-route"


edgeTypeToNiceString : EdgeType -> String
edgeTypeToNiceString edgeType =
  case edgeType of
    SkiRun skiRunType ->
      String.append "Ski run - " <| skiRunTypeToNiceString skiRunType
    Lift -> "Ski lift"
    Unfinished -> "Unfinished"

skiRunTypeToNiceString : SkiRunType -> String
skiRunTypeToNiceString skiRunType =
  case skiRunType of
    Easy -> "easy"
    Medium -> "medium"
    Difficult -> "difficult"
    SkiRoute -> "ski-route"

skiRunTypeFromString : String -> SkiRunType
skiRunTypeFromString skiRunType =
  case skiRunType of
    "easy" -> Easy
    "medium" -> Medium
    "difficult" -> Difficult
    "ski-route" -> SkiRoute
    _ -> SkiRoute



skiRunColor skiRunType =
  case skiRunType of
    Easy -> Color.blue
    Medium -> Color.red
    Difficult -> Color.black
    SkiRoute -> Color.fromRgba { red = 137 / 256, green = 2 / 256, blue = 2/ 256, alpha = 1 }


skiRunColorCode skiRunType =
  Color.toCssString <| skiRunColor skiRunType

init =
  Graph "New graph" "graph-id" "" Dict.empty Dict.empty zeroPoint 1



calculateForkPercentages : List EdgeType -> SkiRunForkPercentages
calculateForkPercentages types =
  let
    skiRunTypes =
      List.filterMap
      (\edgeType ->
        case edgeType of
          SkiRun skiRunType -> Just skiRunType
          _ -> Nothing
      ) types
    total = toFloat <| List.length skiRunTypes
  in
  List.foldl
  ( \skiRunType percentages ->
    Dict.update (skiRunTypeToString skiRunType) (\count -> Just <| Maybe.withDefault (1 / total) <| Maybe.map (\a -> a + 1 / total) count) percentages
  ) Dict.empty skiRunTypes


selectVertexType : Vertex -> List EdgeType -> Vertex
selectVertexType vertex edgeTypes =
  let percentages = calculateForkPercentages edgeTypes in
  if
    List.any
    (\et ->
      case et of
        SkiRun _ -> False
        _ -> True
    ) edgeTypes
  then { vertex | vertexType = LiftStation } else { vertex | vertexType = SkiRunFork percentages }


updateGraphProperty : (a -> Graph -> Graph) -> a -> Maybe Graph -> Maybe Graph
updateGraphProperty func new graph =
  Maybe.map (func new) graph


calculateVertexTypes : Graph -> Graph
calculateVertexTypes graph =
  let
    update edge types =
      case types of
        Nothing -> Just <| [ edge.edgeType ]
        Just list -> Just <| list ++ [ edge.edgeType ]
    vertexEdgeTypes =
      Dict.values graph.edges
      |> List.foldl
        (\edge edgeTypes ->
          Dict.update edge.start.id (update edge) edgeTypes
          |> (
            case edge.end of
              Just v ->
                Dict.update v.id (update edge)
              Nothing ->
                identity
          )
        ) Dict.empty

  in
    { graph | vertices =
        Dict.merge
          (\_ _ result -> result )
          --(\vertexId edgeTypes vertices ->
          --  Dict.update vertexId
          --  (Maybe.map (\v -> selectVertexType v edgeTypes)) vertices
          --)
          (\vertexId edgeTypes vertex vertices ->
            Dict.insert vertexId (selectVertexType vertex edgeTypes) vertices
          )
          (\vertexId vertex vertices ->
            Dict.insert vertexId (vertex) vertices
          )
          (vertexEdgeTypes)
          (graph.vertices)
          Dict.empty
    }


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
  { graph | vertices = Dict.insert id (Vertex id Nothing LiftStation position) graph.vertices }

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

getNextVertexId : Graph -> Int
getNextVertexId graph =
  Maybe.withDefault 0 <| Maybe.map (\x -> x + 1) <| List.maximum <| Dict.keys graph.vertices

getNextEdgeId : Graph -> Int
getNextEdgeId graph =
  Maybe.withDefault 0 <| Maybe.map (\x -> x + 1) <| List.maximum <| Dict.keys graph.edges


zeroPoint : Point
zeroPoint = Point 0 0



edgesTitleComparator : Edge -> Edge -> Order
edgesTitleComparator a b =
  case (a.title, b.title) of
    (Nothing, Nothing) ->
      compare a.id b.id
    (Just _, Nothing) ->
      GT
    (Nothing, Just _) ->
      LT
    (Just titleA, Just titleB) ->
      compare titleA titleB
