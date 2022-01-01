module Saves exposing (..)

import Dict exposing (Dict)
import Graph exposing (..)
import Json.Encode as E
import Json.Decode as D
import Model exposing (..)
import Utils


--graphIndexEntryLocationEncoder : GraphLocation -> E.Value
--graphIndexEntryLocationEncoder loc =
--  E.string <|
--    case loc of
--      Local -> "local"
--      Remote -> "remote"



graphIndexEntryEncoder : GraphIndexEntry -> E.Value
graphIndexEntryEncoder entry =
  E.object
  [ ("title", E.string entry.title)
  , ("path", E.string entry.path)
  , ("id", E.string entry.id)
  ]

graphIndexEncoder : Dict GraphID GraphIndexEntry -> E.Value
graphIndexEncoder index =
  E.list graphIndexEntryEncoder (Dict.values index)


--graphIndexEntryLocationDecoder : D.Decoder GraphLocation
--graphIndexEntryLocationDecoder =
--  D.string
--    |> D.andThen
--      ( \loc ->
--        case loc of
--          "local" -> D.succeed Local
--          "remote" -> D.succeed Remote
--          other -> D.fail <| "Unrecognized location type: " ++ other
--      )


graphIndexDecoder : Graph.GraphLocation -> D.Decoder (Dict Graph.GraphID Graph.GraphIndexEntry)
graphIndexDecoder location =
  D.map Dict.fromList <|
  D.list <|
  D.map3 (\title path id -> (id, Graph.GraphIndexEntry title path id location))
    (D.field "title" D.string)
    (D.field "path" D.string)
    (D.field "id" D.string)

graphIndexToJson : Dict GraphID GraphIndexEntry -> String
graphIndexToJson graphIndex =
  E.encode 0 <| graphIndexEncoder graphIndex


graphIndexFromJson : GraphLocation -> Maybe String -> Dict GraphID GraphIndexEntry
graphIndexFromJson loc graphIndexJson =
  case graphIndexJson of
    Nothing -> Dict.empty
    Just string ->
      Maybe.withDefault Dict.empty <| Result.toMaybe <| D.decodeString (graphIndexDecoder loc) string



graphToJson : Int -> Graph -> String
graphToJson indent graph =
  E.encode indent <|
    E.object
      [ ("zoom", E.float graph.zoom)
      , ("background", E.string graph.background)
      , ("position", pointToJson graph.backgroundPosition)
      , ("vertices", E.list vertexToJson <| Dict.values graph.vertices )
      , ("edges", E.list edgeToJson <| Dict.values graph.edges)
      , ("id", E.string graph.id)
      ]


vertexToJson : Vertex -> E.Value
vertexToJson vertex =
  E.object
    [ ("id", E.int vertex.id)
    , ("title", encodeMaybe E.string vertex.title)
    , ("position", pointToJson vertex.position)
    ]


edgeToJson : Edge -> E.Value
edgeToJson edge =
  E.object
    [ ("id", E.int edge.id)
    , ("title", encodeMaybe E.string edge.title)
    , ("points", E.list pointToJson edge.points)
    , ("start_id", E.int edge.start.id)
    , ("end_id", encodeMaybe E.int <| Maybe.map .id edge.end)
    , ("type", E.string <| edgeTypeToString edge.edgeType)
    ]

edgeTypeToString : EdgeType -> String
edgeTypeToString edgeType =
  case edgeType of
    SkiRun skiRunType ->
      String.append "skirun." <|
      case skiRunType of
        Easy -> "easy"
        Medium -> "medium"
        Difficult -> "difficult"
        SkiRoute -> "ski-route"
    Lift -> "lift"
    Unfinished -> "unfinished"



pointToJson : Point -> E.Value
pointToJson point =
  E.list E.float [point.x, point.y]

encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe enc m =
  case m of
    Just a -> enc a
    Nothing -> E.null


graphFromJson : Maybe String -> Maybe Graph
graphFromJson json =
  Maybe.andThen ( Result.toMaybe << D.decodeString graphDecoder) json

loadGraphFromJsonToModel : Maybe String -> Model -> Model
loadGraphFromJsonToModel jsonString model =
  case jsonString of
    Nothing -> model
    Just json ->
      case D.decodeString graphDecoder json of
        Ok graph -> { model | currentGraph = Just graph, vertexCounter = Graph.getNextVertexId graph, edgeCounter = Graph.getNextEdgeId graph }
        Err _ -> model

graphDecoder : D.Decoder Graph
graphDecoder =
  D.map5
    ( \zoom background position id (vertices, edges) ->
      Graph "" id background vertices edges position zoom
    )
    (D.field "zoom" D.float)
    (D.field "background" D.string)
    (D.field "position" pointDecoder)
    (D.field "id" D.string)
    (edgeDecoder)

vertexDecoder : D.Decoder Vertex
vertexDecoder =
  D.map3 Vertex
    (D.field "id" D.int)
    (D.field "title" <| D.nullable D.string)
    (D.field "position" <| pointDecoder )


edgeDecoder : D.Decoder (Dict.Dict VertexID Vertex, Dict.Dict EdgeID Edge)
edgeDecoder =
  D.andThen
  ( \(vertices, edgeData) ->
    let
      verts = Dict.fromList <| List.map (\v -> (v.id, v)) vertices
      starts =
        List.map
        ( \((_, _, startId), (_, _, _)) ->
          Dict.get startId verts
        ) edgeData
    in
    if List.all Utils.maybeHasValue starts
    then
      D.succeed
        ( verts
        , Dict.fromList
          <| List.map2
            ( \start ((id, title, _), (endId, edgeType, points)) ->
              (id, Edge id title start (Dict.get endId verts) edgeType points)
            ) (List.filterMap identity starts) edgeData
        )
    else
      D.fail "Some edge start Vertices were not found."
  ) <|
  D.map2 (\ a b -> (a, b))
  ( D.field "vertices" <| D.list vertexDecoder )
  ( D.field "edges"
    <| D.list
    <| D.map6 (\a b c d e f -> ((a, b, c), (d, e, f)) )
      (D.field "id" D.int)
      (D.field "title" <| D.nullable D.string)
      (D.field "start_id" D.int)
      (D.field "end_id" D.int)
      (D.field "type" edgeTypeDecoder)
      (D.field "points" <| D.list pointDecoder)
  )

pointDecoder : D.Decoder Point
pointDecoder =
  D.andThen
  ( \coordinates ->
    case coordinates of
      x :: y :: _ ->
        D.succeed <| Point x y
      [_] -> D.fail "Missing coordinate Y."
      [] ->
        D.fail "Wrong coordinates."
  ) <| D.list D.float

edgeTypeDecoder : D.Decoder EdgeType
edgeTypeDecoder =
  D.andThen
  ( \edgeType ->
    case edgeType of
      "skirun.easy" -> D.succeed <| SkiRun Easy
      "skirun.medium" -> D.succeed <| SkiRun Medium
      "skirun.difficult" -> D.succeed <| SkiRun Difficult
      "skirun.ski-route" -> D.succeed <| SkiRun SkiRoute
      "lift" -> D.succeed <| Lift
      "unfinished" -> D.succeed <| Unfinished
      v -> D.fail <| "Unknown edge type: " ++ v
  ) <| D.string
