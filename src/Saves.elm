module Saves exposing (..)

import Dict
import Json.Encode as E
import Json.Decode as D
import Model exposing (..)
import Utils

graphToJson : Model -> Int -> String
graphToJson model indent =
  E.encode indent <|
    E.object
      [ ("zoom", E.float model.zoom)
      , ("background", E.string model.background)
      , ("position", pointToJson model.position)
      , ("vertices", E.list vertexToJson <| Dict.values model.vertices )
      , ("edges", E.list edgeToJson <| Dict.values model.edges)
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



graphFromJson : Maybe String -> Model -> Model
graphFromJson jsonString model =
  case jsonString of
    Nothing -> model
    Just json ->
      case D.decodeString (graphDecoder model) json of
        Ok value ->  Debug.log "value" value
        Err e -> let _ = Debug.log "error" e in model



graphDecoder : Model -> D.Decoder Model
graphDecoder model =
  D.map4
    ( \zoom background position (vertices, edges) ->
      { model
      | zoom = zoom
      , background = background
      , vertices = vertices
      , edges = edges
      , position = position
      , vertexCounter = Maybe.withDefault 0 <| List.maximum <| List.map .id <| Dict.values vertices
      , edgeCounter = Maybe.withDefault 0 <| List.maximum <| List.map .id <| Dict.values edges
      }
    )
    (D.field "zoom" D.float)
    (D.field "background" D.string)
    (D.field "position" pointDecoder)
    (edgeDecoder)

vertexDecoder : D.Decoder Vertex
vertexDecoder =
  D.map3 Vertex
    (D.field "id" D.int)
    (D.field "title" <| D.nullable D.string)
    (D.field "position" <| pointDecoder )


edgeDecoder : D.Decoder (Dict.Dict Int Vertex, Dict.Dict Int Edge)
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
