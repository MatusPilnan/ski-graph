module Saves exposing (..)

import Dict
import Json.Encode as E
import Model exposing (..)

graphToJson : Model -> Int -> String
graphToJson model indent =
  E.encode indent <|
    E.object
      [ ("zoom", E.float model.zoom)
      , ("background", E.string model.background)
      , ("vertices", E.list vertexToJson <| Dict.values model.vertices )
      , ("edges", E.list edgeToJson <| Dict.values model.edges)
      ]


vertexToJson : Vertex -> E.Value
vertexToJson vertex =
  E.object
    [ ("id", E.string vertex.id)
    , ("title", encodeMaybe E.string vertex.title)
    , ("position", pointToJson vertex.position)
    ]


edgeToJson : Edge -> E.Value
edgeToJson edge =
  E.object
    [ ("id", E.string edge.id)
    , ("title", encodeMaybe E.string edge.title)
    , ("points", E.list pointToJson edge.points)
    , ("start_id", E.string edge.start.id)
    , ("end_id", encodeMaybe E.string <| Maybe.map .id edge.end)
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
