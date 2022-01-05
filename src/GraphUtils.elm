module GraphUtils exposing (..)

import Canvas.Texture
import Dict
import Geometry as Geom
import Graph
import Model exposing (Model)
import Utils


constrainBackgroundToCanvas : Model -> Geom.Point -> Geom.Point
constrainBackgroundToCanvas model new =
  let
    w = Maybe.withDefault model.width <| Maybe.map (\t -> (Canvas.Texture.dimensions t).width ) model.texture
    h = Maybe.withDefault model.height <| Maybe.map (\t -> (Canvas.Texture.dimensions t).height ) model.texture
  in
  Geom.Point
    (min (if model.menuShown then 384 else 0) <| max (0 - w * (Graph.getZoom model.currentGraph) + model.width) new.x )
    (min 0 <| max (0 - h * (Graph.getZoom model.currentGraph) + model.height) new.y )


calculateEdgeBoundingBox : Graph.Edge -> Graph.Edge
calculateEdgeBoundingBox edge =
  let
    (topLeft, bottomRight) =
      Maybe.withDefault (edge.start.position, edge.start.position)
      <| Geom.findExtremePoints
      <| edge.points
        ++ [ edge.start.position ]
        ++
        ( Maybe.withDefault []
          <| Maybe.map List.singleton
          <| Maybe.map .position edge.end
        )
  in
  { edge
  | boundBoxTopLeft = topLeft
  , boundBoxBottomRight = bottomRight
  }


mouseOverEdge : Geom.ViewportPoint -> Float -> Geom.ViewportPoint -> Graph.Edge -> Maybe Geom.Point
mouseOverEdge backgroundPosition zoom mousePosition edge =
  let
    mousePos =
      Geom.canvasPointToBackgroundPoint
        mousePosition
        backgroundPosition
        zoom
  in
    pointOverEdge mousePos zoom edge


pointOverEdge : Geom.Point -> Float -> Graph.Edge -> Maybe Geom.Point
pointOverEdge target zoom edge =
  if Geom.isPointInRect target edge.boundBoxTopLeft edge.boundBoxBottomRight
  then
    List.foldl
    ( \point (result, previousPoint) ->
      let
        (topLeft, bottomRight) = Maybe.withDefault (point, point) <| Geom.findExtremePoints [previousPoint, point]
      in
      ( if Geom.isPointInRect target topLeft bottomRight then result ++ [ (previousPoint, point) ] else result
      , point
      )
    )
    ([], edge.start.position)
    (edge.points ++
      ( Maybe.withDefault []
        <| Maybe.map List.singleton
        <| Maybe.map .position edge.end
      )
    )
    |> Tuple.first
    |> List.filterMap
      ( \(segmentStart, segmentEnd) ->
        Geom.isPointOnLine (Geom.lineFromPoints segmentStart segmentEnd) (Geom.lineWidth / zoom) target
      )
    |> List.head
  else Nothing


splitEdge : Graph.Edge -> Geom.Point -> Graph.EdgeID -> Graph.VertexID -> Graph.Graph -> Graph.Graph
splitEdge edge point newEdgeId newVertexId graph =
  let
    newVertex = Graph.Vertex newVertexId Nothing (Graph.SkiRunFork <| Graph.calculateForkPercentages [ edge.edgeType, edge.edgeType ]) point
    (pointsBeforeSplit, pointsAfterSplit) = splitEdgeSegments edge point graph.zoom
  in
  { graph
  | edges =
    Dict.insert edge.id ( calculateEdgeBoundingBox { edge | end = Just newVertex, points = pointsBeforeSplit} ) graph.edges
    |> Dict.insert newEdgeId ( calculateEdgeBoundingBox { edge | start = newVertex, points = pointsAfterSplit, id = newEdgeId })
  , vertices = Dict.insert newVertex.id newVertex graph.vertices
  }


splitEdgeSegments : Graph.Edge -> Geom.Point -> Float -> (List Geom.Point, List Geom.Point)
splitEdgeSegments edge splitPoint zoom =
  List.foldl
  ( \point ((before, after), previousPoint, found) ->
    if found || (Utils.maybeHasValue <| Geom.isPointOnLine (Geom.lineFromPoints previousPoint point) (Geom.lineWidth / zoom) splitPoint)
    then
      ((before, after ++ [ point ]), point, True)
    else
      ((before ++ [ point ], after), point, False)
  ) (([], []), edge.start.position, False) edge.points
  |> (\(result, _, _) -> result)
