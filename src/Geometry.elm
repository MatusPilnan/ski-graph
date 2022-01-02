module Geometry exposing (..)

import Canvas.Texture
import Dict
import Graph
import Model exposing (..)
import Utils

type alias Line =
  { a : Float
  , b : Float
  , c : Float
  }

lineFromPoints : Graph.Point -> Graph.Point -> Line
lineFromPoints one other =
  let
    directionVector = subPoints one other
    a = (-directionVector.y)
    b = directionVector.x
    c = -((a * one.x) + (b * one.y))
  in
  Line a b c

constrainBackgroundToCanvas : Model -> Graph.Point -> Graph.Point
constrainBackgroundToCanvas model new =
  let
    w = Maybe.withDefault model.width <| Maybe.map (\t -> (Canvas.Texture.dimensions t).width ) model.texture
    h = Maybe.withDefault model.height <| Maybe.map (\t -> (Canvas.Texture.dimensions t).height ) model.texture
  in
  Graph.Point
    (min (if model.menuShown then 384 else 0) <| max (0 - w * (Graph.getZoom model.currentGraph) + model.width) new.x )
    (min 0 <| max (0 - h * (Graph.getZoom model.currentGraph) + model.height) new.y )

canvasPointToBackgroundPoint : ViewportPoint -> ViewportPoint -> Float -> Graph.Point
canvasPointToBackgroundPoint canvasPoint backgroundPosition zoomLevel =
  mulPoint (subPoints canvasPoint backgroundPosition) <| 1 / zoomLevel

backgroundPointToCanvasPoint : Graph.Point -> ViewportPoint -> Float -> ViewportPoint
backgroundPointToCanvasPoint backgroundPoint backgroundPosition zoomLevel =
  addPoints backgroundPosition <| mulPoint backgroundPoint zoomLevel

liftStationPointSize = 10
skiRunConnectionPointSize = 8
lineWidth = 5

mouseOverPoint : ViewportPoint -> Float -> ViewportPoint -> Graph.Point -> Bool
mouseOverPoint backgroundPosition zoomLevel mousePosition point =
  let a = backgroundPointToCanvasPoint point backgroundPosition zoomLevel in
    liftStationPointSize >= (pointToLength <| subPoints a mousePosition)

pointToLength point =
   sqrt <| point.x^2 + point.y^2


calculateEdgeBoundingBox : Graph.Edge -> Graph.Edge
calculateEdgeBoundingBox edge =
  let
    (topLeft, bottomRight) =
      Maybe.withDefault (edge.start.position, edge.start.position)
      <| findExtremePoints
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


mouseOverEdge : ViewportPoint -> Float -> ViewportPoint -> Graph.Edge -> Maybe Graph.Point
mouseOverEdge backgroundPosition zoom mousePosition edge =
  let
    mousePos =
      canvasPointToBackgroundPoint
        mousePosition
        backgroundPosition
        zoom
  in
    pointOverEdge mousePos zoom edge


pointOverEdge : Graph.Point -> Float -> Graph.Edge -> Maybe Graph.Point
pointOverEdge target zoom edge =
  if isPointInRect target edge.boundBoxTopLeft edge.boundBoxBottomRight
  then
    List.foldl
    ( \point (result, previousPoint) ->
      let
        (topLeft, bottomRight) = Maybe.withDefault (point, point) <| findExtremePoints [previousPoint, point]
      in
      ( if isPointInRect target topLeft bottomRight then result ++ [ (previousPoint, point) ] else result
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
        isPointOnLine (lineFromPoints segmentStart segmentEnd) (lineWidth / zoom) target
      )
    |> List.head
  else Nothing


splitEdge : Graph.Edge -> Graph.Point -> Graph.EdgeID -> Graph.VertexID -> Graph.Graph -> Graph.Graph
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


splitEdgeSegments : Graph.Edge -> Graph.Point -> Float -> (List Graph.Point, List Graph.Point)
splitEdgeSegments edge splitPoint zoom =
  List.foldl
  ( \point ((before, after), previousPoint, found) ->
    if found || (Utils.maybeHasValue <| isPointOnLine (lineFromPoints previousPoint point) (lineWidth / zoom) splitPoint)
    then
      ((before, after ++ [ point ]), point, True)
    else
      ((before ++ [ point ], after), point, False)
  ) (([], []), edge.start.position, False) edge.points
  |> (\(result, _, _) -> result)


findExtremePoints : List Graph.Point -> Maybe (Graph.Point, Graph.Point)
findExtremePoints points =
  List.foldl
  ( \point extremes ->
    case extremes of
      Nothing -> Just (point, point)
      Just (topLeft, bottomRight) ->
        Just <| (Graph.Point (min topLeft.x point.x ) (max topLeft.y point.y), Graph.Point (max bottomRight.x point.x) (min bottomRight.y point.y) )
  ) Nothing points

isPointInRect : Graph.Point -> Graph.Point -> Graph.Point -> Bool
isPointInRect point topLeft bottomRight =
  (topLeft.x < point.x && point.x < bottomRight.x) && (bottomRight.y < point.y && point.y < topLeft.y)


isPointOnLine : Line -> Float -> Graph.Point -> Maybe Graph.Point
isPointOnLine line threshold point =
  if pointToLineDistance line point <= threshold then Just <| closestPointOnLine line point else Nothing


pointToLineDistance line point =
  (abs <| line.a * point.x + line.b * point.y + line.c) / (pointToLength <| Graph.Point line.a line.b)

normalVector : Line -> Graph.Point
normalVector line =
  Graph.Point line.a line.b

closestPointOnLine : Line -> Graph.Point -> Graph.Point
closestPointOnLine line point =
  let n = (-(line.a * point.x) - (line.b * point.y) - line.c )  / (line.a^2 + line.b^2) in
  addPoints point <| mulPoint (normalVector line) n

addPoints : { a | x : number, y : number } -> { b | x : number, y : number } -> { a | x : number, y : number }
addPoints a b =
  { a | x = (a.x + b.x), y = (a.y + b.y) }

subPoints : { a | x : number, y : number } -> { b | x : number, y : number } -> { a | x : number, y : number }
subPoints a b =
  { a | x = (a.x - b.x), y = (a.y - b.y) }

mulPoint : { a | x : number, y : number } -> number -> { a | x : number, y : number }
mulPoint point coef =
  { point | x = (point.x * coef), y = (point.y * coef) }
