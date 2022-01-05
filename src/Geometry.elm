module Geometry exposing (..)

type alias ViewportPoint = { x : Float, y : Float}
type alias Point = { x : Float, y : Float}


type alias Line =
  { a : Float
  , b : Float
  , c : Float
  }

lineFromPoints : Point -> Point -> Line
lineFromPoints one other =
  let
    directionVector = subPoints one other
    a = (-directionVector.y)
    b = directionVector.x
    c = -((a * one.x) + (b * one.y))
  in
  Line a b c


canvasPointToBackgroundPoint : ViewportPoint -> ViewportPoint -> Float -> Point
canvasPointToBackgroundPoint canvasPoint backgroundPosition zoomLevel =
  mulPoint (subPoints canvasPoint backgroundPosition) <| 1 / zoomLevel

backgroundPointToCanvasPoint : Point -> ViewportPoint -> Float -> ViewportPoint
backgroundPointToCanvasPoint backgroundPoint backgroundPosition zoomLevel =
  addPoints backgroundPosition <| mulPoint backgroundPoint zoomLevel

liftStationPointSize = 10
skiRunConnectionPointSize = 8
lineWidth = 5

mouseOverPoint : ViewportPoint -> Float -> ViewportPoint -> Point -> Bool
mouseOverPoint backgroundPosition zoomLevel mousePosition point =
  let a = backgroundPointToCanvasPoint point backgroundPosition zoomLevel in
    liftStationPointSize >= (pointToLength <| subPoints a mousePosition)

pointToLength point =
   sqrt <| point.x^2 + point.y^2


findExtremePoints : List Point -> Maybe (Point, Point)
findExtremePoints points =
  List.foldl
  ( \point extremes ->
    case extremes of
      Nothing -> Just (point, point)
      Just (topLeft, bottomRight) ->
        Just <| (Point (min topLeft.x point.x ) (max topLeft.y point.y), Point (max bottomRight.x point.x) (min bottomRight.y point.y) )
  ) Nothing points

isPointInRect : Point -> Point -> Point -> Bool
isPointInRect point topLeft bottomRight =
  (topLeft.x < point.x && point.x < bottomRight.x) && (bottomRight.y < point.y && point.y < topLeft.y)


isPointOnLine : Line -> Float -> Point -> Maybe Point
isPointOnLine line threshold point =
  if pointToLineDistance line point <= threshold then Just <| closestPointOnLine line point else Nothing


pointToLineDistance line point =
  (abs <| line.a * point.x + line.b * point.y + line.c) / (pointToLength <| Point line.a line.b)

normalVector : Line -> Point
normalVector line =
  Point line.a line.b

closestPointOnLine : Line -> Point -> Point
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
