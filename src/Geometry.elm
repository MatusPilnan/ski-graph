module Geometry exposing (..)

import Canvas.Texture
import Graph
import Model exposing (..)


constrainBackgroundToCanvas : Model -> Graph.Point -> Graph.Point
constrainBackgroundToCanvas model new =
  let
    w = Maybe.withDefault model.width <| Maybe.map (\t -> (Canvas.Texture.dimensions t).width ) model.texture
    h = Maybe.withDefault model.height <| Maybe.map (\t -> (Canvas.Texture.dimensions t).height ) model.texture
  in
  Graph.Point
    (min 0 <| max (0 - w * (Graph.getZoom model.currentGraph) + model.width) new.x )
    (min 0 <| max (0 - h * (Graph.getZoom model.currentGraph) + model.height) new.y )

canvasPointToBackgroundPoint : ViewportPoint -> ViewportPoint -> Float -> Graph.Point
canvasPointToBackgroundPoint canvasPoint backgroundPosition zoomLevel =
  mulPoint (subPoints canvasPoint backgroundPosition) <| 1 / zoomLevel

backgroundPointToCanvasPoint : Graph.Point -> ViewportPoint -> Float -> ViewportPoint
backgroundPointToCanvasPoint backgroundPoint backgroundPosition zoomLevel =
  addPoints backgroundPosition <| mulPoint backgroundPoint zoomLevel

pointSize = 10
lineWidth = 5

mouseOverPoint backgroundPosition zoomLevel mousePosition point =
  let a = backgroundPointToCanvasPoint point backgroundPosition zoomLevel in
    pointSize >= (pointToLength <| subPoints a mousePosition)

pointToLength point =
   sqrt <| point.x^2 + point.y^2



addPoints : { a | x : number, y : number } -> { b | x : number, y : number } -> { a | x : number, y : number }
addPoints a b =
  { a | x = (a.x + b.x), y = (a.y + b.y) }

subPoints : { a | x : number, y : number } -> { b | x : number, y : number } -> { a | x : number, y : number }
subPoints a b =
  { a | x = (a.x - b.x), y = (a.y - b.y) }

mulPoint : { a | x : number, y : number } -> number -> { a | x : number, y : number }
mulPoint point coef =
  { point | x = (point.x * coef), y = (point.y * coef) }
