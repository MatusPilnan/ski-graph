module Messages exposing (..)

import Canvas.Texture
import Dict exposing (Dict)
import Graph
import Menus.Menus as Menus
import Model exposing (MouseEvent)
import Time

type Msg
  = Noop
  | TextureLoaded (Maybe Canvas.Texture.Texture)
  | MouseDown MouseEvent
  | MouseUp MouseEvent
  | MouseMove MouseEvent
  | MouseLeave
  | SetMapFieldVisible Bool
  | TrySettingBackground String
  | DimensionsChanged (Float, Float)
  | ZoomChanged Float
  | AnimationFrame Time.Posix
  | SetActiveEdgeType Graph.EdgeType
  | DownloadCurrentGraph
  | CreateNewGraph (Maybe Graph.GraphID)
  | SetCurrentGraph Graph.Graph
  | LoadExistingGraph
  | LoadGraphIndex (Maybe (Result String (Dict Graph.GraphID Graph.GraphIndexEntry)))
  | SelectGraphFromIndex (Maybe Graph.GraphIndexEntry)
  | UpdateMenu Menus.MenuMsg
  | UpdateEdgeTitle Graph.Edge String


menuMsgToMsg : Menus.MenuMsg -> Msg
menuMsgToMsg menuMsg =
  case menuMsg of
    Menus.SetEdgeTitle edge title -> UpdateEdgeTitle edge title
    _ -> UpdateMenu menuMsg
