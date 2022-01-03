module Messages exposing (..)

import Canvas.Texture
import Dict exposing (Dict)
import Graph
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
  | LeaveGraph
  | LoadExistingGraph
  | LoadGraphIndex (Maybe (Result String (Dict Graph.GraphID Graph.GraphIndexEntry)))
  | SelectGraphFromIndex (Maybe Graph.GraphIndexEntry)
  | SetMenuShown Bool
  | SetBackgroundOpacity Float
