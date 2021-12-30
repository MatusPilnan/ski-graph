module Requests exposing
  ( fetchGraphIndex
  , fetchGraph
  )

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Graph
import Messages exposing (Msg(..))
import Saves


fetchGraphIndex : String -> Cmd Msg
fetchGraphIndex baseUrl =
  Http.get
    { url = baseUrl ++ "/graphs/index.json"
    , expect = Http.expectJson graphIndexHandler (graphIndexDecoder Graph.Remote)
    }

graphIndexHandler : Result Http.Error (Dict Graph.GraphID Graph.GraphIndexEntry) -> Msg
graphIndexHandler response =
  case response of
    Ok index ->
      LoadGraphIndex <| Just <| Result.Ok index

    Err _ ->
      LoadGraphIndex <| Just <| Result.Err "Failed to load remote graph index."



graphIndexDecoder : Graph.GraphLocation -> D.Decoder (Dict Graph.GraphID Graph.GraphIndexEntry)
graphIndexDecoder location =
  D.map Dict.fromList <|
  D.list <|
  D.map3 (\title path id -> (id, Graph.GraphIndexEntry title path id location))
    (D.field "title" D.string)
    (D.field "path" D.string)
    (D.field "id" D.string)

fetchGraph : String -> String -> Cmd Msg
fetchGraph baseUrl path =
  Http.get
    { url = baseUrl ++ path
    , expect = Http.expectJson handleGraph Saves.graphDecoder
    }


handleGraph : Result Http.Error Graph.Graph -> Msg
handleGraph result =
  case result of
    Ok graph ->
      SetCurrentGraph graph
    Err _ -> Noop


