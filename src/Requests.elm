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
    , expect = Http.expectJson graphIndexHandler (Saves.graphIndexDecoder Graph.Remote)
    }

graphIndexHandler : Result Http.Error (Dict Graph.GraphID Graph.GraphIndexEntry) -> Msg
graphIndexHandler response =
  case response of
    Ok index ->
      LoadGraphIndex <| Just <| Result.Ok index

    Err _ ->
      LoadGraphIndex <| Just <| Result.Err "Failed to load remote graph index."


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


