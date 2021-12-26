module Utils exposing (..)



maybeHasValue : Maybe a -> Bool
maybeHasValue m =
  case m of
    Nothing -> False
    Just _ -> True
