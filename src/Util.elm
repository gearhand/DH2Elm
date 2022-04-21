module Util exposing (..)

import Maybe
contains: List a -> a -> Bool
contains lst el =
  case lst of
    x::xs -> if el == x then True else contains xs el
    [] -> False

applyM: Maybe (a -> b) -> Maybe a -> Maybe b
applyM mf ma =
  case mf of
    Just f -> Maybe.map f ma
    Nothing -> Nothing