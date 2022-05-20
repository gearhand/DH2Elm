module Util exposing (..)

import FieldLens exposing (FieldLens)
import Maybe
import Selectize
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

type alias Selector s =
  { selection : Maybe s
  , menu : Selectize.State s
  }

type SelectorMsg a
    = MenuMsg (Selectize.Msg a)
    | SelectTree (Maybe a)

type alias SelectorLens m s = FieldLens m (Selector s) (Selector s) m
menuLens: FieldLens (Selector s) (Selectize.State s) (Selectize.State s) (Selector s)
menuLens = FieldLens .menu (\val rec -> { rec | menu = val })

selectionLens: FieldLens (Selector s) (Maybe s) (Maybe s) (Selector s)
selectionLens = FieldLens .selection (\val rec -> { rec | selection = val })
