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

selectorUpdate : SelectorLens m s -> (s -> m -> m) -> SelectorMsg s -> m -> ( m, Cmd (SelectorMsg s))
selectorUpdate lens updater msg model =
    case msg of
        MenuMsg selectizeMsg ->
            let
                ( newMenu, menuCmd, maybeMsg ) =
                    Selectize.update SelectTree
                        (lens.get model).selection
                        (lens.get model).menu
                        selectizeMsg

                newModel =
                  (FieldLens.compose lens menuLens).set newMenu model

                cmd =
                    menuCmd |> Cmd.map MenuMsg
            in
            case maybeMsg of
                Just nextMsg ->
                    selectorUpdate lens updater nextMsg newModel
                        |> \( model_, cmds ) -> ( model_ , Cmd.batch [ cmd, cmds ] )

                Nothing ->
                    ( newModel, cmd )

        SelectTree newSelection ->
          ( {-(FieldLens.compose lens selectionLens).set newSelection-} model
            |> case newSelection of
                    Just s -> updater s
                    Nothing -> identity
          , Cmd.none )

selectorView conf sel = Selectize.view conf sel.selection sel.menu
