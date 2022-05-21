module TalentSelectorView exposing (..)

import Html exposing (Html, text)
import Html.Attributes exposing (class, classList, style, title)
import Model exposing (Model, Talent, checkTalent, talentMenu, talentSelection)
import Selectize exposing (Msg)


view : Model -> Html (Msg Talent)
view model  =
  Selectize.view (config model)
    (talentSelection.get model)
    (talentMenu.get model)

config model =
  { container = []
  , menu = [ style "overflow" "scroll", style "background" "white", style "max-height" "20rem" ]
  , ul = []
  , entry = entry model
  , divider = (\string -> { attributes = [], children = [ text string ]})
  , input =
      Selectize.autocomplete
        { attrs =
          \isSelected isOpen ->
          [ class "selectize__textfield"
          , classList
              [ ( "selectize__textfield--selection", isSelected )
              , ( "selectize__textfield--no-selection", not isSelected )
              , ( "selectize__textfield--menu-open", isOpen )
              ]
          ]
        , toggleButton = toggleButton
        , clearButton = clearButton
        , placeholder = "Click to list available talents"
        }
  }

entry : Model -> Talent -> Bool -> Bool -> Selectize.HtmlDetails Never
entry model talent mouseFocused keyboardFocused =
  let checkResult = checkTalent talent model
   in { attributes = [ style "cursor" "pointer" ] ++
        if keyboardFocused then [ style "background-color" "#f5fafd"]
                           else if not checkResult then [ style "color" "red" ]
                           else []
        ++ [ title talent.benefit ]
      , children = [ text <| talent.name ++ if not checkResult then " — needs " ++ talent.prerequisites else ""]
      }

toggleButton : Maybe (Bool -> Html Never)
toggleButton =
    Just <|
        \open ->
            Html.div
                [ ]
                [ Html.i
                    [ class "material-icons"
                    ]
                    [ if open then
                        Html.text "arrow_drop_up"
                      else
                        Html.text "arrow_drop_down"
                    ]
                ]


clearButton : Maybe (Html Never)
clearButton =
    Just <|
        Html.div
            [ ]
            [ Html.i
                [ class "material-icons"
                ]
                [ Html.text "clear" ]
            ]
