module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)

--- main = text "hi"
main = Browser.sandbox { init = init, update = update, view = view }

update msg model =
    case msg of
        Change newContent -> { model | content = newContent }

type Msg = Change String

type alias Model = { content : String }

init = { content = ""}

view : Model -> Html Msg
view { content } =
    div []
    [ input [ placeholder "Text to reverse", value content, onInput Change ] []
    , div [] [ text (String.reverse content), text <| String.fromInt <| String.length content ]
    ]

--levelUp = Browser.element { }

type Character = Char { homeworld : String
                      , background: String
                      , role: String
                      , melee: Int
                      , ballistics: Int
                      , strength: Int
                      , toughness: Int
                      , agility: Int
                      , intelligence: Int
                      , perception: Int
                      , willpower: Int
                      , fellowship: Int
                      , influence: Int
                      , wounds: Int
                      , maxFate: Int
                      , aptitudes: List String
                      , talents: List String
                      , skills : List String
                      , homeBonus: String
                      , backBonus: String
                      , roleBonus: String
                      , divination: String
                      , currentExp: Int
                      }