module Talents exposing (..)


import Browser
import Html exposing (Html, datalist, div, input, option, span, text)
import Html.Attributes exposing (class, classList, id, list, style, value)
import Html.Events exposing (on, targetValue)
import Json.Decode as Json
import Selectize
import Stats exposing (Aptitude(..), StatName(..))

type alias Talent = { name: String
                    , prerequisites: String
                    , aptitudes: List Aptitude
                    , tier: Int
                    , benefit: String
                    , page: String
                    }


main = Browser.element
  { init = init1
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }

init1: () -> (Model, Cmd Msg)
init1 _ = (init, Cmd.none)
--view =
--  div [ class "w3-row", id "talent0" ]
--  [ div [ class "sheet-item w3-center underline", style "width" "100%" ]
--    [ span [ class "talentDetail" ] [ text "Weapon Training (Chain)" ]]
--  ]

addView action =
  div [ class "w3-row", id "addTalent" ]
  [ input [ list "addTalentList"
          , on "change" <| Json.map action <| targetValue
          ] []
  , datalist [ id "addTalentList" ]
    [ option [ value "Weapon Training (Las)"] [ text "Выучка с оружием (Лаз)" ]
    , option [ value "Weapon Training (Stub)"] [ text "Выучка с оружием (Стаб)" ]
    ]
  ]

type alias Model =
  { selection : Maybe Talent
  , menu : Selectize.State Talent
  }

type Msg
    = MenuMsg (Selectize.Msg Talent)
    | SelectTree (Maybe Talent)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Update: " msg of
        MenuMsg selectizeMsg ->
            let
                ( newMenu, menuCmd, maybeMsg ) =
                    Selectize.update SelectTree
                        model.selection
                        model.menu
                        selectizeMsg

                newModel =
                    { model | menu = newMenu }

                cmd =
                    menuCmd |> Cmd.map MenuMsg
            in
            case maybeMsg of
                Just nextMsg ->
                    update nextMsg newModel
                      |> andDo cmd

                Nothing ->
                    ( newModel, cmd )

        SelectTree newSelection ->
            ( { model | selection = newSelection }, Cmd.none )

andDo : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
andDo cmd ( model, cmds ) =
    ( model
    , Cmd.batch [ cmd, cmds ]
    )

init: Model
init =
  { selection = Nothing
  , menu = Selectize.closed "talentSelector" .name <| List.map Selectize.entry talentList
  }

view : Model -> Html Msg
view model =
  div [ class "w3-row", id "addTalent" ]
    [ Selectize.view config
        model.selection
        model.menu
        |> Html.map MenuMsg
    ]

config =
  { container = []
  , menu = []
  , ul = []
  , entry = entry
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

entry : Talent -> Bool -> Bool -> Selectize.HtmlDetails Never
entry talent mouseFocused keyboardFocused =
  { attributes = [ style "cursor" "pointer" ] ++
    if keyboardFocused then [ style "background-color" "#f5fafd"] else []
  , children = [ text talent.name ]
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

talentList = [t1, t2, t4]
t1 = Talent "Ambidextrous" "Ag 30" [StatApt WS, StatApt BS] 1 "When combined with Two-Weapon Wielder, the penalty for attacks with both weapons in the same turn drops to -10." "PG 123 CB"
t2 = Talent "Blind Fighting" "Per 30" [StatApt Per, Fieldcraft] 1 "Ignores all penalties for fighting with a melee weapon while suffering from obscured vision" "PG 123 CB"
--t3 = Talent "Mounted Warrior" "Rank 2 (Trained) in any Operate skill or Rank 2(Trained) in Survival skill, BS 30 or WS 30" ["Weapon Skill / Ballistic Skill", "Offence"] "T1" "Reduces any penalty for making attacks (Melee or Ranged) from a moving vehicle      or mount      by 10 for each advance." "PG 58 EI"
t4 = Talent "Bodyguard" "Ag 35" [StatApt Ag, Defence] 1 "After an enemy makes a successful attack against an ally, the character may use      a Reaction      to move up to his Half Move distance in order to interpose himself between the      attacker      and target. The attack is then resolved against the character instead of the      original      target. In the case of a melee attack, the character may also attempt to Parry      the      attack as part of his Reaction." "PG 60 EB"
--Talent "Catfall" "Ag 30" ["Agility", "Fieldcraft"] "T1" "Reduces the effective distance of all falls by a number of metres equal to his      Agility      bonus. Also adds +20 to his Acrobatics skill tests when using Jump" "PG 124 CB"
--Talent "Clues from the Crowds" "Fel 30" ["General", "Social "] "T1" "Once per day, he can re-roll a test made to gather information from a group of      people." "PG 124 CB"
--Talent "Die Hard" "WP 40" ["Willpower", "Defence "] "T1" "Test Willpower to avoid Fatigue from Blood Loss with a Challenging (+0)      Willpower test." "PG 125 CB"
--Talent "Disarm " "Ag 30" ["Weapon Skill", "Defence"] "T1" "As a Full Action, may make an Opposed Weapon Skill test and force opponent to      drop weapon.      If 3 or more DoS, can steal weapon." "PG 125 CB"
--Talent "Double Team" "-" ["General", "Offence "] "T1" "Gain additional +10 for outnumbering opponent." "PG 126 CB"
--Talent "Enemy (choose)" "-" ["General", "Social"] "T1" "Suffers a -10 times X penalty to Fellowship and Influence tests when dealing      with group." "PG 126 CB"
--Talent "Ferric Summons" "Ferric Lure Implants, Mechanicus Implants" ["Willpower", "Tech"] "T1" "Can summon larger metallic objects up to 2 kilograms per point of his Willpower      bonus,      and can summon such objects up to 40 metres." "PG 127 CB"
--Talent "Flagellant" "Willpower 30" ["Offense", "Toughness"] "T1" "As a Full Action, the character      can take 1d5–2 levels of Fatigue (minimum 1) to gain a +10      bonus on Willpower tests to resist Fear, Pinning, psychic powers,      or Corruption, for one hour or until the end of the      current encounter. If the Acolyte also possesses the Frenzy talent,      he can enter a Frenzied state as a Free Action while under the      efects of this talent." "PG 127 CB"
--Talent "Frenzy" "-" ["Strength", "Offence"] "T1" "May spend one full round to enter Frenzy gaining +10 bonus to Weapon Skill,      Strength,      Toughness, and Willpower, but suffering a -20 penalty to Ballistic Skill,      Intelligence,      and Fellowship and is immune to Fear, Pinning, Stunning effects, and the effects      of Fatigue; he cannot Parry, retreat, or flee. He remains Frenzied for the      duration      of the combat, and cannot use psychic powers while Frenzied. After combat ends      can      make a Willpower test to snap out and cannot Frenzy again for at least an hour.    " "PG 127 CB"
--Talent "Grenadier" "BS 35" ["Ballistic Skill", "Finesse"] "T1" "When the character misses with a thrown weapon or weapon with the Blast quality,      he may      reduce the distance it scatters by a number of metres up to half his BS bonus    " "PG 62 EO"
--Talent "Iron Jaw" "T 40" ["Toughness", "Defence"] "T1" "Test Challenging (+0) Toughness Test to overcome Stunning." "PG 128 CB"
--Talent "Jaded" "WP 40" ["Willpower", "Defence"] "T1" "Ignore mundane horrors - dead bodies, xenos abominations, etc. do not cause the      Acolyte      to gain Insanity points, nor do they require a fear rolls. Daemons,      manifestations      of the Warp, and other unnatural or supernatural horrors still inflict their      effects.    " "PG 128 CB"
--Talent "Keen Intuition" "Int 35" ["Perception", "Social"] "T1" "Can retry Awareness test once with -10 modifier." "PG 129 CB"
--Talent "Leap Up" "Ag 30" ["Agility", "General"] "T1" "Stand as a Free Action." "PG 129 CB"
--Talent "Leaping Dodge" "Ag 35, Rank 2 in the Dodge skill" ["Agility", "Defence"] "T1" "When he would make an Agility test to avoid attacks from weapons with the Spray      quality,      he may make the test using his Dodge (Ag) skill instead." "PG 63 EO"
