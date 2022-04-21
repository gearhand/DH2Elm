module Charsheet exposing (..)
import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, br, button, div, h3, h4, hr, img, input, label, option, p, section, select, span, table, td, text, tr)
import Html.Attributes exposing (class, disabled, href, id, name, src, style, target, type_, value)
import Html.Events exposing (on, onClick)
import Maybe exposing (andThen)
import Skills exposing (Skill, skillArray)
import Stats exposing (Aptitude(..), Stat, StatName(..), aptToString)
import FieldLens exposing (FieldLens, modify)
import Json.Decode as Json
import Util exposing (applyM)

main = Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

type Msg = StatUp ModelLens
         | StatDown ModelLens
         | RmSkill String
         | Drop String
         | AddTemp Skill
         | AddSpec String Skill
         | SkillUp String
         | SkillDown String
         | Add -- Talent
         | Remove -- Talent
         | Placeholder String
         | Empty

type alias Model =
  { weaponSkill: Int
  , freeExp: Int
  , spentExp: Int
  , aptitudes: List Aptitude
  , skills: Dict String (Skill, Int)
  , temp: Maybe Skill
  , drop: String
  }

type alias SkillView = (String, Int)
viewSkill (skill, spec) =
  case spec of
    "" -> skill.name
    _  -> skill.name ++ " (" ++ spec ++ ")"

type alias ModelLens = FieldLens Model Int Int Model
weaponSkill : ModelLens
weaponSkill = FieldLens .weaponSkill (\v r -> { r | weaponSkill = v })

init: () -> (Model, Cmd Msg)
init _ =
  ( { weaponSkill = 20
    , freeExp = 600
    , spentExp = 0
    , aptitudes = [ StatApt Ag
                  , StatApt BS
                  , Fieldcraft
                  , Finesse
                  , General
                  , Knowledge
                  , StatApt Per
                  , StatApt Tou
                  ]
    , skills = Dict.fromList <| List.map (\v -> (v.name, (v, 0))) [ Skills.logic , Skills.acrobatics ]
    , temp = Nothing
    , drop = "none"
    }
  , Cmd.none)

foo: {a | weaponSkill: b} -> b
foo = .weaponSkill

getSkillUpCost: List Aptitude -> (Skill, Int) -> Maybe Int
getSkillUpCost charApts (skill, lvl) =
  Skills.getCost (Stats.aptsCounter charApts skill.aptitudes, lvl + 10)

getSkillDownCost: List Aptitude -> (Skill, Int) -> Maybe Int
getSkillDownCost charApts (skill, lvl) =
  Skills.getCost (Stats.aptsCounter charApts skill.aptitudes, lvl)

statProgression =
  Array.fromList [ [500, 750, 1000, 1500, 2500]
                 , [250, 500, 750, 1000, 1500]
                 , [100, 250, 500, 750, 1250]
                 ]

payCost: Int -> Model -> Model
payCost cost model =
  { model | spentExp = model.spentExp + cost, freeExp = model.freeExp - cost }

refund: Int -> Model -> Model
refund cost model =
  { model | spentExp = model.spentExp - cost, freeExp = model.freeExp + cost }

skillUp: Model -> String -> Model
skillUp model sName =
  let skill = Dict.get sName model.skills
      cost  = andThen <| getSkillUpCost model.aptitudes
      sAdj (s,lvl) m = { m | skills = Dict.insert sName (s, lvl + 10) m.skills }
      pay = Maybe.map payCost <| cost skill
      adj = Maybe.map sAdj skill
  in applyM pay (Just model) |> applyM adj |> Maybe.withDefault model

skillDown: Model -> String -> Model
skillDown model sName =
  let skill = Dict.get sName model.skills
      cost  = andThen <| getSkillDownCost model.aptitudes
      sAdj (s,lvl) m = { m | skills = Dict.insert sName (s, lvl - 10) m.skills }
      pay = Maybe.map refund <| cost skill
      adj = Maybe.map sAdj skill
  in applyM pay (Just model) |> applyM adj |> Maybe.withDefault model

skillAdd: Model -> String -> Skill -> Model
skillAdd model sName skill =
  let skill_ = Dict.get sName model.skills
      cost  = getSkillDownCost model.aptitudes (skill, 0) -- We need current lvl cost, so it's downCost function
      sAdj s m = { m | skills = Dict.insert sName (s, 0) m.skills, temp = Nothing }
      pay = Maybe.map payCost cost
      adj = Just (sAdj skill)
  in case skill_ of
    Just _ -> model
    Nothing -> applyM pay (Just model) |> applyM adj |> Maybe.withDefault model

skillRm: Model -> String -> Model
skillRm model sName =
  let skill = Dict.get sName model.skills
      cost  = andThen <| getSkillDownCost model.aptitudes
      sAdj m = { m | skills = Dict.remove sName m.skills }
      pay = Maybe.map refund <| cost skill
      adj = Just sAdj
  in applyM pay (Just model) |> applyM adj |> Maybe.withDefault model

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StatUp f -> (modify f (\v -> v + 5) model, Cmd.none)
    StatDown f -> (modify f (\v -> v - 5) model, Cmd.none)
    SkillUp sName -> (skillUp model sName, Cmd.none)
    SkillDown sName -> (skillDown model sName, Cmd.none)
    RmSkill sName -> (skillRm model sName, Cmd.none)
    AddTemp tmp -> ({ model | temp = Just tmp }, Cmd.none)
    AddSpec sName skill -> (skillAdd model sName skill, Cmd.none)
    Drop state -> ({ model | drop = state }, Cmd.none)
    _ -> (model, Cmd.none)

statBox: Stat -> Html Msg
statBox ((sname, sval) as stat) =
  div [ class "w3-row-padding"]
  [ div [ class "w3-half"]
    [ div
      [ name "roll_WS", type_ "roll", style "text-align" "center;"
      , value "/em rolls Weapon Skill: [[1d100]] Target: [[@{WeaponSkill}+ ?{Modifier|0}]]."
      ]
      [ label [ style "font-size" "1.25em", style "vertical-align" "middle;"]
        [text <| Stats.fullName sname]
      ]
    ]
  , div [ class "w3-third"]
    [ div [ class "sheet-unnatural_box" ]
      [ input [ disabled True, id "attr_WS", type_ "text", value <| String.fromInt sval] [ ]
      ]
    ]
  , div [ class "w3-rest" ]
    [ button [ style "height" "33px", style "width" "100%", onClick <| StatUp weaponSkill ] [ text "+" ]
    , button [ style "height" "33px", style "width" "100%", onClick <| StatDown weaponSkill ] [ text "-" ]
    ]
  ]

aptitudesView aptList =
  let apt value = div [ class "w3-half"]
                [ span [ class "sheet-item w3-center underline", id "aptitude1"] [ text <| aptToString value ] ]
      row pair = div [ class "w3-row"] pair
      combine : List Aptitude -> List (Html Msg)
      combine aptLst =
        case aptLst of
          (f::s::tail) -> row [apt f, apt s] :: combine tail
          [x] -> row [ apt x ] :: []
          [] -> []
  in combine aptList

skillView: Model -> (String, (Skill, Int)) -> Html Msg
skillView model (name, (skill, lvl)) =
  div [ class "w3-row"{-, id "defaultSkill", style "display" "none"-}]
  [ div [ class "sheet-item w3-center underline", style "width" "100%"]
    [ span [ class "skillDetail"] [ text <| name ++ " +" ++ String.fromInt lvl ]
    , button [ style "height" "100%", style "float" "right"
             , onClick <| SkillUp name
             , disabled (lvl >= 30 || Maybe.withDefault 100000 (getSkillUpCost model.aptitudes (skill, lvl)) > model.freeExp)
             ] [ text "+" ]
    , if lvl > 0
        then button [ style "height" "100%", style "float" "right"
                    , onClick <| SkillDown name
                    ] [ text "-" ]
        else button [ style "height" "100%", style "float" "right"
                    , onClick <| RmSkill name
                    ] [ text "X" ]
    ]
  ]

tempView: Skill -> Html Msg
tempView skill =
  let idxSpec f = Array.toList <| Array.indexedMap f skill.specs
      getSpec idx = Array.get idx skill.specs
      formName spec = viewSkill (skill, Maybe.withDefault "" spec)
  in
  div [ class "w3-row" ]
  [ div [ class "sheet-item w3-center underline", style "width" "100%" ]
    [ span [ class "skillDetail", style "margin-right" "5px" ] [ text <| skill.name ]
    , select [ selectHandler (\idx -> AddSpec (formName (getSpec idx)) skill) , style "width" "50%" ] <|
        idxSpec <| \idx name -> option [style "text-align" "center", value <| String.fromInt idx] [text name]
    ]
  ]

skillSelectAction: Int -> Msg
skillSelectAction idx =
  case Skills.get (idx - 1) of
    Just skill -> if Array.isEmpty skill.specs
                    then AddSpec skill.name skill
                    else AddTemp skill
    Nothing -> Empty

selectHandler: (Int -> Msg) -> Attribute Msg
selectHandler action =
  on "change" <| Json.map action <| Json.at ["target", "selectedIndex"] Json.int

skillSelect: Html Msg
skillSelect =
  let opt (idx, skill) = option [style "text-align" "center", value (String.fromInt idx)] [ text <| .name skill ]
  in select [ selectHandler skillSelectAction ] <|
      [ option [style "text-align" "center"] [ text "Learn new skill" ]
      ] ++ List.map (opt) (Array.toIndexedList skillArray)

subscriptions: Model -> Sub Msg
subscriptions _ = Sub.none


view: Model -> Html Msg
view model =
    section [ id "charSheet"
            --, style "display" "none" -- Off switch
            , style "opacity" "1"
            ]
    [ h3 [class "w3-center"] [text "Admire your Character"]
    , p [class "w3-center"]
      [ text "Wondering what any of the stats on your equipment are? Want more detailed descriptions of your talents or skills?"
      , br [] []
      , text "Check out my "
      , a [ href "https://ajott.github.io/quickref", target "_blank", style "color" "blue" ]
        [ text "Quick Reference Guide" ]
      , text " for DH 2e!"
      ]
    , hr [] []
    , div [ class "w3-row-padding"]
      [ div [ class "w3-col s0 m0 l2"] [ text "&" ]
      , div [ class "w3-col s12 m12 l8 charsheet w3-round-large"]
        [ div [ class "w3-row-padding w3-section sheet-wrapper"]
          [ div [ class "w3-quarter w3-center", style "padding-top" "25px" ]
            [ img [ src "images/charsheet_logo.png" ] [] ]
          , div [ class "w3-half"]
            [ br [ ] []
            , div [ class "sheet-row" ]
              [ div [ class "sheet-item", style "width" "10%"] []
              , div [ class "sheet-item", style "width" "30%"] [label [] [text "Home World"]]
              , div [ class "sheet-item", style "width" "55%"]
                [ span [ disabled True, class "underline", id "attr_Homeworld", type_ "text"] [] ]
              ]
            , div [ class "sheet-row"]
              [ div [ class "sheet-item", style "width" "10%"] []
              , div [ class "sheet-item", style "width" "30%"] [label [] [text "Background"]]
              , div [ class "sheet-item", style "width" "55%"]
                [ span [ disabled True, class "underline", id "attr_Background", type_ "text"] [] ]
              ]
            , div [ class "sheet-row"]
              [ div [ class "sheet-item", style "width" "10%"] []
              , div [ class "sheet-item", style "width" "30%"] [label [] [text "Role"]]
              , div [ class "sheet-item", style "width" "55%"]
                [span [ disabled True, class "underline", id "attr_Role", type_ "text"] [] ]
              ]
            , div [ class "sheet-row"]
              [ div [ class "sheet-item", style "width" "10%"] []
              , div [ class "sheet-item", style "width" "30%"] [label [] [text "Divination"]]
              , div [ class "sheet-item", style "width" "55%"] [span [ disabled True, class "underline", id "attr_Divination", type_ "text"] [] ]
              ]
            ]
          , div [ class "w3-quarter w3-center"] [
              div [ style "padding-top" "25px;" ] [
                button [ class "w3-button w3-round w3-yellow"
                       , onClick <| Placeholder "document.getElementById('saveModal').style.display 'block'"
                       ] [text "Export/Save"]
              ]
            ]
          ]
        , hr [ style "border-top" "1px solid maroon;" ] []
          -- End Tab setup --
        , div [ class "w3-row-padding w3-section"]
          [ -- Core Tab --
            div [ class "sheet-wrapper"]
            [ -- First Part   Characteristics\Experience&Fate\Insanity&Corruption\Skills --
              --%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%--
              hr [ class "sheet-dhhr"] []
            , div [ class "w3-row-padding w3-section"]
              [ div [ class "w3-half"] -- Left Side --
                [ div [ class "sheet-characteristics"]
                  [ h3 [] [text "Characteristics"]
                  , div [ class "w3-row-padding"]
                    [ -- Left Column (Characteristics) --
                      div [ class "w3-half"]
                      [ -- WeaponSkill (WS) --
                        -- div [ class "w3-row-padding"]
                        -- [ div [ class "w3-half"]
                        --   [ div [ name "roll_WS", type_ "roll", style "text-align" "center;"
                        --         , value "/em rolls Weapon Skill: [[1d100]] Target: [[@{WeaponSkill}+ ?{Modifier|0}]]."
                        --         ] [ label [ style "font-size" "1.25em", style "vertical-align" "middle;"] [text "Weapon Skill (WS)"] ]
                        --   ]
                        -- , div [ class "w3-half"] [
                        --     div [ class "sheet-unnatural_box"] [ input [ disabled True, id "attr_WS", type_ "text", value "0"] [ ] ]
                        --   ]
                        -- ]
                        statBox (Stats.WS, model.weaponSkill)
                        -- BallisticSkill (BS) --
                      , div [ class "w3-row-padding"]
                        [ div [ class "w3-half"] [
                            div [ name "roll_BS", type_ "roll", style "text-align" "center;"
                                , value "/em rolls Ballistic Skill: [[1d100]] Target: [[@{BallisticSkill}+ ?{Modifier|0}]]."
                                ] [ label [ style "font-size" "1.25em", style "vertical-align" "middle;"] [text "Ballistic Skill (BS)"] ]
                          ]
                        , div [ class "w3-half"] [
                            div [ class "sheet-unnatural_box"] [
                              input [ disabled True, id "attr_BS", type_ "text", value "0"] [ ]
                            ]
                          ]
                        ]

                        -- Strength (S) --
                      , div [ class "w3-row-padding"]
                        [ div [ class "w3-half"] [
                            div [ name "roll_S", type_ "roll", style "text-align" "center;", value "/em rolls Strength: [[1d100]] Target: [[@{Strength}+ ?{Modifier|0}]]."
                                ] [ label [ style "font-size" "1.25em", style "vertical-align" "middle"] [text "Strength (S)"]
                                  ]
                          ]
                        , div [ class "w3-half"] [
                            div [ class "sheet-unnatural_box"] [
                              input [ disabled True, id "attr_S", type_ "text", value "0"] [

                              ]
                            ]
                          ]
                        ]

                        -- Toughness (T) --
                      , div [ class "w3-row-padding"]
                        [ div [ class "w3-half"] [
                            div [ name "roll_T", type_ "roll", style "text-align" "center;"
                                , value "/em rolls Toughness: [[1d100]] Target: [[@{Toughness}+ ?{Modifier|0}]]."] [
                              label [ style "font-size" "1.25em", style "vertical-align" "middle"] [text "Toughness (T)"]]
                          ]
                        , div [ class "w3-half"] [
                            div [ class "sheet-unnatural_box"] [
                              input [ disabled True, id "attr_T", type_ "text", value "0"] [

                              ]
                            ]
                          ]
                        ]

                        -- Agility (Ag) --
                      , div [ class "w3-row-padding"]
                        [ div [ class "w3-half"] [
                            div [ name "roll_Ag", type_ "roll", style "text-align" "center;", value "/em rolls Agility: [[1d100]] Target: [[@{Agility}+ ?{Modifier|0}]]."] [
                              label [ style "font-size" "1.25em", style "vertical-align" "middle"] [text "Agility (Ag)"]]
                          ]
                        , div [ class "w3-half"] [
                            div [ class "sheet-unnatural_box"] [
                              input [ disabled True, id "attr_Ag", type_ "text", value "0"] [

                              ]
                            ]
                          ]
                        ]
                      ]

                    -- Characteristics (Right Column) --
                    , div [ class "w3-half"]
                      [ div [ class "w3-row-padding"] -- Intelligence (Int) --
                        [ div [ class "w3-half"] [
                            div [ name "roll_Int", type_ "roll", style "text-align" "center;"
                                , value "/em rolls Intelligence: [[1d100]] Target: [[@{Intelligence}+ ?{Modifier|0}]]."] [
                              label [ style "font-size" "1.25em", style "vertical-align" "middle"] [text "Intelligence (Int)"]]
                          ]
                        , div [ class "w3-half"]
                          [ div [ class "sheet-unnatural_box"] [ input [ disabled True, id "attr_Int", type_ "text", value "0"] [ ] ]
                          ]
                        ]
                      , div [ class "w3-row-padding"] -- Perception (Per) --
                        [ div [ class "w3-half"] [
                            div [ name "roll_Per", type_ "roll", style "text-align" "center;"
                                , value "/em rolls Perception: [[1d100]] Target: [[@{Perception}+ ?{Modifier|0}]]."] [
                              label [ style "font-size" "1.25em", style "vertical-align" "middle"] [text "Perception (Per)"]]
                          ]
                        , div [ class "w3-half"] [
                            div [ class "sheet-unnatural_box"] [
                              input [ disabled True, id "attr_Per", type_ "text", value "0"] [

                              ]
                            ]
                          ]
                        ]


                        -- Willpower (WP) --
                      , div [ class "w3-row-padding"]
                        [ div [ class "w3-half"] [
                            div [ name "roll_WP", type_ "roll", style "text-align" "center;"
                                , value "/em rolls Willpower: [[1d100]] Target: [[@{Willpower}+ ?{Modifier|0}]]."] [
                              label [ style "font-size" "1.25em", style "vertical-align" "middle"] [text "Willpower (WP)"]]
                          ]
                        , div [ class "w3-half"] [
                            div [ class "sheet-unnatural_box"] [
                              input [ disabled True, id "attr_WP", type_ "text", value "0"] []

                            ]
                          ]
                        ]


                        -- Fellowship (Fel) --
                      , div [ class "w3-row-padding"]
                        [ div [ class "w3-half"] [
                            div [ name "roll_Fel", type_ "roll", style "text-align" "center;",
                                 value "/em rolls Fellowship: [[1d100]] Target: [[@{Fellowship}+ ?{Modifier|0}]]."] [
                              label [ style "font-size" "1.25em", style "vertical-align" "middle"] [text "Fellowship (Fel)"]]
                          ]
                        , div [ class "w3-half"] [
                            div [ class "sheet-unnatural_box"] [
                              input [ disabled True, id "attr_Fel", type_ "text", value "0"] []

                            ]
                          ]
                        ]


                        -- Influence (Ifl) --
                      , div [ class "w3-row-padding"]
                        [ div [ class "w3-half"] [
                            div [ name "roll_Inf", type_ "roll", style "text-align" "center"
                                , value "/em rolls Influence: [[1d100]] Target: [[@{Influence}+ ?{Modifier|0}]]."] [
                              label [ style "font-size" "1.25em", style "vertical-align" "middle"] [text "Influence (Inf)"]]
                          ]
                        , div [ class "w3-half"] [
                            div [ class "sheet-unnatural_box"] [
                              input [ disabled True, id "attr_Infl", type_ "text", value "0"] []

                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                , br [] [] -- break between Characteristics and Unnaturals --
                , hr [ class "sheet-dhhr"] []
                , div [ class "w3-row-padding"]
                  [ div [ class "w3-half"] [ p [style "text-align" "center"
                                               , style "font-size" "1.25em"
                                               , style "font-weight" "600"
                                               --, style "horizontal-align" "center"
                                               --, style "margin" "0 auto"
                                               ] [text "Experience"]]
                  , div [ class "w3-half"]
                    [ let cellStyle = style "text-align" "center" in
                        table [ style "margin" "0 auto" ]
                        [ tr [] [ td [ cellStyle ] [text "Free exp."], td [ cellStyle ] [ text <| String.fromInt model.freeExp ] ]
                        , tr [] [ td [ cellStyle ] [text "Used exp."], td [ cellStyle ] [ text <| String.fromInt model.spentExp ] ]
                        ]
                    ]
                  ]
                , div [ class "w3-row-padding"]
                  [ div [ class "w3-half"]
                    [ h3 [] [text "Wounds"]
                    , div [ class "w3-row"] [
                        div [ class "w3-center"] [
                          h3 [ id "attr_Wounds_max", type_ "number", style "text-align" "center"] []
                        ]
                      ]
                    ]
                  , div [ class "w3-half"]
                    [ h3 [] [text "Fate Point Threshold"]
                    , div [ class "w3-row"] [
                        div [ class "w3-center"] [
                          h3 [ id "attr_Fate_max", type_ "number", style "text-align" "center"] []
                        ]
                      ]
                    ]
                  ]
                , br [] []
                , div [ class "w3-row-padding"]
                  [ div [ class "w3-half"]
                    [ h3 [] [text "Movement"]
                    , div [ class ""]
                      [ div [ class "w3-row"]
                        [ div [ class "w3-half"] [label [] [text "Half Move: "], span [ style "vertical-align" "middle", id "halfMove"] []]
                        , div [ class "w3-half"] [label [] [text "Full Move: "], span [ style "vertical-align" "middle", id "fullMove"] []]
                        ]
                      , div [ class "w3-row"]
                        [ div [ class "w3-row"]
                          [ div [ class "w3-half"] [label [] [text "Charge: "], span [ style "vertical-align" "middle", id "charge"] []]
                          , div [ class "w3-half"] [label [] [text "Run: "],    span [ style "vertical-align" "middle", id "run"] []]
                          ]
                        ]
                      ]
                    ]
                  , div [ class "w3-half"] [
                      div [ class "sheet-row"]
                      [ h3 [] [text "Fatigue Threshold"]
                      , div [ class "w3-center "]
                        [ h3 [ id "attr_FatigueThreshold", type_ "number", style "text-align" "center"] []
                        ]
                      ]
                    ]
                  ]
                ]

              , div [ class "w3-half"] <| -- Right Side --
                [ h3 [] [text "Skills"] -- Right Column (Skills) --
                ] ++ List.map (skillView model) (Dict.toList model.skills) ++
                (case model.temp of
                  Just tSkill -> [tempView tSkill]
                  Nothing -> [skillSelect]
                ) ++
                --[skillSelect] ++
                --[skillDropdown model] ++
                [ br [] [] -- Break between Skills and Talents
                , hr [ class "sheet-dhhr"] []
                , h3 [] [text "Talents"]
                , div [ class "w3-row", id "defaultTalent", style "display" "none"] [
                    div [ class "sheet-item w3-center underline", style "width" "100%"] [
                      span [ class "talentDetail"] []
                    ]
                  ]

                , br [] []-- Break between Talents and Aptitudes
                , hr [ class "sheet-dhhr"] []
                , h3 [] [text "Aptitudes"]
                , div [ class "w3-row", id "aptitudeContainer"] (aptitudesView model.aptitudes)
                  --[ div [ class "w3-row"]
                  --  [ div [ class "w3-half"] [
                  --      span [ class "sheet-item w3-center underline", id "aptitude0"] []
                  --    ]
                  --  , div [ class "w3-half"] [
                  --      span [ class "sheet-item w3-center underline", id "aptitude1"] []
                  --    ]
                  --  ]
                  --, div [ class "w3-row"]
                  --  [ div [ class "w3-half"] [
                  --      span [ class "sheet-item w3-center underline", id "aptitude2"] []
                  --    ]
                  --  , div [ class "w3-half"] [
                  --      span [ class "sheet-item w3-center underline", id "aptitude3"] []
                  --    ]
                  --  ]
                  --, div [ class "w3-row"]
                  --  [ div [ class "w3-half"] [
                  --      span [ class "sheet-item w3-center underline", id "aptitude4"] []
                  --    ]
                  --  , div [ class "w3-half"] [
                  --      span [ class "sheet-item w3-center underline", id "aptitude5"] []
                  --    ]
                  --  ]
                  --, div [ class "w3-row"]
                  --  [ div [ class "w3-half"] [
                  --      span [ class "sheet-item w3-center underline",  id "aptitude6"] []
                  --    ]
                  --  , div [ class "w3-half"] [
                  --      span [ class "sheet-item w3-center underline", id "aptitude7"] []
                  --    ]
                  --  ]
                  --]
                ]

              ]
            , div [ class "w3-row-padding"]
              [ hr [ class "sheet-dhhr"] []
              , h3 [] [text "Bonuses"]
              , div [ class "w3-half"]
                [ label [] [text "Homeworld Bonus:"]
                , div [ class "w3-row"] [
                    span [ id "homeBonus"] []
                  ]
                ]
              , div [ class "w3-half"]
                [ label [] [text "Background Bonus:"]
                , div [ class "w3-row"] [
                    span [ id "backBonus"] []
                  ]
                ]
              ]
            , br [] []
            , div [ class "w3-row-padding"]
              [ div [ class "w3-half"]
                [ label [] [text "Role Bonus:"]
                , div [ class "w3-row"] [
                    span [ id "roleBonus"] []
                  ]
                ]
              , div [ class "w3-half"]
                [ label [] [text "Divination:"]
                , div [ class "w3-row"] [
                    span [ id "divBonus"] []
                  ]
                ]
              ]

            , br [] []

          ]

        , hr [ style "border-top" "1px solid maroon" ] []
        , div [ class "sheet-section-gear", style "display" "block"]
          [
            -- Gear Tab --
            --%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%--
            div [ class "sheet-wrapper"] [
              div [ class "w3-row-padding w3-section"]
              [
                -- Left Side --
                div [ class "w3-half"]
                [ h3 [] [text "Ranged Weapons"]
                , div [ class "repeating_Rangedweapons"]
                  [ div [ id "defaultRangedWeapon", class "sheet-quickborder w3-round-large", style "display" "none"]
                    [ div [ class "w3-row underline"]
                      [ div [ class "w3-col l2"] [
                          label [] [text "Name: "]
                        ]
                      , div [ class "w3-col l6"] [
                          span [ class "weaponDetail weaponName "] []
                        ]
                      , div [ class "w3-col l2"] [
                          label [] [text "Class: "]
                        ]
                      , div [ class "w3-col l2"] [
                          span [ class "weaponDetail weaponClass "] []
                        ]
                      ]
                    , div [ class "w3-row underline"]
                      [ div [ class "w3-col l2"] [ label [] [text "Range: "]]
                      , div [ class "w3-col l1 "] [span [ class "weaponDetail weaponRange"] [] ]
                      , div [ class "w3-col l2"] [ label [] [text "Damage: "]]
                      , div [ class "w3-col l2 "] [span [ class "weaponDetail weaponDamage"] [] ]
                      , div [ class "w3-col l1"] [ label [] [text "Type: "]]
                      , div [ class "w3-col l2 "] [span [ class "weaponDetail weaponType"] []]
                      , div [ class "w3-col l1"] [ label [] [text "Pen: "]]
                      , div [ class "w3-col l1 "] [span [ class "weaponDetail weaponPen"] [] ]
                      ]
                    , div [ class "w3-row underline"]
                      [ div [ class "w3-col l2"] [ label [] [text "RoF: "] ]
                      , div [ class "w3-col l2"] [ span [ class "weaponDetail weaponRoF"] [] ]
                      , div [ class "w3-col l1"] [ label [] [text "Clip: "] ]
                      , div [ class "w3-col l2"] [ span [ class "weaponDetail weaponClip"] [] ]
                      , div [ class "w3-col l2"] [ label [] [text "Reload: "] ]
                      , div [ class "w3-col l2"] [ span [ class "weaponDetail weaponReload"] [] ]
                      ]
                    , div [ class "w3-row underline"]
                      [ div [ class "w3-col l2"] [ label [] [text "Special: "]]
                      , div [ class "w3-rest "] [span [ class "weaponDetail weaponSpecial"] [] ]
                      ]
                    ]
                  ]
                  -- Break between Ranged and Melee
                , h3 [] [text "Melee Weapons"]
                , table []
                  [ div [ id "charMelee"]
                    [ div [ class "repeating_meleeweapons"]
                      [ div [ id "defaultMeleeWeapon", class "sheet-quickborder w3-round-large", style "display" "none"]
                        [ div [ class "w3-row underline"]
                          [ div [ class "w3-col l2"] [
                              label [] [text "Name: "]
                            ]
                          , div [ class "w3-col l6"] [

                              span [ class "weaponDetail weaponName "] []
                            ]
                          , div [ class "w3-col l2"] [
                              label [] [text "Class: "]
                            ]
                          , div [ class "w3-col l2"] [
                              span [ class "weaponDetail weaponClass "] []
                            ]
                          ]
                        , div [ class "w3-row underline"]
                          [ div [ class "w3-col l2"] [
                              label [] [text "Damage: "]]
                          , div [ class "w3-col l2 "] [span [
                                class "weaponDetail weaponDamage"] []]
                          , div [ class "w3-col l2"] [
                              label [] [text "Type: "]]
                          , div [ class "w3-col l2 "] [span [
                                class "weaponDetail weaponType"] []]
                          , div [ class "w3-col l2"] [
                              label [] [text "Pen: "]]
                          , div [ class "w3-col l2 "] [span [ class "weaponDetail weaponPen"] [] ]
                          ]
                        , div [ class "w3-row underline"]
                          [ div [ class "w3-col l2"] [
                              label [] [text "Special: "]]
                          , div [ class "w3-rest "] [span [
                                class "weaponDetail weaponSpecial"] []
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
                -- Right Side --
              , div [ class "w3-half"]
                [ h3 [] [text "Cybernetics"]
                , div [] [
                    div [ class "repeating_Cybernetics sheet-quickborder w3-round-large", style "display" "none"] [
                      div [ class "sheet-item", style "width" "100%"] [
                        span [ id "attr_Cybernetics", type_ "text", class "underline"] []
                      ]
                    ]
                  ]
                , h3 [] [text "Gear"]
                , p [ class "w3-center"]
                  [ text "In addition to the list below, you may claim "
                  , span [ id "inflGear", style "font-weight" "bolder"] []
                  , text "extra items of Scarce (-10) availability or better from the Armoury."
                  ]
                , div [ class "sheet-quickborder w3-round-large"]
                  [ div [ class "sheet-row"]
                    [ div [ class "sheet-item", style "width" "42%"] [
                        label [] [text "Base Carry Weight S+T: "]
                      ]
                    , span [ id "attr_baseCarry"] []
                    ]
                  , div [ class "sheet-row"]
                    [ div [ class "sheet-item", style "width" "10%"] [
                        label [] [text "Max:"]
                      ]
                    , div [ class "sheet-item", style "width" "15%"] [
                        span [ disabled True, id "attr_GearCarryMax"] [] , text "kg"
                      ]
                    , div [ class "sheet-item", style "width" "17%"] [
                        label [] [text "Available: "]
                      ]
                    , div [ class "sheet-item", style "width" "15%"] [
                        span [ id "attr_GearCarryAvailable"] [], text "kg"
                      ]
                    , div [ class "sheet-item", style "width" "1%"] [
                      ]
                    , div [ class "sheet-item", style "width" "15%"] [
                        label [] [text "Current : "]
                      ]
                    , div [ class "sheet-item", style "width" "15%"] [
                        span [ id "attr_GearCarryCurrent", type_ "text"] [], text "kg"
                      ]
                    ]
                  , hr [ class "sheet-dhhr"] []
                  , div [ class "repeating_Gears"] [
                      div [ id "defaultGear", style "display" "none", class "underline"]
                      [ div [ class "sheet-item", style "width" "90%"] [
                          span [ class "gearDetail gearName"] []
                        ]
                      , div [ class "sheet-item", style "width" "10%"] [
                          span [ class "gearDetail gearWt"] [] , text "kg"
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
      , div [ class "w3-col s0 m0 l2"] [text "&"]
      ]
    ]
