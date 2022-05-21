module Charsheet exposing (..)
import Array exposing (Array)
import Browser
import Controller exposing (Msg(..))
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, br, button, div, h3, hr, img, input, label, option, p, s, section, select, span, table, td, text, tr)
import Html.Attributes exposing (class, disabled, href, id, name, src, style, target, type_, value)
import Html.Events exposing (on, onClick)
import Html.Events.Extra exposing (targetValueInt)
import Maybe exposing (withDefault)
import Model exposing (Model, ModelLens, Talent, agility, ballisticSkill, fellowship, intelligence, perception, strength, toughness, weaponSkill, willpower)
import Selectize
import Set
import Skills exposing (Skill)
import Stats exposing (Aptitude(..), StatName(..), aptMap, aptToString)
import FieldLens exposing (FieldLens)
import Json.Decode as Json
import TalentSelectorView
import Util exposing (SelectorMsg(..))

main = Browser.element
  { init = init
  , view = view
  , update = Controller.update
  , subscriptions = subscriptions
  }

subscriptions: Model -> Sub Msg
subscriptions _ = Sub.none




type alias SkillView = (String, Int)
viewSkill (skill, spec) =
  case spec of
    "" -> skill.name
    _  -> skill.name ++ " (" ++ spec ++ ")"


init: () -> (Model, Cmd Msg)
init _ =
  ( { weaponSkill = (0, 20, aptMap WS)
    , ballisticSkill = (0, 20, aptMap BS)
    , strength = (0, 20, aptMap Str)
    , toughness = (0, 20, aptMap Tou)
    , agility = (0, 20, aptMap Ag)
    , intelligence = (0, 20, aptMap Int)
    , perception = (0, 20, aptMap Per)
    , willpower = (0, 20, aptMap Will)
    , fellowship = (0, 20, aptMap Fell)
    , influence = 20
    , freeExp = 6000
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
    , talentSelector =
      { selection = Nothing
      , menu = Selectize.closed "talentSelector" .name
               <| List.map Selectize.entry <| Dict.values Model.talentList
      }
    , talents = Dict.empty
    , traits = Set.empty
    , implants = Set.empty
    , psyRating = 0
    , madness = 0
    , corruption = 0
    }
  , Cmd.none)


statBox: StatName -> Model -> ModelLens -> Html Msg
statBox sName model lens =
  let (sVal, sBase, sApts) = FieldLens.get lens model
      aCnt = Stats.aptsCounter model.aptitudes sApts
      upCost = Stats.getCost aCnt sVal
  in
  div [ class "w3-row-padding"]
  [ div [ class "w3-half"]
    [ div []
      [ label [ style "font-size" "1.25em", style "vertical-align" "middle;"]
        [text <| Stats.fullName sName]
      ]
    ]
  , div [ class "w3-third"]
    [ div [ class "sheet-unnatural_box" ]
      [ input [ disabled True, id "attr_WS", type_ "text", value <| String.fromInt (sBase + 5*sVal)] [ ]
      ]
    ]
  , div [ class "w3-rest" ]
    [ button [ style "height" "33px", style "width" "100%"
             , onClick <| StatUp lens
             , disabled <| sVal == 5 || (withDefault True <| Maybe.map ((<) model.freeExp) upCost)  ] [ text "+" ]
    , button [ style "height" "33px", style "width" "100%", onClick <| StatDown lens, disabled (sVal == 0) ] [ text "-" ]
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
             , disabled (lvl >= 30 || Maybe.withDefault 100000 (Skills.getUpCost model.aptitudes (skill, lvl)) > model.freeExp)
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

skillSelectAction: Array Skill -> Int -> Msg
skillSelectAction skills idx =
  case Array.get (idx - 1) skills of
    Just skill -> if Array.isEmpty skill.specs
                    then AddSpec skill.name skill
                    else AddTemp skill
    Nothing -> Empty

selectHandler: (Int -> Msg) -> Attribute Msg
selectHandler action =
  on "change" <| Json.map action <| Json.at ["target", "selectedIndex"] Json.int

skillSelect: Dict String (Skill, Int) -> Html Msg
skillSelect skills =
  let opt (idx, skill) = option [style "text-align" "center", value (String.fromInt idx)] [ text <| .name skill ]
      filtered = Skills.filtered skills
  in select [ selectHandler <| skillSelectAction filtered ] <|
      [ option [style "text-align" "center"] [ text "Learn new skill" ]
      ] ++ List.map (opt) (Array.toIndexedList filtered)

freeExpView model =
  input [ style "text-align" "center", style "width" "100px", style "margin-left" "5px"
        , type_ "number", value <| String.fromInt model.freeExp
        , on "input" <| Json.map UpdateExp <| targetValueInt
        ]
  [ text <| String.fromInt model.freeExp]

talentEntry : Talent -> Bool -> Bool -> Selectize.HtmlDetails Never
talentEntry talent mouseFocused keyboardFocused =
  { attributes = [ style "cursor" "pointer" ]
    ++ if keyboardFocused then [ style "background-color" "#f5fafd"] else []
    --++ [ disabled <| Dict.member talent.name ]
  , children = [ text talent.name ]
  }

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
                      [ statBox Stats.WS model weaponSkill -- WeaponSkill (WS) --
                      , statBox Stats.BS model ballisticSkill -- BallisticSkill (BS) --
                      , statBox Stats.Str model strength -- Strength (S) --
                      , statBox Stats.Tou model toughness -- Toughness (T) --
                      , statBox Stats.Ag model agility -- Agility (Ag) --
                      ]

                    -- Characteristics (Right Column) --
                    , div [ class "w3-half"]
                      [ statBox Stats.Int model intelligence -- Intelligence (Int) --
                      , statBox Stats.Per model perception -- Perception (Per) --
                      , statBox Stats.Will model willpower -- Willpower (WP) --
                      , statBox Stats.Fell model fellowship -- Fellowship (Fel) --

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
                        [ tr [] [ td [ cellStyle ] [text "Free exp."], td [ cellStyle ] [ freeExpView model ] ]
                        , tr [] [ td [ cellStyle ] [text "Used exp."], td [ cellStyle ] [ text <| String.fromInt model.spentExp ] ]
                        ]
                    ]
                  ]
                , let template_ name_ id_ = div [ class "w3-half"]
                                              [ h3 [] [text name_]
                                              , div [ class "w3-row"] [
                                                  div [ class "w3-center"] [
                                                    h3 [ id id_, type_ "number", style "text-align" "center"] []
                                                  ]
                                                ]
                                              ]
                  in div [ class "w3-row-padding"]
                  [ template_ "Wounds" "attr_Wounds_max"
                  , template_ "Fate Point Threshold" "attr_Fate_max"
                  ]
                , br [] []
                , div [ class "w3-row-padding"]
                  [ div [ class "w3-half"]
                    [ h3 [] [text "Movement"]
                    , let movement_ text_ id_ = div [ class "w3-half"] [label [] [text text_], span [ style "vertical-align" "middle", id id_] [] ]
                      in div [ class "w3-row"]
                      [ div [ class "w3-row"]
                        [ movement_ "Half Move: " "halfMove"
                        , movement_ "Full Move: " "fullMove"
                        ]
                      , div [ class "w3-row"]
                        [ movement_ "Charge: " "charge"
                        , movement_ "Run: " "run"
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
                  Nothing -> [skillSelect model.skills]
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
                , div [ class "w3-row", id "talent0" ]
                  [ div [ class "sheet-item w3-center underline", style "width" "100%" ]
                    [ span [ class "talentDetail" ] [ text "Weapon Training (Chain)" ]]
                  ]
                ] ++ let template_ talent_ =
                           div [ class "w3-row", id "talent0" ]
                           [ div [ class "sheet-item w3-center underline", style "width" "100%" ]
                             [ span [ class "talentDetail" ] [ text talent_.name ]
                             , button [ style "height" "100%", style "float" "right"
                                      , onClick <| TalentRemove talent_
                                      ] [ text "X" ]
                             ]
                           ]
                      in List.map template_ (Dict.values model.talents)
                ++
                [ div [ class "w3-row", id "addTalent" ]
                  [ TalentSelectorView.view model |> Html.map (TalentSelect << MenuMsg)
                  --, button []
                  ]
                , br [] []-- Break between Talents and Aptitudes
                , hr [ class "sheet-dhhr"] []
                , h3 [] [text "Aptitudes"]
                , div [ class "w3-row", id "aptitudeContainer"] <| aptitudesView model.aptitudes
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
