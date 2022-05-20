module Talents exposing (..)


import Browser
import Dict exposing (Dict)
import FieldLens
import Html exposing (Html, datalist, div, input, option, span, text)
import Html.Attributes exposing (class, classList, id, list, style)
import Html.Events exposing (on, targetValue)
import Json.Decode as Json
import Selectize exposing (Msg)
import Skills exposing (Skill)
import Stats exposing (Aptitude(..), StatName(..))
import Util exposing (Selector, applyM)

type alias Talent = { name: String
                    , prerequisites: String
                    , aptitudes: List Aptitude
                    , tier: Int
                    , benefit: String
                    , page: String
                    }


--main = Browser.element
--  { init = init1
--  , view = view
--  , update = update
--  , subscriptions = \_ -> Sub.none
--  }

--type alias Selector =
--  { selection : Maybe Talent
--  , menu : Selectize.State Talent
--  , talentList: List Talent
--  }

--type Msg
--    = MenuMsg (Selectize.Msg Talent)
--    | SelectTree (Maybe Talent)

--update : Msg -> Model -> ( Model, Cmd Msg )
--update msg model =
--    case Debug.log "Update: " msg of
--        MenuMsg selectizeMsg ->
--            let
--                ( newMenu, menuCmd, maybeMsg ) =
--                    Selectize.update SelectTree
--                        model.talentSelector.selection
--                        model.talentSelector.menu
--                        selectizeMsg
--
--                newModel =
--                  FieldLens.set Model.talentMenu newMenu model
--
--                cmd =
--                    menuCmd |> Cmd.map MenuMsg
--            in
--            case maybeMsg of
--                Just nextMsg ->
--                    update nextMsg newModel
--                      |> andDo cmd
--
--                Nothing ->
--                    ( newModel, cmd )
--
--        SelectTree newSelection ->
--            ( { model | talents =
--                  case newSelection of
--                    Just talent_ -> talent_.name :: model.talents
--                    _ -> model.talents
--              } |> FieldLens.set Model.talentSelection newSelection
--            , Cmd.none )

--andDo : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
--andDo cmd ( model, cmds ) =
--    ( model
--    , Cmd.batch [ cmd, cmds ]
--    )

init: Selector Talent
init =
  { selection = Nothing
  , menu = Selectize.closed "talentSelector" .name <| List.map Selectize.entry talentList
  }

view : Selector Talent -> Html (Msg Talent)
view model =
  Selectize.view config
    model.selection
    model.menu

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

talentList1 =
  Dict.fromList <| List.map (\t -> (t.name, t))
  [ Talent "Ambidextrous"
           "Ag 30"
           [StatApt WS, StatApt BS]
           1
           "When combined with Two-Weapon Wielder, the penalty for attacks with both weapons      in the      same turn drops to -10."
           "PG 123 CB"
  , Talent "Blind Fighting"
           "Per 30"
           [StatApt Per, Fieldcraft]
           1
           "Ignores all penalties for fighting with a melee weapon while suffering from      obscured      vision    "
           "PG 123 CB"
  , Talent "Bodyguard"
           "Ag 35"
           [StatApt Ag, Defence]
           1
           "After an enemy makes a successful attack against an ally, the character may use      a Reaction      to move up to his Half Move distance in order to interpose himself between the      attacker      and target. The attack is then resolved against the character instead of the      original      target. In the case of a melee attack, the character may also attempt to Parry      the      attack as part of his Reaction."
           "PG 60 EB"
  , Talent "Catfall"
           "Ag 30"
           [StatApt Ag, Fieldcraft]
           1
           "Reduces the effective distance of all falls by a number of metres equal to his      StatApt Ag      bonus. Also adds +20 to his Acrobatics skill tests when using Jump"
           "PG 124 CB"
  , Talent "Clues from the Crowds"
           "Fel 30"
           [General, Social ]
           1
           "Once per day, he can re-roll a test made to gather information from a group of      people."
           "PG 124 CB"
  , Talent "Die Hard"
           "WP 40"
           [StatApt Will, Defence ]
           1
           "Test StatApt Will to avoid Fatigue from Blood Loss with a Challenging (+0)      Willpower test."
           "PG 125 CB"
  , Talent "Disarm "
           "Ag 30"
           [StatApt WS, Defence]
           1
           "As a Full Action, may make an Opposed StatApt WS test and force opponent to      drop weapon.      If 3 or more DoS, can steal weapon."
           "PG 125 CB"
  , Talent "Double Team"
           "-"
           [General, Offence ]
           1
           "Gain additional +10 for outnumbering opponent."
           "PG 126 CB"
  , Talent "Enemy (choose)"
           "-"
           [General, Social]
           1
           "Suffers a -10 times X penalty to StatApt Fell and Influence tests when dealing      with group."
           "PG 126 CB"
  , Talent "Ferric Summons"
           "Ferric Lure Implants, Mechanicus Implants"
           [StatApt Will, Tech]
           1
           "Can summon larger metallic objects up to 2 kilograms per point of his StatApt Will      bonus,      and can summon such objects up to 40 metres."
           "PG 127 CB"
  , Talent "Flagellant"
           "StatApt Will 30"
           [Offence, StatApt Tou]
           1
           "As a Full Action, the character      can take 1d5–2 levels of Fatigue (minimum 1) to gain a +10      bonus on StatApt Will tests to resist Fear, Pinning, psychic powers,      or Corruption, for one hour or until the end of the      current encounter. If the Acolyte also possesses the Frenzy talent,      he can enter a Frenzied state as a Free Action while under the      efects of this talent."
           "PG 127 CB"
  , Talent "Frenzy"
           "-"
           [StatApt Str, Offence]
           1
           "May spend one full round to enter Frenzy gaining +10 bonus to StatApt WS,      StatApt Str,      StatApt Tou, and StatApt Will, but suffering a -20 penalty to StatApt BS,      StatApt Int,      and StatApt Fell and is immune to Fear, Pinning, Stunning effects, and the effects      of Fatigue; he cannot Parry, retreat, or flee. He remains Frenzied for the      duration      of the combat, and cannot use psychic powers while Frenzied. After combat ends      can      make a Willpower test to snap out and cannot Frenzy again for at least an hour.    "
           "PG 127 CB"
  , Talent "Grenadier"
           "BS 35"
           [StatApt BS, Finesse]
           1
           "When the character misses with a thrown weapon or weapon with the Blast quality,      he may      reduce the distance it scatters by a number of metres up to half his BS bonus    "
           "PG 62 EO"
  , Talent "Iron Jaw"
           "T 40"
           [StatApt Tou, Defence]
           1
           "Test Challenging (+0) StatApt Tou Test to overcome Stunning."
           "PG 128 CB"
  , Talent "Jaded"
           "WP 40"
           [StatApt Will, Defence]
           1
           "Ignore mundane horrors - dead bodies, xenos abominations, etc. do not cause the      Acolyte      to gain Insanity points, nor do they require a fear rolls. Daemons,      manifestations      of the Warp, and other unnatural or supernatural horrors still inflict their      effects.    "
           "PG 128 CB"
  , Talent "Keen Intuition"
           "Int 35"
           [StatApt Per, Social]
           1
           "Can retry Awareness test once with -10 modifier."
           "PG 129 CB"
  , Talent "Leap Up"
           "Ag 30"
           [StatApt Ag, General]
           1
           "Stand as a Free Action."
           "PG 129 CB"
  , Talent "Leaping Dodge"
           "Ag 35, Rank 2 in the Dodge skill"
           [StatApt Ag, Defence]
           1
           "When he would make an StatApt Ag test to avoid attacks from weapons with the Spray      quality,      he may make the test using his Dodge (Ag) skill instead."
           "PG 63 EO"
  --, Talent "Mounted Warrior" -- TODO
  --         "Rank 2 (Trained) in any Operate skill or Rank 2(Trained) in Survival skill, BS 30 or WS 30"
  --         [StatApt WS / StatApt BS, Offence]
  --         1
  --         "Reduces any penalty for making attacks (Melee or Ranged) from a moving vehicle      or mount      by 10 for each advance."
  --         "PG 58 EI"
  , Talent "Nowhere to Hide"
           "Per 30"
           [StatApt Per, Offence]
           1
           "Can add DoS from the attack to reduce armour value of cover."
           "PG 130 CB"
  , Talent "Peer (choose)"
           "Fel 30"
           [StatApt Fell, Social]
           1
           "Character has good reputation amongst chosen group with bonus to StatApt Fell      tests increases      to +10 times X. Additionally, when the character acquires his talent, also      increases      his Influence by 1."
           "PG 130 CB"
  , Talent "Quick Draw"
           "-"
           [StatApt Ag, Finesse]
           1
           "Draw weapon as Free Action."
           "PG 131 CB"
  , Talent "Rapid Reload"
           "-"
           [StatApt Ag, Fieldcraft ]
           1
           "Reduce reload time by half time."
           "PG 131 CB"
  , Talent "Resistance (choose)"
           "-"
           [StatApt Tou, Defence]
           1
           "Gain +10 bonus to particular resistance test."
           "PG 131 CB"
  , Talent "Skilled Rider"
           "Rank 2 in any Operate skill "
           [StatApt Ag, Fieldcraft]
           1
           "Whenever the character would be thrown from or tossed about within his vehicle,      he makes      an Ordinary (+10) StatApt Ag test. If he succeeds, the character may choose to      either      land safely on his feet or retain in his original position in the vehicle. In      addition,      once per round the character can attempt an Ordinary (+10) Agility test to Mount      or Dismount a vehicle as a Free Action."
           "PG 63 EO"
  , Talent "Sound Constitution"
           "-"
           [StatApt Tou, General]
           1
           "Gain an additional wound."
           "PG 131 CB"
  , Talent "Takedown"
           "-"
           [StatApt WS, Offence]
           1
           "Make special attack to stun opponent."
           "PG 132 CB"
  , Talent "Technical Knock"
           "Int 30"
           [StatApt Int, Tech]
           1
           "Un-jam gun as Half Action."
           "PG 132 CB"
  , Talent "Warp Sense"
           "Psy Rating, Psyniscience, Per 30 "
           [StatApt Per, Psyker]
           1
           "Allows Psyniscience test as Free Action."
           "PG 133 CB"
  , Talent "Weapon Training (choose)"
           "-"
           [General, Finesse]
           1
           "Use Weapon Group without penalty."
           "PG 133 CB"
  , Talent "Weapon-Tech"
           "Tech Use +10, Int 40"
           [StatApt Int, Tech]
           1
           "May enhance any Melta, Plasma, Power, or Exotic weapon by increases the weapon's      damage      and penetration by an amount equal to the character's StatApt Int bonus until      the      end of the round once per encounter."
           "PG 133 CB"
  , Talent "Ambassador Imperialis"
           "Fellowship 35, Intelligence 35"
           [StatApt Per, Social]
           2
           "Any penalty to Interaction skill tests for dealing with xenos or non-Imperial      NPCs by      20. In addition, once per encounter, he can reroll a failed Interaction skill      test      when interacting with such an NPC. "
           "PG 62 EO"
  , Talent "Archivator"
           "StatApt Int 40"
           [Knowledge, Social]
           2
           "A Scholastic Lore or Forbidden Lore test utilising sources of recorded      information of      any sort can re-roll with -10."
           "PG 62 EO"
  , Talent "Armor-Monger"
           "Int 35, Tech-Use, Trade (Armourer)"
           [StatApt Int, Tech]
           2
           "Gains an extra amount of Armor points equal to his Intelligence bonus. Takes an      hour      each day.    "
           "PG 123 CB"
  , Talent "Battle Rage"
           "Frenzy"
           [StatApt Str, Defence]
           2
           "Can Parry while Frenzied, and can re-roll a failed test to snap out of Frenzy or      resist      entering Frenzy if they choose."
           "PG 123 CB"
  , Talent "Bulging Biceps"
           "S 45"
           [StatApt Str, Offence]
           2
           "Remove bracing requirement from Heavy weapons, and +20 to his Athletics skill      test when      using Heft.    "
           "PG 123 CB"
  , Talent "Bulwark of Faith"
           "WP 45, Iron Faith"
           [Defence, StatApt Will]
           2
           "When passing a Fear test caused by a Daemon, the Daemon suffers 1 Energy damage      ignoring      armour and StatApt Tou bonus for each degree of success on the Fear test."
           "PG 60 EB"
  , Talent "Combat Master"
           "WS 30"
           [StatApt WS, Defence]
           2
           "Opponents get no bonus for outnumbering the character."
           "PG 124 CB"
  , Talent "Constant Vigilance (choose)"
           "Int 35 or Per 35, Awareness +10"
           [StatApt Per, Defence]
           2
           "Can use Per or Int instead of Ag for Initiative rolls, and rolls two dice      (picking higher)      for the result."
           "PG 124 CB"
  , Talent "Contact Network"
           "Cover-Up, Int 35"
           [StatApt Fell, Leadership]
           2
           "Use StatApt Fell instead of Influence for Requisition tests."
           "PG 124 CB"
  , Talent "Coordinated Interrogation"
           "S 40 or WP 40, Clues from the Crowds, Rank 1 (Known) Interrogation"
           [StatApt Int, Social]
           2
           "Has a +10 bonus to all Interrogate tests, additional +5 for others who also have      Coordinated      Interrogation    "
           "PG 124 CB"
  , Talent "Counter Attack"
           "WS 40"
           [StatApt WS, Defence]
           2
           "May make a Standard Attack after successful Parry with -20 to StatApt WS."
           "PG 125 CB"
  , Talent "Cover-Up"
           "Int 35"
           [StatApt Int, Knowledge]
           2
           "Can reduce Influence by 1 to gain 1d5 Subtlety."
           "PG 125 CB"
  , Talent "Daemonhunter"
           "Forbidden Lore (Daemonology), WP 40"
           [Offence, StatApt Will]
           2
           "May re-roll failed Awareness and Psyniscience Tests to detect the presence of      Daemons.      In addition, his attacks against Daemons gain the Proven (3) quality."
           "PG 60 EB"
  , Talent "Daemonologist"
           "Psy rating 3, WP 45, Forbidden Lore (Daemonology)"
           [Psyker, StatApt Will]
           2
           "When the character takes the Focus Power action and the target of the psychic      power is      a Daemon, he gains a +10 bonus to the Focus Power test. At the GM's discretion,      this      bonus may apply to other tests, such as rituals to summon or bind a Daemon."
           "PG 60 EB"
  , Talent "Deny the Witch"
           "WP 35"
           [StatApt Will, Defence]
           2
           "Can use StatApt Will to Evade against psychic attacks."
           "PG 125 CB"
  , Talent "Devastating Assault"
           "WS 35"
           [StatApt WS, Offence]
           2
           "Successful All Out Attack grants, may make a second All Out Attack once per      turn."
           "PG 125 CB"
  , Talent "Double Tap"
           "Two-Weapon Wielder"
           [Finesse, Offence]
           2
           "A second ranged attack against the same target, grants a+20 bonus if scored 1 or      more      DoS.    "
           "PG 125 CB"
  , Talent "Exotic Weapon Training"
           "-"
           [StatApt Int, Finesse]
           2
           "Gain proficiency with one exotic weapon type."
           "PG 127 CB"
  , Talent "Face in a Crowd"
           "Fel 35, Clues from the Crowds"
           [StatApt Fell, Social]
           2
           "Can use StatApt Fell instead of StatApt Ag when Shadowing."
           "PG 127 CB"
  --, Talent "Field Vivisection" -- TODO
  --         "BS or WS 40, Forbidden Lore (Xenos-Any), Rank 2 in the Medicae skill"
  --         [StatApt BS/StatApt WS, Knowledge]
  --         2
  --         "When using the Called Shot action with a melee or ranged attack (depending on      the Specialisation)      against a target for which the character has the appropriate Forbidden Lore      (Xenos)      skill, he makes a Medicae (WS) or Medicae (BS) test in place of the normal      Weapon      Skill or StatApt BS test. "
  --         "PG 62 EO"
  , Talent "Hard Target"
           "Ag 40"
           [StatApt Ag, Defence]
           2
           "-20 to hit character when he Charges or Runs."
           "PG 128 CB"
  , Talent "Harden Soul"
           "WP 35, 10 Corruption points"
           [Defence, StatApt Will]
           2
           "Whenever the character would gain Corruption points, he may reduce the amount      gained      by half (rounded up) and gain Insanity points equal to the amount reduced."
           "PG 61 EB"
  , Talent "Hardy"
           "T 40"
           [StatApt Tou, Defence]
           2
           "Character always heals as if Lightly Damaged."
           "PG 128 CB"
  , Talent "Hatred (choose)"
           "-"
           [StatApt WS, Social]
           2
           "Gain +10 bonus to attack StatApt WS tests. Must make a Challenging (+0)      StatApt Will test      to retreat or surrender."
           "PG 128 CB"
  , Talent "Hip Shooting"
           "BS 40, Ag 40"
           [StatApt BS, Finesse]
           2
           "As a Full Action, can both move up to his Full Move rate and make a single shot      attack      with a ranged weapon per weapon."
           "PG 128 CB"
  , Talent "Hotshot Pilot"
           " Rank 2 in Survival or any Operate skill, Ag 35 "
           [StatApt Ag, Tech]
           2
           "On an succesful Operate test, he may voluntarily suffer 1 level of Fatigue to      add a number      of DoS equal to half of his StatApt Ag bonus. When failed an Operate test, he may      voluntarily      suffer 1 level of Fatigue in order to reduce the degrees of failure by an amount      equal to his Agility bonus, to a minimum of 1. "
           "PG 62 EO"
  , Talent "Independent Targeting"
           "BS 40"
           [StatApt BS, Finesse]
           2
           "Fire at multiple targets more than 10 metres apart when firing two weapons."
           "PG 128 CB"
  , Talent "Inescapable Attack (Melee)"
           "WS 40,Per 35"
           [StatApt WS, Finesse]
           2
           "Attacker imposes penalty on all evasion attempts made against this attack equal      to 10      times the total degrees of success scored on the attack test."
           "PG 128 CB"
  , Talent "Inescapable Attack (Ranged)"
           "BS 40,Per 35"
           [StatApt BS, Finesse]
           2
           "Attacker imposes penalty on all evasion attempts made against this attack equal      to 10      times the total degrees of success scored on the attack test."
           "PG 128 CB"
  , Talent "Inspiring Aura"
           "Halo of Command"
           [Leadership, StatApt Will]
           2
           "can affect allies of any kind with the Terrify special use for the Command skill      (pg      101 CB). This need not represent threats and intimidation, but might represent      inspiring      words, encouragement, or sheer steadfastness in the face of terrifying foes.    "
           "PG 61 EB"
  , Talent "Iron Resolve"
           "Resistance (Fear), Jaded"
           [Defence, StatApt Will]
           2
           "After failing a Fear or Pinning test, the character can re-roll the test with a      -10 modifier."
           "PG 61 EB"
  , Talent "Killing Strike"
           "WS 50"
           [StatApt WS, Offence]
           2
           "Spend Fate point to make melee attacks unavoidable."
           "PG 129 CB"
  , Talent "Lexographer"
           "Rank 3 in Linguistics (Any)"
           [StatApt Int, Knowledge]
           2
           "The character can attempt any Linguistics skill in which he is not trained as an      untrained      skill test as if it were not a Specialist skill. "
           "PG 63 EO"
  , Talent "Luminen Shock"
           "Luminen Capacitors, Mechanicus Implants"
           [StatApt WS, Tech]
           2
           "Counts as a melee weapon that inflicts 1d10 plus his WPB in Energy damage, with      Pen 0      and the Shocking quality. Must pass a StatApt Tou test or suffer 1 level of      Fatigue      after attack."
           "PG 129 CB"
  , Talent "Maglev Transcendence"
           "Maglev Coils, Mechanicus Implants"
           [StatApt Int, Tech]
           2
           "Can hover for a number of minutes equal to 1d10 plus twice his StatApt Tou bonus.      He can      move his Run speed when making a Half Move action and suffers no damage from      falling      if the coils are active. Each use drains half the power stored in the coils (can      use the coils twice before recharging them)."
           "PG 129 CB"
  , Talent "Marksman"
           "BS 35"
           [StatApt BS, Finesse]
           2
           "No penalties for firing at long or extreme range."
           "PG 130 CB"
  , Talent "Mechadendrite Use (choose)"
           "Mechanicus Implants"
           [StatApt Int, Tech]
           2
           "Gain ability to use certain Mechadendrites. Weapon or Utility."
           "PG 130 CB"
  , Talent "One-on-One"
           "WS 40"
           [Finesse, StatApt WS]
           2
           "When fighting a single enemy in melee combat, the character scores extra degrees      of success      on successful StatApt WS tests equal to half of his Weapon Skill bonus      (rounded      down).    "
           "PG 61 EB"
  , Talent "Penitent Psyker"
           "Psy rating, Strong Minded, WP 40"
           [Psyker, Defence]
           2
           "psyker or ally within 10 metres target of a psychic power suffer any number of      levels      of Fatigue, each level grants +10 to opposed test to resist or avoid."
           "PG 58 EI"
  , Talent "Precision Killer (Ranged)"
           "BS 40"
           [StatApt BS, Finesse]
           2
           "No penalty to making Called Shot in either Ranged or Melee Combat."
           "PG 130 CB"
  , Talent "Precision Killer (Melee)"
           "WS 40"
           [StatApt WS, Finesse]
           2
           "No penalty to making Called Shot in either Ranged or Melee Combat."
           "PG 130 CB"
  , Talent "Prosanguine"
           "Auto Sanguine Implants, Mechanicus Implants"
           [StatApt Tou, Tech]
           2
           "Spend 10 minutes and make a Tech-Use test to heal 1d5 damage. Rolls 96 or      higher, loses      the ability for one week."
           "PG 131 CB"
  , Talent "Purity of Hatred"
           "Hatred (Any)"
           [Offence, StatApt Will]
           2
           "Attacks gain the Vengeful (9) quality against opponents of Hatred group."
           "PG 58 EI"
  , Talent "Rites of Banishment"
           "Common Lore (Imperial Creed) +10 or Forbidden Lore (Daemonology)"
           [Offence, StatApt Will]
           2
           "Once per round as a Half Action, the character may speak the litanies and      invocations      to disrupt Daemons. Until the beginning of his next turn, Daemons within a      distance      equal to twice the character's StatApt Will bonus in meters suffer a -10 penalty to      Willpower tests.    "
           "PG 61 EB"
  , Talent "Strong Minded"
           "WP 30, Resistance (Psychic Powers)"
           [StatApt Will, Defence]
           2
           "May reroll failed WP tests to resist psychic powers."
           "PG 131 CB"
  , Talent "Swift Attack"
           "WS 30"
           [StatApt WS, Finesse]
           2
           "May make multiple melee attacks."
           "PG 131 CB"
  , Talent "Tainted Psyker"
           "Psy rating, Rank 2 (Trained) in Psyniscience Skill, 10 Corruption points"
           [Knowledge, Psyker]
           2
           "When making a Focus Power test, the character may gain a number of Corruption      points      up to his psy rating. For each point he gains in this way, he gains a +10 bonus      but      adds +5 to rolls on Table 6-2 Psychic Phenomena"
           "PG 58 EI"
  , Talent "Two-Weapon Wielder (Ranged)"
           "-"
           [StatApt BS, Finesse]
           2
           "Attacks with two weapons count as being part of the same Half Action, and both      tests      made to attack with the weapons suffer a -20 penalty. "
           "PG 132 CB"
  , Talent "Two-Weapon Wielder (Melee)"
           "-"
           [StatApt WS, Finesse]
           2
           "Attacks with two weapons count as being part of the same Half Action, and both      tests      made to attack with the weapons suffer a -20 penalty. "
           "PG 132 CB"
  , Talent "Unarmed Specialist"
           "Ambidextrous, Ag 35, WS 35"
           [StatApt Str, Offence]
           2
           "When fighting unarmed, attacks hit as Deadly Natural Weapons and user can      re-roll damage."
           "PG 132 CB"
  , Talent "Warp Conduit"
           "Psy Rating, Strong Minded, WP 50"
           [StatApt Will, Psyker]
           2
           "Spend Fate point to add 1d5 to psy rating however he adds +30 to rolls on Table      6-2:      Psychic Phenomena    "
           "PG 132 CB"
  , Talent "Whirlwind of Death"
           "WS 40"
           [StatApt WS, Finesse]
           2
           "As a Half Action, may make one Standard Attack action with a melee weapon      against a foe,      plus one additional Standard Attack action with the same weapon targeting each      other      foe also engaged in melee combat with the character beyond the first (to a      maximum      number of attacks up to his StatApt WS bonus)."
           "PG 133 CB"
  , Talent "Witch Finder"
           "Rank 2 (Trained) in the Forbidden Lore (Psykers)skill, WP 45"
           [Knowledge, StatApt Per]
           2
           "Counts as possessing the Psyniscience skill at Rank 1 (Known), even though he is      not      a psyker."
           "PG 58 EI"
  , Talent "Xenosavant"
           "Rank 3 in Forbidden Lore (Xenos-Any) "
           [StatApt Int, Knowledge]
           2
           "The character can attempt any Forbidden Lore (Xenos) test in which he is not      trained      as an untrained skill test as if it were not a Specialist skill."
           "PG 63 EO"
  , Talent "Adamantium Faith"
           "Jaded, Resistance (Fear), WP 45"
           [StatApt Will, Defence]
           3
           "Subtract StatApt Will bonus from his degrees of failure on a failed Fear or Pinning      test.      If this reduces the result to zero or less, he counts as having passed the Fear      test      with 1 degree of success."
           "PG 123 CB"
  , Talent "Aegis of Contempt"
           "Shared Destiny, Shield of Contempt, Hatred (any)"
           [Defence, Leadership]
           3
           "Character or an ally within 10 metres gains Corruption, reduce the amount by 1      to a min      of 0.    "
           "PG 57 EI"
  , Talent "Assassin Strike"
           "Ag 40, Acrobatics"
           [StatApt WS, Fieldcraft]
           3
           "After a melee attack, a Challenging (+0) Acrobatics skill test allows to move at      half      rate as a Free Action. Only once per round, and opponent does not receive a free      attack.    "
           "PG 123 CB"
  , Talent "Bastion of Iron Will"
           "Psy Rating, Strong Minded, WP 40"
           [StatApt Will, Psyker]
           3
           "Adds 5 x his psy rating to any Opposed test when defending against psychic      powers."
           "PG 123 CB"
  , Talent "Blademaster"
           "WS 30, Weapon Training (any Melee)"
           [StatApt WS, Finesse]
           3
           "When attacking with any bladed weapon, can re-roll one missed attack per round.    "
           "PG 123 CB"
  , Talent "Crushing Blow"
           "WS 40"
           [StatApt WS, Offence]
           3
           "Add half WS bonus (Round up) to damage inflicted in melee."
           "PG 125 CB"
  , Talent "Daemonic Disruption"
           "Bane of the Daemon, WP 50, Untouchable elite advance"
           [StatApt Will, General]
           3
           "Whenever a creature with the Warp Instability trait makes a successful attack      test against      this character, it must immediately test for Warp Instability after resolving      the      attack.    "
           "PG 60 EB"
  , Talent "Dark Soul"
           "Hardened Soul, 20 Corruption points"
           [StatApt Tou, StatApt Will]
           3
           "When the character would test to gain a mutation as a result of increasing      Corruption,      he may choose to automatically pass the test. If he does, he also gains a      Malignancy      and increases his Corruption total by 1d10."
           "PG 60 EB"
  , Talent "Deathdealer (Ranged)"
           "BS 45"
           [StatApt Per, Finesse]
           3
           "Selects the Specialisation that matches the prerequisite used in purchase (Melee with Weapon Skill, Ranged with Ballistic Skill). Attack in that combat type inflicts Critical damage, add his Perception bonus to the damage result."
           "PG 125 CB"
  , Talent "Deathdealer (Melee)"
           "WS 45"
           [StatApt Per, Finesse]
           3
           "Selects the Specialisation that matches the prerequisite used in purchase (Melee with Weapon Skill, Ranged with Ballistic Skill). Attack in that combat type inflicts Critical damage, add his Perception bonus to the damage result."
           "PG 125 CB"
  , Talent "Delicate Interrogation"
           "Fel 50, Coordinated Interrogation"
           [StatApt Int, Finesse]
           3
           "Reduce Subtlety loss by 1d5 when conducting an Interrogation. If this results in      a negative      number increase the warband's Subtlety by 1."
           "PG 125 CB"
  , Talent "Divine Protection"
           "BS 45, WP 35"
           [General, Finesse]
           3
           "Attacks using Spray quality only effects enemies."
           "PG 57 EI"
  , Talent "Eye of Vengeance"
           "BS 50"
           [StatApt BS, Offence]
           3
           "Spend 1 Fate point to add the number of DoS scored on the attack test to damage      and Penetration."
           "PG 127 CB"
  , Talent "Favored by the Warp"
           "WP 35"
           [StatApt Will, Psyker]
           3
           "Roll twice for Psychic Phenomena and choose result."
           "PG 127 CB"
  , Talent "Flash of Insight"
           "Int 40, Contact Network, Coordinated Interrogation"
           [StatApt Per, Knowledge]
           3
           "Spend 1 Fate point to reveal a clue."
           "PG 127 CB"
  , Talent "Halo of Command"
           "Fel 40, WP 40"
           [StatApt Fell, Leadership]
           3
           "Affect NPCs within 100xFelB metres with Social skills."
           "PG 127 CB"
  , Talent "Hammer Blow"
           "Crushing Blow"
           [StatApt Str, Offence]
           3
           "When he uses an All Out Attack action to make a single attack, he can add half      his StatApt Str      bonus (rounded up) to the weapon's penetration. The attack also counts as having      the Concussive (2) weapon quality."
           "PG 128 CB"
  , Talent "Hull Down"
           "Rank 2 in Survival or any Operate skill"
           [StatApt Ag, Fieldcraft]
           3
           "When the character takes a vehicle combat action with the Movement subtype, his      vehicle      or steed counts the value of its Size trait as being one lower for purposes of      attack      modifiers and the benefits of cover until the start of his next turn."
           "PG 62 EO"
  , Talent "Indomitable Conviction"
           "Shared Destiny, StatApt Str through Conviction, Resistance (Fear), Jaded"
           [Leadership, StatApt Will]
           3
           "Character or an ally within 10 metres gains Insanity, reduce the amount by 1 to      a min      of 0. "
           "PG 57 EI"
  , Talent "Infused Knowledge"
           "Int 40, Lore (anyone)"
           [StatApt Int, Knowledge]
           3
           "counts as having all Common Lore and Scholastic Lore skills at rank 1 (Known).    "
           "PG 128 CB"
  , Talent "Instrument of His Will"
           "WP 50"
           [Offence, StatApt Will]
           3
           "After making a successful attack against a Daemon (this can include striking it      with      a psychic power), the character may spend a Fate point to increase the damage of      the first hit he inflicts as part of that attack by an amount equal to twice his      StatApt Will bonus. This additional damage ignores armour and StatApt Tou bonus."
           "PG 61 EB"
  , Talent "Into the Jaws of Hell"
           "Adamantium Faith, Halo of Command, Will of the Inquisitor"
           [Leadership, StatApt Will]
           3
           "Subtract the fellowship bonus from DoF for failed Fear or Pinning test."
           "PG 57 EI"
  , Talent "Iron Faith"
           "Iron Resolve"
           [Defence, StatApt Will]
           3
           "The character is immune to the effects of the Baneful Presence trait."
           "PG 61 EB"
  , Talent "Lightning Attack"
           "Swift Attack"
           [StatApt WS, Finesse]
           3
           "Character may make many melee attacks with single roll."
           "PG 129 CB"
  , Talent "Luminen Blast"
           "Luminen Shock, Luminen Capacitors, Mechanicus Implants"
           [StatApt BS, Tech]
           3
           "Counts as being equipped with a single shot Pistol weapon with a 10m range and      deals      1d10 plus twice his StatApt Will bonus in Energy damage. Must pass a StatApt Tou test      or suffer 1 level of Fatigue after attack."
           "PG 129 CB"
  , Talent "Mastery (choose)"
           "Rank 4 in selected skill"
           [StatApt Int, Knowledge]
           3
           "May spend Fate point to succeed on test if the final modifier to his skill test      is Challenging      (+0) or better, score a number of degrees of success equal to the characteristic      bonus.    "
           "PG 130 CB"
  , Talent "Mighty Shot"
           "BS 40"
           [StatApt BS, Offence]
           3
           "Add half BS (rounding up) bonus to ranged damage rolls."
           "PG 130 CB"
  , Talent "Never Die"
           "WP 50, T 50"
           [StatApt Tou, Defence]
           3
           "Ignore penalties from Critical damage by spending Fate point. Once the encounter      ends,      the effects trigger as normal."
           "PG 130 CB"
  , Talent "Preternatural Speed"
           "WS 40, Ag 50"
           [StatApt Ag, Offence]
           3
           "Double speed when charging."
           "PG 130 CB"
  , Talent "Push the Limit"
           "Rank 2 in Survival or any Operate skill, Tech-Use"
           [StatApt Per, Tech]
           3
           "Once per round, the character may add +20 to an Operate test however, if he      fails the      test by 4 or more degrees of failure, immediately roll 1d5 on Table 7-32: Motive      Systems Critical Hit effects and apply the result. If he is riding a living      mount,      roll 1d5 on Table 7-18: Impact Critical effects - Leg"
           "PG 63 EO"
  , Talent "Sanctic Purity"
           "Daemonologist, Favoured by the Warp, WP 50"
           [Psyker, StatApt Will]
           3
           "When the character triggers Psychic Phenomena when manifesting a power from the      Sanctic      Daemonology discipline, he may spend a Fate point in order to negate the result      entirely."
           "PG 61 EB"
  , Talent "Shield Wall"
           "Ambidextrous, WS 40"
           [Defence, StatApt WS]
           3
           "When armed with a shield, the character can re-roll one failed Evasion test to      Parry      an attack per round."
           "PG 61 EB"
  , Talent "Sprint"
           "-"
           [StatApt Ag, Fieldcraft]
           3
           "Move more quickly in combat."
           "PG 131 CB"
  , Talent "Step Aside"
           "Ag 40, Dodge or Parry"
           [StatApt Ag, Defence]
           3
           "Can make additional Dodge or Parry attempt per round."
           "PG 131 CB"
  , Talent "Superior Chirurgeon"
           "Rank 2 in Medicae skill"
           [StatApt Int, Fieldcraft]
           3
           "Gain +20 to Medicae tests, bonuses to first aid tests and only suffers a -10      penalty      for those suffering Critical damage."
           "PG 131 CB"
  , Talent "Target Selection"
           "BS 50"
           [StatApt BS, Finesse]
           3
           "May shoot into melee without penalty. Also an Aim action beforehand, prevents      any chance      of hitting friendly targets as well."
           "PG 132 CB"
  , Talent "Thunder Charge"
           "S 50"
           [StatApt Str, Offence]
           3
           "Break enemies with armoured charge."
           "PG 132 CB"
  , Talent "True Grit"
           "T 40"
           [StatApt Tou, Defence]
           3
           "Reduce Critical damage taken."
           "PG 132 CB"
  , Talent "Two-Weapon Master"
           "Ag 45, Ambidextrous, BS 40 or WS 40, Two-Weapon Wielder (Melee, Ranged)"
           [Finesse, Offence]
           3
           "No penalties when fighting with two single handed weapons."
           "PG 132 CB"
  , Talent "Warp Lock"
           "Psy Rating, Strong Minded, WP 50"
           [StatApt Will, Psyker]
           3
           "Ignore Psychic Phenomenon once per session. Suffers 1d5 Energy damage to the      Head location      (not reduced by Armour or StatApt Tou) as a result, and cannot make any Focus      Power      tests or sustain other psychic powers until the beginning of his next turn.    "
           "PG 133 CB"
  , Talent "Weapon Intuition"
           "Exotic Weapon Training (Any)"
           [StatApt Int, Finesse]
           3
           "The character reduces the penalty for using a weapon without the proper training      by 10.    "
           "PG 63 EO"
  ]