module Model exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import FieldLens exposing (FieldLens)
import Selectize
import Set exposing (Set)
import Skills exposing (Skill)
import Stats exposing (Aptitude(..), StatName(..))
import String exposing (startsWith)
import Util exposing (Selector, SelectorLens, SelectorMsg(..), menuLens, selectionLens)

type alias Model =
  { weaponSkill: (Int, Int, List Aptitude)
  , ballisticSkill: (Int, Int, List Aptitude)
  , strength: (Int, Int, List Aptitude)
  , toughness: (Int, Int, List Aptitude)
  , agility: (Int, Int, List Aptitude)
  , intelligence: (Int, Int, List Aptitude)
  , perception: (Int, Int, List Aptitude)
  , willpower: (Int, Int, List Aptitude)
  , fellowship: (Int, Int, List Aptitude)
  , influence: Int
  , freeExp: Int
  , spentExp: Int
  , aptitudes: List Aptitude
  , skills: Dict String (Skill, Int)
  , temp: Maybe Skill
  , drop: String
  , talentSelector: Selector Talent
  , talents: Dict String Talent
  , talentSpecInput: String
  , traits: Set String
  , implants: Set String
  , psyRating: Int
  , madness: Int
  , corruption: Int
  }

type ModelPredicate = ModelPredicate (Model -> Bool)

type TSpec = Fixed (Array String) | Free
type alias Talent = { spec: Maybe TSpec
                    , name: String
                    , prerequisites: String
                    , predicate: ModelPredicate
                    , aptitudes: List Aptitude
                    , tier: Int
                    , benefit: String
                    , page: String
                    }

mkTalent = Talent Nothing
mkFree = Talent <| Just Free
mkFixed = Talent << Just << Fixed << Array.fromList

getTalentCost: (Int, Int) -> Maybe Int
getTalentCost (aptitudes, rank) =
  let costs =
        Array.fromList <| List.map Array.fromList
        [ [ 600, 900, 1200 ]
        , [ 300, 450, 600 ]
        , [ 200, 300, 400 ]
        ]
    in
  Array.get aptitudes costs |> Maybe.andThen (Array.get rank)

checkTalent: Talent -> Model -> Bool
checkTalent talent =
  let (ModelPredicate p) = talent.predicate in p

type alias StatTriplet = (Int, Int, List Aptitude)
type alias ModelLens = FieldLens Model StatTriplet Int Model
weaponSkill : ModelLens
weaponSkill = FieldLens .weaponSkill (\v r -> let (_, base, apts) = r.weaponSkill in { r | weaponSkill = (v, base, apts) })

ballisticSkill : ModelLens
ballisticSkill = FieldLens .ballisticSkill (\v r -> let (_, base, apts) = r.ballisticSkill in { r | ballisticSkill = (v, base, apts) })

strength : ModelLens
strength = FieldLens .strength (\v r -> let (_, base, apts) = r.strength in { r | strength = (v, base, apts) })

toughness : ModelLens
toughness = FieldLens .toughness (\v r -> let (_, base, apts) = r.toughness in { r | toughness = (v, base, apts) })

agility : ModelLens
agility = FieldLens .agility (\v r -> let (_, base, apts) = r.agility in { r | agility = (v, base, apts) })

intelligence : ModelLens
intelligence = FieldLens .intelligence (\v r -> let (_, base, apts) = r.intelligence in { r | intelligence = (v, base, apts) })

perception : ModelLens
perception = FieldLens .perception (\v r -> let (_, base, apts) = r.perception in { r | perception = (v, base, apts) })

willpower : ModelLens
willpower = FieldLens .willpower (\v r -> let (_, base, apts) = r.willpower in { r | willpower = (v, base, apts) })

fellowship : ModelLens
fellowship = FieldLens .fellowship (\v r -> let (_, base, apts) = r.fellowship in { r | fellowship = (v, base, apts) })


talentSelector: SelectorLens Model Talent
talentSelector =
  FieldLens .talentSelector (\val rec -> { rec | talentSelector = val})


talentMenu : FieldLens Model (Selectize.State Talent) (Selectize.State Talent) Model
talentMenu = FieldLens.compose talentSelector menuLens

talentSelection : FieldLens Model (Maybe Talent) (Maybe Talent) Model
talentSelection = FieldLens.compose talentSelector selectionLens

--compose1 : FieldLens a b d e -> FieldLens b c g d -> FieldLens a c g e
--compose1 a b =
--    FieldLens (get a >> get b) (\x r -> modify a (set b x) r)

talentUpdate = Util.selectorUpdate talentSelector

type alias TalentPrerequisite = Model -> Talent -> Bool
--type TalPre = TalPre String (Model -> Bool) (List Aptitude)
type alias TalPre =
  { name: String
  , pred: Model -> Bool
  , apts: List Aptitude
  }

talentList =
  let statThresh: ModelLens -> Int -> Model -> Bool
      statThresh sLens val = (<=) val << Stats.value << FieldLens.get sLens
      sAnd: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
      sAnd f1 f2 = \m -> f1 m && f2 m
      sOr f1 f2 = \m -> f1 m || f2 m
      talentCheck name = Dict.member name << .talents
      skillCheck name = Dict.member name << .skills
      skillThresh name value = .skills >> Dict.get name >> Maybe.map (\(_, v) -> v >= value) >> Maybe.withDefault False
      psyThresh value = \m -> m.psyRating >= value
      p = ModelPredicate
  in
  Dict.fromList <| List.map (\tal -> (tal.name, tal)) <|
  [ mkTalent "Ambidextrous"
           "Ag 30"
           (p <| statThresh agility 30)
           [StatApt WS, StatApt BS]
           1
           "When combined with Two-Weapon Wielder, the penalty for attacks with both weapons      in the      same turn drops to -10."
           "PG 123 CB"

  , mkTalent "Blind Fighting"
           "Per 30"
           (p <| statThresh perception 30)
           [StatApt Per, Fieldcraft]
           1
           "Ignores all penalties for fighting with a melee weapon while suffering from      obscured      vision    "
           "PG 123 CB"
  , mkTalent "Bodyguard"
           "Ag 35"
           (p <| statThresh agility 35)
           [StatApt Ag, Defence]
           1
           "After an enemy makes a successful attack against an ally, the character may use      a Reaction      to move up to his Half Move distance in order to interpose himself between the      attacker      and target. The attack is then resolved against the character instead of the      original      target. In the case of a melee attack, the character may also attempt to Parry      the      attack as part of his Reaction."
           "PG 60 EB"
  , mkTalent "Catfall"
           "Ag 30"
           (p <| statThresh agility 35)
           [StatApt Ag, Fieldcraft]
           1
           "Reduces the effective distance of all falls by a number of metres equal to his      StatApt Ag      bonus. Also adds +20 to his Acrobatics skill tests when using Jump"
           "PG 124 CB"
  , mkTalent "Clues from the Crowds"
           "Fel 30"
           (p <| statThresh fellowship 30)
           [General, Social ]
           1
           "Once per day, he can re-roll a test made to gather information from a group of      people."
           "PG 124 CB"
  , mkTalent "Die Hard"
           "WP 40"
           (p <| statThresh willpower 30)
           [StatApt Will, Defence ]
           1
           "Test StatApt Will to avoid Fatigue from Blood Loss with a Challenging (+0)      Willpower test."
           "PG 125 CB"
  , mkTalent "Disarm "
           "Ag 30"
           (p <| statThresh agility 30)
           [StatApt WS, Defence]
           1
           "As a Full Action, may make an Opposed StatApt WS test and force opponent to      drop weapon.      If 3 or more DoS, can steal weapon."
           "PG 125 CB"
  , mkTalent "Double Team"
           "-"
           (p <| \_ -> True)
           [General, Offence ]
           1
           "Gain additional +10 for outnumbering opponent."
           "PG 126 CB"
  , mkTalent "Enemy (choose)"
           "-"
           (p <| \_ -> True)
           [General, Social]
           1
           "Suffers a -10 times X penalty to StatApt Fell and Influence tests when dealing      with group."
           "PG 126 CB"
  , mkTalent "Ferric Summons"
           "Ferric Lure Implants, Mechanicus Implants"
           (p <| \m -> Set.member "Ferric Lure Implants" m.implants  && Set.member "Mechanicus Implants" m.traits )
           [StatApt Will, Tech]
           1
           "Can summon larger metallic objects up to 2 kilograms per point of his StatApt Will      bonus,      and can summon such objects up to 40 metres."
           "PG 127 CB"
  , mkTalent "Flagellant"
           "StatApt Will 30"
           (p <| statThresh willpower 30)
           [Offence, StatApt Tou]
           1
           "As a Full Action, the character      can take 1d5–2 levels of Fatigue (minimum 1) to gain a +10      bonus on StatApt Will tests to resist Fear, Pinning, psychic powers,      or Corruption, for one hour or until the end of the      current encounter. If the Acolyte also possesses the Frenzy talent,      he can enter a Frenzied state as a Free Action while under the      efects of this talent."
           "PG 127 CB"
  , mkTalent "Frenzy"
           "-"
           (p <| \_ -> True)
           [StatApt Str, Offence]
           1
           "May spend one full round to enter Frenzy gaining +10 bonus to StatApt WS,      StatApt Str,      StatApt Tou, and StatApt Will, but suffering a -20 penalty to StatApt BS,      StatApt Int,      and StatApt Fell and is immune to Fear, Pinning, Stunning effects, and the effects      of Fatigue; he cannot Parry, retreat, or flee. He remains Frenzied for the      duration      of the combat, and cannot use psychic powers while Frenzied. After combat ends      can      make a Willpower test to snap out and cannot Frenzy again for at least an hour.    "
           "PG 127 CB"
  , mkTalent "Grenadier"
           "BS 35"
           (p <| statThresh ballisticSkill 35)
           [StatApt BS, Finesse]
           1
           "When the character misses with a thrown weapon or weapon with the Blast quality,      he may      reduce the distance it scatters by a number of metres up to half his BS bonus    "
           "PG 62 EO"
  , mkTalent "Iron Jaw"
           "T 40"
           (p <| statThresh toughness 40)
           [StatApt Tou, Defence]
           1
           "Test Challenging (+0) StatApt Tou Test to overcome Stunning."
           "PG 128 CB"
  , mkTalent "Jaded"
           "WP 40"
           (p <| statThresh willpower 40)
           [StatApt Will, Defence]
           1
           "Ignore mundane horrors - dead bodies, xenos abominations, etc. do not cause the      Acolyte      to gain Insanity points, nor do they require a fear rolls. Daemons,      manifestations      of the Warp, and other unnatural or supernatural horrors still inflict their      effects.    "
           "PG 128 CB"
  , mkTalent "Keen Intuition"
           "Intelligence 35"
           (p <| statThresh intelligence 35)
           [StatApt Per, Social]
           1
           "Can retry Awareness test once with -10 modifier."
           "PG 129 CB"
  , mkTalent "Leap Up"
           "Ag 30"
           (p <| statThresh agility 30)
           [StatApt Ag, General]
           1
           "Stand as a Free Action."
           "PG 129 CB"
  , mkTalent "Leaping Dodge"
           "Ag 35, Rank 2 in the Dodge skill"
           (p <| sAnd (statThresh agility 35) (skillThresh "Dodge" 10))
           [StatApt Ag, Defence]
           1
           "When he would make an StatApt Ag test to avoid attacks from weapons with the Spray      quality,      he may make the test using his Dodge (Ag) skill instead."
           "PG 63 EO"
  --, mkTalent "Mounted Warrior" -- TODO
  --         "Rank 2 (Trained) in any Operate skill or Rank 2(Trained) in Survival skill, BS 30 or WS 30"
  --         (\_ -> False)
  --         [StatApt WS / StatApt BS, Offence]
  --         1
  --         "Reduces any penalty for making attacks (Melee or Ranged) from a moving vehicle      or mount      by 10 for each advance."
  --         "PG 58 EI"
  , mkTalent "Nowhere to Hide"
           "Per 30"
           (p <| statThresh perception 30)
           [StatApt Per, Offence]
           1
           "Can add DoS from the attack to reduce armour value of cover."
           "PG 130 CB"
  , mkTalent "Peer (choose)"
           "Fel 30"
           (p <| statThresh fellowship 30)
           [StatApt Fell, Social]
           1
           "Character has good reputation amongst chosen group with bonus to StatApt Fell      tests increases      to +10 times X. Additionally, when the character acquires his talent, also      increases      his Influence by 1."
           "PG 130 CB"
  , mkTalent "Quick Draw"
           "-"
           (p <| \_ -> True)
           [StatApt Ag, Finesse]
           1
           "Draw weapon as Free Action."
           "PG 131 CB"
  , mkTalent "Rapid Reload"
           "-"
           (p <| \_ -> True)
           [StatApt Ag, Fieldcraft ]
           1
           "Reduce reload time by half time."
           "PG 131 CB"
  , mkTalent "Resistance (choose)"
           "-"
           (p <| \_ -> True)
           [StatApt Tou, Defence]
           1
           "Gain +10 bonus to particular resistance test."
           "PG 131 CB"
  , mkTalent "Skilled Rider"
           "Rank 2 in any Operate skill "
           (p <| \_ -> False) -- TODO read rulebook
           [StatApt Ag, Fieldcraft]
           1
           "Whenever the character would be thrown from or tossed about within his vehicle,      he makes      an Ordinary (+10) StatApt Ag test. If he succeeds, the character may choose to      either      land safely on his feet or retain in his original position in the vehicle. In      addition,      once per round the character can attempt an Ordinary (+10) Agility test to Mount      or Dismount a vehicle as a Free Action."
           "PG 63 EO"
  , mkTalent "Sound Constitution"
           "-"
           (p <| \_ -> True)
           [StatApt Tou, General]
           1
           "Gain an additional wound."
           "PG 131 CB"
  , mkTalent "Takedown"
           "-"
           (p <| \_ -> True)
           [StatApt WS, Offence]
           1
           "Make special attack to stun opponent."
           "PG 132 CB"
  , mkTalent "Technical Knock"
           "Int 30"
           (p <| statThresh intelligence 30)
           [StatApt Int, Tech]
           1
           "Un-jam gun as Half Action."
           "PG 132 CB"
  , mkTalent "Warp Sense"
           "Psy Rating, Psyniscience, Per 30 "
           (p <| sAnd (statThresh perception 30)
              <| sAnd (Dict.member "Psyniscience" << .skills)
              <| psyThresh 0
           )
           [StatApt Per, Psyker]
           1
           "Allows Psyniscience test as Free Action."
           "PG 133 CB"
  , mkFixed ["Bolt", "Chain", "Burn", "Heavy", "Laz", "Launcher", "Melta", "Plasma", "Force", "Nano", "Shock", "Stub"]
           "Weapon Training"
           "-"
           (p <| \_ -> True)
           [General, Finesse]
           1
           "Use Weapon Group without penalty."
           "PG 133 CB"
  , mkTalent "Weapon-Tech"
           "Tech Use +10, Int 40"
           (p <| sAnd (statThresh intelligence 40) (skillThresh "Dodge" 10))
           [StatApt Int, Tech]
           1
           "May enhance any Melta, Plasma, Power, or Exotic weapon by increases the weapon's      damage      and penetration by an amount equal to the character's StatApt Int bonus until      the      end of the round once per encounter."
           "PG 133 CB"
  , mkTalent "Ambassador Imperialis"
           "Fellowship 35, Intelligence 35"
           (p <| sAnd (statThresh fellowship 35) (statThresh intelligence 35))
           [StatApt Per, Social]
           2
           "Any penalty to Interaction skill tests for dealing with xenos or non-Imperial      NPCs by      20. In addition, once per encounter, he can reroll a failed Interaction skill      test      when interacting with such an NPC. "
           "PG 62 EO"
  , mkTalent "Archivator"
           "Intelligence 40"
           (p <| statThresh intelligence 40)
           [Knowledge, Social]
           2
           "A Scholastic Lore or Forbidden Lore test utilising sources of recorded      information of      any sort can re-roll with -10."
           "PG 62 EO"
  , mkTalent "Armor-Monger"
           "Int 35, Tech-Use, Trade (Armourer)"
           (p <| sAnd (statThresh intelligence 35)
              <| sAnd (skillCheck "Tech-Use") (skillCheck "Trade (Armourer)")
           )
           [StatApt Int, Tech]
           2
           "Gains an extra amount of Armor points equal to his Intelligence bonus. Takes an      hour      each day.    "
           "PG 123 CB"
  , mkTalent "Battle Rage"
           "Frenzy"
           (p <| talentCheck "Frenzy")
           [StatApt Str, Defence]
           2
           "Can Parry while Frenzied, and can re-roll a failed test to snap out of Frenzy or      resist      entering Frenzy if they choose."
           "PG 123 CB"
  , mkTalent "Bulging Biceps"
           "S 45"
           (p <| statThresh strength 45)
           [StatApt Str, Offence]
           2
           "Remove bracing requirement from Heavy weapons, and +20 to his Athletics skill      test when      using Heft.    "
           "PG 123 CB"
  , mkTalent "Bulwark of Faith"
           "WP 45, Iron Faith"
           (p <| sAnd (statThresh willpower 45) <| talentCheck "Iron Faith")
           [Defence, StatApt Will]
           2
           "When passing a Fear test caused by a Daemon, the Daemon suffers 1 Energy damage      ignoring      armour and StatApt Tou bonus for each degree of success on the Fear test."
           "PG 60 EB"
  , mkTalent "Combat Master"
           "WS 30"
           (p <| statThresh weaponSkill 30)
           [StatApt WS, Defence]
           2
           "Opponents get no bonus for outnumbering the character."
           "PG 124 CB"
  , mkTalent "Constant Vigilance (choose)"
           "Int 35 or Per 35, Awareness +10"
           (p <| \_ -> False) -- TODO Spec Talent with spec dependent prerequisite
           [StatApt Per, Defence]
           2
           "Can use Per or Int instead of Ag for Initiative rolls, and rolls two dice      (picking higher)      for the result."
           "PG 124 CB"
  , mkTalent "Contact Network"
           "Cover-Up, Int 35"
           (p <| sAnd (statThresh intelligence 35) <| talentCheck "Cover-Up")
           [StatApt Fell, Leadership]
           2
           "Use StatApt Fell instead of Influence for Requisition tests."
           "PG 124 CB"
  , mkTalent "Coordinated Interrogation"
           "S 40 or WP 40, Clues from the Crowds, Rank 1 (Known) Interrogation"
           (statThresh strength 40 |> sOr (statThresh willpower 40)
                               |> sAnd (talentCheck "Clues from the Crouds")
                               |> sAnd (skillCheck "Interrogation")
                               |> p
           )
           [StatApt Int, Social]
           2
           "Has a +10 bonus to all Interrogate tests, additional +5 for others who also have      Coordinated      Interrogation    "
           "PG 124 CB"
  , mkTalent "Counter Attack"
           "WS 40"
           (p <| statThresh weaponSkill 40)
           [StatApt WS, Defence]
           2
           "May make a Standard Attack after successful Parry with -20 to StatApt WS."
           "PG 125 CB"
  , mkTalent "Cover-Up"
           "Int 35"
           (p <| statThresh intelligence 35)
           [StatApt Int, Knowledge]
           2
           "Can reduce Influence by 1 to gain 1d5 Subtlety."
           "PG 125 CB"
  , mkTalent "Daemonhunter"
           "Forbidden Lore (Daemonology), WP 40"
           (skillCheck "Forbidden Lore (Daemonology)" |> sAnd (statThresh willpower 40) |> p)
           [Offence, StatApt Will]
           2
           "May re-roll failed Awareness and Psyniscience Tests to detect the presence of      Daemons.      In addition, his attacks against Daemons gain the Proven (3) quality."
           "PG 60 EB"
  , mkTalent "Daemonologist"
           "Psy rating 3, WP 45, Forbidden Lore (Daemonology)"
           (statThresh willpower 45 |> sAnd (skillCheck "Forbidden Lore (Daemonology)")
                                    |> sAnd (psyThresh 3)
                                    |> p
           )
           [Psyker, StatApt Will]
           2
           "When the character takes the Focus Power action and the target of the psychic      power is      a Daemon, he gains a +10 bonus to the Focus Power test. At the GM's discretion,      this      bonus may apply to other tests, such as rituals to summon or bind a Daemon."
           "PG 60 EB"
  , mkTalent "Deny the Witch"
           "WP 35"
           (p <| statThresh willpower 35)
           [StatApt Will, Defence]
           2
           "Can use StatApt Will to Evade against psychic attacks."
           "PG 125 CB"
  , mkTalent "Devastating Assault"
           "WS 35"
           (p <| statThresh weaponSkill 35)
           [StatApt WS, Offence]
           2
           "Successful All Out Attack grants, may make a second All Out Attack once per      turn."
           "PG 125 CB"
  , mkTalent "Double Tap"
           "Two-Weapon Wielder"
           (p <| talentCheck "Two-Weapon Wielder (Ranged)")
           [Finesse, Offence]
           2
           "A second ranged attack against the same target, grants a+20 bonus if scored 1 or      more      DoS.    "
           "PG 125 CB"
  , mkFree "Exotic Weapon Training" -- TODO Spec Talent
           "-"
           (p <| \_ -> True)
           [StatApt Int, Finesse]
           2
           "Gain proficiency with one exotic weapon type."
           "PG 127 CB"
  , mkTalent "Face in a Crowd"
           "Fel 35, Clues from the Crowds"
           (statThresh fellowship 35 |> sAnd (talentCheck "Clues from the Crowds") |> p)
           [StatApt Fell, Social]
           2
           "Can use StatApt Fell instead of StatApt Ag when Shadowing."
           "PG 127 CB"
  --, mkTalent "Field Vivisection" -- TODO
  --         "BS or WS 40, Forbidden Lore (Xenos-Any), Rank 2 in the Medicae skill"
  --         ( p <| statThresh ballisticSkill 40 |> sOr (statThresh weaponSkill 40)
  --         |> sAnd (skillThresh "Medicae" 10)
  --         |> sAnd (skillCheck "Forbidden Lore (Xenos)") -- TODO match forbidden lore
  --         )
  --         [StatApt BS/StatApt WS, Knowledge]
  --         2
  --         "When using the Called Shot action with a melee or ranged attack (depending on      the Specialisation)      against a target for which the character has the appropriate Forbidden Lore      (Xenos)      skill, he makes a Medicae (WS) or Medicae (BS) test in place of the normal      Weapon      Skill or StatApt BS test. "
  --         "PG 62 EO"
  , mkTalent "Hard Target"
           "Ag 40"
           (p <| statThresh agility 40)
           [StatApt Ag, Defence]
           2
           "-20 to hit character when he Charges or Runs."
           "PG 128 CB"
  , mkTalent "Harden Soul"
           "WP 35, 10 Corruption points"
           (statThresh willpower 35 |> sAnd (\m -> m.corruption >= 10) |> p)
           [Defence, StatApt Will]
           2
           "Whenever the character would gain Corruption points, he may reduce the amount      gained      by half (rounded up) and gain Insanity points equal to the amount reduced."
           "PG 61 EB"
  , mkTalent "Hardy"
           "T 40"
           (p <| statThresh toughness 40)
           [StatApt Tou, Defence]
           2
           "Character always heals as if Lightly Damaged."
           "PG 128 CB"
  , mkTalent "Hatred (choose)"
           "-"
           (p <| \_ -> True)
           [StatApt WS, Social]
           2
           "Gain +10 bonus to attack StatApt WS tests. Must make a Challenging (+0)      StatApt Will test      to retreat or surrender."
           "PG 128 CB"
  , mkTalent "Hip Shooting"
           "BS 40, Ag 40"
           (statThresh ballisticSkill 40 |> sAnd (statThresh agility 40) |> p)
           [StatApt BS, Finesse]
           2
           "As a Full Action, can both move up to his Full Move rate and make a single shot      attack      with a ranged weapon per weapon."
           "PG 128 CB"
  --, mkTalent "Hotshot Pilot" -- TODO Spec Talent?
  --         " Rank 2 in Survival or any Operate skill, Ag 35 "
  --         (p <| statThresh agility 35)
  --         [StatApt Ag, Tech]
  --         2
  --         "On an succesful Operate test, he may voluntarily suffer 1 level of Fatigue to      add a number      of DoS equal to half of his StatApt Ag bonus. When failed an Operate test, he may      voluntarily      suffer 1 level of Fatigue in order to reduce the degrees of failure by an amount      equal to his Agility bonus, to a minimum of 1. "
  --         "PG 62 EO"
  , mkTalent "Independent Targeting"
           "BS 40"
           (p <| statThresh ballisticSkill 40)
           [StatApt BS, Finesse]
           2
           "Fire at multiple targets more than 10 metres apart when firing two weapons."
           "PG 128 CB"
  , mkTalent "Inescapable Attack (Melee)"
           "WS 40,Per 35"
           (statThresh weaponSkill 40 |> sAnd (statThresh perception 35) |> p)
           [StatApt WS, Finesse]
           2
           "Attacker imposes penalty on all evasion attempts made against this attack equal      to 10      times the total degrees of success scored on the attack test."
           "PG 128 CB"
  , mkTalent "Inescapable Attack (Ranged)"
           "BS 40,Per 35"
           (statThresh ballisticSkill 40 |> sAnd (statThresh perception 35) |> p)
           [StatApt BS, Finesse]
           2
           "Attacker imposes penalty on all evasion attempts made against this attack equal      to 10      times the total degrees of success scored on the attack test."
           "PG 128 CB"
  , mkTalent "Inspiring Aura"
           "Halo of Command"
           (p <| talentCheck "Halo of Command")
           [Leadership, StatApt Will]
           2
           "can affect allies of any kind with the Terrify special use for the Command skill      (pg      101 CB). This need not represent threats and intimidation, but might represent      inspiring      words, encouragement, or sheer steadfastness in the face of terrifying foes.    "
           "PG 61 EB"
  , mkTalent "Iron Resolve"
           "Resistance (Fear), Jaded"
           (talentCheck "Resistance (Fear)" |> sAnd (talentCheck "Jaded") |> p)
           [Defence, StatApt Will]
           2
           "After failing a Fear or Pinning test, the character can re-roll the test with a      -10 modifier."
           "PG 61 EB"
  , mkTalent "Killing Strike"
           "WS 50"
           (p <| statThresh weaponSkill 50)
           [StatApt WS, Offence]
           2
           "Spend Fate point to make melee attacks unavoidable."
           "PG 129 CB"
  , mkTalent "Lexographer"
           "Rank 3 in Linguistics (Any)"
           (p <| \m -> Dict.foldl (\key (_, value) acc ->
                               if key |> startsWith "Linguistics" then max value acc
                                                                  else acc
                             )
                             0 m.skills >= 20
           )
           [StatApt Int, Knowledge]
           2
           "The character can attempt any Linguistics skill in which he is not trained as an      untrained      skill test as if it were not a Specialist skill. "
           "PG 63 EO"
  , mkTalent "Luminen Shock"
           "Luminen Capacitors, Mechanicus Implants"
           (p <| \m -> Set.member "Luminen Capacitors" m.implants  && Set.member "Mechanicus Implants" m.traits )
           [StatApt WS, Tech]
           2
           "Counts as a melee weapon that inflicts 1d10 plus his WPB in Energy damage, with      Pen 0      and the Shocking quality. Must pass a StatApt Tou test or suffer 1 level of      Fatigue      after attack."
           "PG 129 CB"
  , mkTalent "Maglev Transcendence"
           "Maglev Coils, Mechanicus Implants"
           (p <| \m -> Set.member "Maglev Coils" m.implants  && Set.member "Mechanicus Implants" m.traits )
           [StatApt Int, Tech]
           2
           "Can hover for a number of minutes equal to 1d10 plus twice his StatApt Tou bonus.      He can      move his Run speed when making a Half Move action and suffers no damage from      falling      if the coils are active. Each use drains half the power stored in the coils (can      use the coils twice before recharging them)."
           "PG 129 CB"
  , mkTalent "Marksman"
           "BS 35"
           (p <| statThresh ballisticSkill 35)
           [StatApt BS, Finesse]
           2
           "No penalties for firing at long or extreme range."
           "PG 130 CB"
  , mkTalent "Mechadendrite Use (choose)" -- TODO Spec Talent
           "Mechanicus Implants"
           (p <| \m -> Set.member "Mechanicus Implants" m.traits)
           [StatApt Int, Tech]
           2
           "Gain ability to use certain Mechadendrites. Weapon or Utility."
           "PG 130 CB"
  , mkTalent "One-on-One"
           "WS 40"
           (p <| statThresh weaponSkill 40)
           [Finesse, StatApt WS]
           2
           "When fighting a single enemy in melee combat, the character scores extra degrees      of success      on successful StatApt WS tests equal to half of his Weapon Skill bonus      (rounded      down).    "
           "PG 61 EB"
  , mkTalent "Penitent Psyker"
           "Psy rating, Strong Minded, WP 40"
           (psyThresh 0 |> sAnd (talentCheck "Strong Minded")
                        |> sAnd (statThresh willpower 40)
                        |> p
           )
           [Psyker, Defence]
           2
           "psyker or ally within 10 metres target of a psychic power suffer any number of      levels      of Fatigue, each level grants +10 to opposed test to resist or avoid."
           "PG 58 EI"
  , mkTalent "Precision Killer (Ranged)"
           "BS 40"
           (p <| statThresh ballisticSkill 40)
           [StatApt BS, Finesse]
           2
           "No penalty to making Called Shot in either Ranged or Melee Combat."
           "PG 130 CB"
  , mkTalent "Precision Killer (Melee)"
           "WS 40"
           (p <| statThresh weaponSkill 40)
           [StatApt WS, Finesse]
           2
           "No penalty to making Called Shot in either Ranged or Melee Combat."
           "PG 130 CB"
  , mkTalent "Prosanguine"
           "Auto Sanguine Implants, Mechanicus Implants"
           (p <| \m -> Set.member "Auto Sanguine Implants" m.implants  && Set.member "Mechanicus Implants" m.traits )
           [StatApt Tou, Tech]
           2
           "Spend 10 minutes and make a Tech-Use test to heal 1d5 damage. Rolls 96 or      higher, loses      the ability for one week."
           "PG 131 CB"
  , mkTalent "Purity of Hatred"
           "Hatred (Any)"
           (p <| List.any (startsWith "Hatred") << Dict.keys << .talents  )
           [Offence, StatApt Will]
           2
           "Attacks gain the Vengeful (9) quality against opponents of Hatred group."
           "PG 58 EI"
  , mkTalent "Rites of Banishment"
           "Common Lore (Imperial Creed) +10 or Forbidden Lore (Daemonology)"
           (skillThresh "Common Lore (Imperial Creed)" 10 |> sOr (skillCheck "Forbidden Lore (Daemonology)")
                                                          |> p
           )
           [Offence, StatApt Will]
           2
           "Once per round as a Half Action, the character may speak the litanies and      invocations      to disrupt Daemons. Until the beginning of his next turn, Daemons within a      distance      equal to twice the character's StatApt Will bonus in meters suffer a -10 penalty to      Willpower tests.    "
           "PG 61 EB"
  , mkTalent "Strong Minded"
           "WP 30, Resistance (Psychic Powers)"
           (statThresh willpower 30 |> sAnd (talentCheck "Resistance (Psychic Powers)") |> p)
           [StatApt Will, Defence]
           2
           "May reroll failed WP tests to resist psychic powers."
           "PG 131 CB"
  , mkTalent "Swift Attack"
           "WS 30"
           (p <| statThresh weaponSkill 30)
           [StatApt WS, Finesse]
           2
           "May make multiple melee attacks."
           "PG 131 CB"
  , mkTalent "Tainted Psyker"
           "Psy rating, Rank 2 (Trained) in Psyniscience Skill, 10 Corruption points"
           (psyThresh 0 |> sAnd (skillThresh "Psyniscience" 10)
                        |> sAnd (\m -> m.corruption >= 10)
                        |> p
           )
           [Knowledge, Psyker]
           2
           "When making a Focus Power test, the character may gain a number of Corruption      points      up to his psy rating. For each point he gains in this way, he gains a +10 bonus      but      adds +5 to rolls on Table 6-2 Psychic Phenomena"
           "PG 58 EI"
  , mkTalent "Two-Weapon Wielder (Ranged)"
           "-"
           (p <| \_ -> True)
           [StatApt BS, Finesse]
           2
           "Attacks with two weapons count as being part of the same Half Action, and both      tests      made to attack with the weapons suffer a -20 penalty. "
           "PG 132 CB"
  , mkTalent "Two-Weapon Wielder (Melee)"
           "-"
           (p <| \_ -> True)
           [StatApt WS, Finesse]
           2
           "Attacks with two weapons count as being part of the same Half Action, and both      tests      made to attack with the weapons suffer a -20 penalty. "
           "PG 132 CB"
  , mkTalent "Unarmed Specialist"
           "Ambidextrous, Ag 35, WS 35"
           (statThresh agility 35 |> sAnd (statThresh weaponSkill 35)
                                  |> sAnd (talentCheck "Ambidextrous")
                                  |> p
           )
           [StatApt Str, Offence]
           2
           "When fighting unarmed, attacks hit as Deadly Natural Weapons and user can      re-roll damage."
           "PG 132 CB"
  , mkTalent "Warp Conduit"
           "Psy Rating, Strong Minded, WP 50"
           (psyThresh 0 |> sAnd (talentCheck "Strong Minded")
                        |> sAnd (statThresh willpower 50)
                        |> p
           )
           [StatApt Will, Psyker]
           2
           "Spend Fate point to add 1d5 to psy rating however he adds +30 to rolls on Table      6-2:      Psychic Phenomena    "
           "PG 132 CB"
  , mkTalent "Whirlwind of Death"
           "WS 40"
           (p <| statThresh weaponSkill 40)
           [StatApt WS, Finesse]
           2
           "As a Half Action, may make one Standard Attack action with a melee weapon      against a foe,      plus one additional Standard Attack action with the same weapon targeting each      other      foe also engaged in melee combat with the character beyond the first (to a      maximum      number of attacks up to his StatApt WS bonus)."
           "PG 133 CB"
  , mkTalent "Witch Finder"
           "Rank 2 (Trained) in the Forbidden Lore (Psykers)skill, WP 45"
           (skillThresh "Forbidden Lore (Psykers)" 10 |> sAnd (statThresh willpower 45) |> p)
           [Knowledge, StatApt Per]
           2
           "Counts as possessing the Psyniscience skill at Rank 1 (Known), even though he is      not      a psyker."
           "PG 58 EI"
  , mkTalent "Xenosavant"
           "Rank 3 in Forbidden Lore (Xenos-Any) "
           (p <| \_ -> False) -- TODO FUKKEN XENOS
           [StatApt Int, Knowledge]
           2
           "The character can attempt any Forbidden Lore (Xenos) test in which he is not      trained      as an untrained skill test as if it were not a Specialist skill."
           "PG 63 EO"
  , mkTalent "Adamantium Faith"
           "Jaded, Resistance (Fear), WP 45"
           (talentCheck "Jaded" |> sAnd (talentCheck "Resistance (Fear)")
                                |> sAnd (statThresh willpower 45)
                                |> p
           )
           [StatApt Will, Defence]
           3
           "Subtract StatApt Will bonus from his degrees of failure on a failed Fear or Pinning      test.      If this reduces the result to zero or less, he counts as having passed the Fear      test      with 1 degree of success."
           "PG 123 CB"
  , mkTalent "Aegis of Contempt"
           "Shared Destiny, Shield of Contempt, Hatred (any)"
           (p <| \_ -> False) -- TODO Read Rulebook
           [Defence, Leadership]
           3
           "Character or an ally within 10 metres gains Corruption, reduce the amount by 1      to a min      of 0.    "
           "PG 57 EI"
  , mkTalent "Assassin Strike"
           "Ag 40, Acrobatics"
           (statThresh agility 40 |> sAnd (skillCheck "Acrobatics") |> p)
           [StatApt WS, Fieldcraft]
           3
           "After a melee attack, a Challenging (+0) Acrobatics skill test allows to move at      half      rate as a Free Action. Only once per round, and opponent does not receive a free      attack.    "
           "PG 123 CB"
  , mkTalent "Bastion of Iron Will"
           "Psy Rating, Strong Minded, WP 40"
           (psyThresh 0 |> sAnd (talentCheck "Strong Minded")
                        |> sAnd (statThresh willpower 40)
                        |> p
           )
           [StatApt Will, Psyker]
           3
           "Adds 5 x his psy rating to any Opposed test when defending against psychic      powers."
           "PG 123 CB"
  , mkTalent "Blademaster"
           "WS 30, Weapon Training (any Melee)"
           (p <| \_ -> False) -- TODO Melee Spec
           [StatApt WS, Finesse]
           3
           "When attacking with any bladed weapon, can re-roll one missed attack per round.    "
           "PG 123 CB"
  , mkTalent "Crushing Blow"
           "WS 40"
           (p <| statThresh weaponSkill 40)
           [StatApt WS, Offence]
           3
           "Add half WS bonus (Round up) to damage inflicted in melee."
           "PG 125 CB"
  , mkTalent "Daemonic Disruption"
           "Bane of the Daemon, WP 50, Untouchable elite advance"
           (p <| \_ -> False) -- TODO Elite Advance
           [StatApt Will, General]
           3
           "Whenever a creature with the Warp Instability trait makes a successful attack      test against      this character, it must immediately test for Warp Instability after resolving      the      attack.    "
           "PG 60 EB"
  , mkTalent "Dark Soul"
           "Hardened Soul, 20 Corruption points"
           (p <| \_ -> False) -- TODO What is Hardened Soul?
           [StatApt Tou, StatApt Will]
           3
           "When the character would test to gain a mutation as a result of increasing      Corruption,      he may choose to automatically pass the test. If he does, he also gains a      Malignancy      and increases his Corruption total by 1d10."
           "PG 60 EB"
  , mkTalent "Deathdealer (Ranged)"
           "BS 45"
           (p <| statThresh ballisticSkill 45)
           [StatApt Per, Finesse]
           3
           "Selects the Specialisation that matches the prerequisite used in purchase (Melee with Weapon Skill, Ranged with Ballistic Skill). Attack in that combat type inflicts Critical damage, add his Perception bonus to the damage result."
           "PG 125 CB"
  , mkTalent "Deathdealer (Melee)"
           "WS 45"
           (p <| statThresh weaponSkill 45)
           [StatApt Per, Finesse]
           3
           "Selects the Specialisation that matches the prerequisite used in purchase (Melee with Weapon Skill, Ranged with Ballistic Skill). Attack in that combat type inflicts Critical damage, add his Perception bonus to the damage result."
           "PG 125 CB"
  , mkTalent "Delicate Interrogation"
           "Fel 50, Coordinated Interrogation"
           (statThresh fellowship 50 |> sAnd (talentCheck "Coordinated Interrogation") |> p)
           [StatApt Int, Finesse]
           3
           "Reduce Subtlety loss by 1d5 when conducting an Interrogation. If this results in      a negative      number increase the warband's Subtlety by 1."
           "PG 125 CB"
  , mkTalent "Divine Protection"
           "BS 45, WP 35"
           (statThresh ballisticSkill 45 |> sAnd (statThresh willpower 35) |> p)
           [General, Finesse]
           3
           "Attacks using Spray quality only effects enemies."
           "PG 57 EI"
  , mkTalent "Eye of Vengeance"
           "BS 50"
           (p <| statThresh ballisticSkill 50)
           [StatApt BS, Offence]
           3
           "Spend 1 Fate point to add the number of DoS scored on the attack test to damage      and Penetration."
           "PG 127 CB"
  , mkTalent "Favored by the Warp"
           "WP 35"
           (p <| statThresh willpower 35)
           [StatApt Will, Psyker]
           3
           "Roll twice for Psychic Phenomena and choose result."
           "PG 127 CB"
  , mkTalent "Flash of Insight"
           "Int 40, Contact Network, Coordinated Interrogation"
           (statThresh intelligence 40 |> sAnd (talentCheck "Contact Network")
                                       |> sAnd (talentCheck "Coordinated Interrogation")
                                       |> p
           )
           [StatApt Per, Knowledge]
           3
           "Spend 1 Fate point to reveal a clue."
           "PG 127 CB"
  , mkTalent "Halo of Command"
           "Fel 40, WP 40"
           (statThresh fellowship 40 |> sAnd (statThresh willpower 40) |> p)
           [StatApt Fell, Leadership]
           3
           "Affect NPCs within 100xFelB metres with Social skills."
           "PG 127 CB"
  , mkTalent "Hammer Blow"
           "Crushing Blow"
           (p <| talentCheck "Crushing Blow")
           [StatApt Str, Offence]
           3
           "When he uses an All Out Attack action to make a single attack, he can add half      his StatApt Str      bonus (rounded up) to the weapon's penetration. The attack also counts as having      the Concussive (2) weapon quality."
           "PG 128 CB"
  , mkTalent "Hull Down"
           "Rank 2 in Survival or any Operate skill"
           (p <| \_ -> False) -- TODO Spec talent?
           [StatApt Ag, Fieldcraft]
           3
           "When the character takes a vehicle combat action with the Movement subtype, his      vehicle      or steed counts the value of its Size trait as being one lower for purposes of      attack      modifiers and the benefits of cover until the start of his next turn."
           "PG 62 EO"
  , mkTalent "Indomitable Conviction"
           "Shared Destiny, Strength through Conviction, Resistance (Fear), Jaded"
           (p <| \_ -> False) -- TODO What is Shared Destiny?
           [Leadership, StatApt Will]
           3
           "Character or an ally within 10 metres gains Insanity, reduce the amount by 1 to      a min      of 0. "
           "PG 57 EI"
  , mkTalent "Infused Knowledge"
           "Int 40, Lore (anyone)"
           (statThresh intelligence 40
             |> sAnd (.talents >> Dict.keys >> List.any (String.contains "Lore"))
             |> p
           )
           [StatApt Int, Knowledge]
           3
           "counts as having all Common Lore and Scholastic Lore skills at rank 1 (Known).    "
           "PG 128 CB"
  , mkTalent "Instrument of His Will"
           "WP 50"
           (p <| statThresh willpower 50)
           [Offence, StatApt Will]
           3
           "After making a successful attack against a Daemon (this can include striking it      with      a psychic power), the character may spend a Fate point to increase the damage of      the first hit he inflicts as part of that attack by an amount equal to twice his      StatApt Will bonus. This additional damage ignores armour and StatApt Tou bonus."
           "PG 61 EB"
  , mkTalent "Into the Jaws of Hell"
           "Adamantium Faith, Halo of Command, Will of the Inquisitor"
           (p <| \_ -> False) -- TODO What is Will of the Inquisitor?
           [Leadership, StatApt Will]
           3
           "Subtract the fellowship bonus from DoF for failed Fear or Pinning test."
           "PG 57 EI"
  , mkTalent "Iron Faith"
           "Iron Resolve"
           (p <| talentCheck "Iron Resolve")
           [Defence, StatApt Will]
           3
           "The character is immune to the effects of the Baneful Presence trait."
           "PG 61 EB"
  , mkTalent "Lightning Attack"
           "Swift Attack"
           (p <| talentCheck "Swift Attack")
           [StatApt WS, Finesse]
           3
           "Character may make many melee attacks with single roll."
           "PG 129 CB"
  , mkTalent "Luminen Blast"
           "Luminen Shock, Luminen Capacitors, Mechanicus Implants"
           (p <| talentCheck "Luminen Shock") -- should be enough
           [StatApt BS, Tech]
           3
           "Counts as being equipped with a single shot Pistol weapon with a 10m range and      deals      1d10 plus twice his StatApt Will bonus in Energy damage. Must pass a StatApt Tou test      or suffer 1 level of Fatigue after attack."
           "PG 129 CB"
  , mkTalent "Mastery (choose)"
           "Rank 4 in selected skill"
           (p <| \_ -> False) -- TODO needed parameter
           [StatApt Int, Knowledge]
           3
           "May spend Fate point to succeed on test if the final modifier to his skill test      is Challenging      (+0) or better, score a number of degrees of success equal to the characteristic      bonus.    "
           "PG 130 CB"
  , mkTalent "Mighty Shot"
           "BS 40"
           (p <| statThresh ballisticSkill 40)
           [StatApt BS, Offence]
           3
           "Add half BS (rounding up) bonus to ranged damage rolls."
           "PG 130 CB"
  , mkTalent "Never Die"
           "WP 50, T 50"
           (statThresh willpower 50
           |> sAnd (statThresh toughness 50)
           |> p
           )
           [StatApt Tou, Defence]
           3
           "Ignore penalties from Critical damage by spending Fate point. Once the encounter      ends,      the effects trigger as normal."
           "PG 130 CB"
  , mkTalent "Preternatural Speed"
           "WS 40, Ag 50"
           (statThresh weaponSkill 40
           |> sAnd (statThresh agility 50)
           |> p
           )
           [StatApt Ag, Offence]
           3
           "Double speed when charging."
           "PG 130 CB"
  , mkTalent "Push the Limit"
           "Rank 2 in Survival or any Operate skill, Tech-Use"
           (p <| \_ -> False) -- TODO Yet another riding skill
           [StatApt Per, Tech]
           3
           "Once per round, the character may add +20 to an Operate test however, if he      fails the      test by 4 or more degrees of failure, immediately roll 1d5 on Table 7-32: Motive      Systems Critical Hit effects and apply the result. If he is riding a living      mount,      roll 1d5 on Table 7-18: Impact Critical effects - Leg"
           "PG 63 EO"
  , mkTalent "Sanctic Purity"
           "Daemonologist, Favoured by the Warp, WP 50"
           (talentCheck "Daemonologist"
           |> sAnd (talentCheck "Favoured by the Warp") -- TODO it is not a talent
           |> sAnd (statThresh willpower 50)
           |> p
           )
           [Psyker, StatApt Will]
           3
           "When the character triggers Psychic Phenomena when manifesting a power from the      Sanctic      Daemonology discipline, he may spend a Fate point in order to negate the result      entirely."
           "PG 61 EB"
  , mkTalent "Shield Wall"
           "Ambidextrous, WS 40"
           (talentCheck "Ambidextrous"
           |> sAnd (statThresh weaponSkill 40)
           |> p
           )
           [Defence, StatApt WS]
           3
           "When armed with a shield, the character can re-roll one failed Evasion test to      Parry      an attack per round."
           "PG 61 EB"
  , mkTalent "Sprint"
           "-"
           (p <| \_ -> True)
           [StatApt Ag, Fieldcraft]
           3
           "Move more quickly in combat."
           "PG 131 CB"
  , mkTalent "Step Aside"
           "Ag 40, Dodge or Parry"
           (statThresh agility 40 |> sAnd (skillCheck "Dodge")
                                  |> sAnd (skillCheck "Parry")
                                  |> p
           )
           [StatApt Ag, Defence]
           3
           "Can make additional Dodge or Parry attempt per round."
           "PG 131 CB"
  , mkTalent "Superior Chirurgeon"
           "Rank 2 in Medicae skill"
           (p <| skillThresh "Medicae" 10)
           [StatApt Int, Fieldcraft]
           3
           "Gain +20 to Medicae tests, bonuses to first aid tests and only suffers a -10      penalty      for those suffering Critical damage."
           "PG 131 CB"
  , mkTalent "Target Selection"
           "BS 50"
           (p <| statThresh ballisticSkill 50)
           [StatApt BS, Finesse]
           3
           "May shoot into melee without penalty. Also an Aim action beforehand, prevents      any chance      of hitting friendly targets as well."
           "PG 132 CB"
  , mkTalent "Thunder Charge"
           "S 50"
           (p <| statThresh strength 50)
           [StatApt Str, Offence]
           3
           "Break enemies with armoured charge."
           "PG 132 CB"
  , mkTalent "True Grit"
           "T 40"
           (p <| statThresh toughness 40)
           [StatApt Tou, Defence]
           3
           "Reduce Critical damage taken."
           "PG 132 CB"
  , mkTalent "Two-Weapon Master"
           "Ag 45, Ambidextrous, BS 40 or WS 40, Two-Weapon Wielder (Melee, Ranged)"
           (p <| sAnd (statThresh agility 45)
              <| sAnd (talentCheck "Ambidextrous")
              <| sOr (statThresh ballisticSkill 40 |> sAnd (talentCheck "Two-Weapon Wielder (Ranged)"))
                     (statThresh weaponSkill 40 |> sAnd (talentCheck "Two-Weapon Wielder (Melee)"))
           )
           [Finesse, Offence]
           3
           "No penalties when fighting with two single handed weapons."
           "PG 132 CB"
  , mkTalent "Warp Lock"
           "Psy Rating, Strong Minded, WP 50"
           (psyThresh 0 |> sAnd (talentCheck "Strong Minded")
                        |> sAnd (statThresh willpower 50)
                        |> p
           )
           [StatApt Will, Psyker]
           3
           "Ignore Psychic Phenomenon once per session. Suffers 1d5 Energy damage to the      Head location      (not reduced by Armour or StatApt Tou) as a result, and cannot make any Focus      Power      tests or sustain other psychic powers until the beginning of his next turn.    "
           "PG 133 CB"
  , mkTalent "Weapon Intuition"
           "Exotic Weapon Training (Any)"
           (p <| \_ -> False) -- TODO Spec Talent Dependency
           [StatApt Int, Finesse]
           3
           "The character reduces the penalty for using a weapon without the proper training      by 10.    "
           "PG 63 EO"

  ]
