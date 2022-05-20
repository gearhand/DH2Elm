module Model exposing (..)

import Dict exposing (Dict)
import FieldLens exposing (FieldLens, get, modify, set)
import Maybe exposing (andThen)
import Selectize
import Set exposing (Set)
import Skills exposing (Skill)
import Stats exposing (Aptitude(..), StatName(..))
import String exposing (startsWith)
import Talents exposing (Talent)
import Util exposing (Selector, SelectorLens, SelectorMsg(..), applyM, menuLens, selectionLens)

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
  , traits: Set String
  , implants: Set String
  , psyRating: Int
  , madness: Int
  , corruption: Int
  }


payCost: Int -> Model -> Model
payCost cost model =
  { model | spentExp = model.spentExp + cost, freeExp = model.freeExp - cost }

refund: Int -> Model -> Model
refund cost model =
  { model | spentExp = model.spentExp - cost, freeExp = model.freeExp + cost }

skillUp: Model -> String -> Model
skillUp model sName =
  let skill = Dict.get sName model.skills
      cost  = andThen <| Skills.getUpCost model.aptitudes
      sAdj (s,lvl) m = { m | skills = Dict.insert sName (s, lvl + 10) m.skills }
      pay = Maybe.map payCost <| cost skill
      adj = Maybe.map sAdj skill
  in applyM pay (Just model) |> applyM adj |> Maybe.withDefault model

skillDown: Model -> String -> Model
skillDown model sName =
  let skill = Dict.get sName model.skills
      cost  = andThen <| Skills.getDownCost model.aptitudes
      sAdj (s,lvl) m = { m | skills = Dict.insert sName (s, lvl - 10) m.skills }
      pay = Maybe.map refund <| cost skill
      adj = Maybe.map sAdj skill
  in applyM pay (Just model) |> applyM adj |> Maybe.withDefault model

skillAdd: Model -> String -> Skill -> Model
skillAdd model sName skill =
  let skill_ = Dict.get sName model.skills
      cost  = Skills.getDownCost model.aptitudes (skill, 0) -- We need current lvl cost, so it's downCost function
      sAdj s m = { m | skills = Dict.insert sName (s, 0) m.skills, temp = Nothing }
      pay = Maybe.map payCost cost
      adj = Just (sAdj skill)
  in case skill_ of
    Just _ -> model
    Nothing -> applyM pay (Just model) |> applyM adj |> Maybe.withDefault model

skillRm: Model -> String -> Model
skillRm model sName =
  let skill = Dict.get sName model.skills
      cost  = andThen <| Skills.getDownCost model.aptitudes
      sAdj m = { m | skills = Dict.remove sName m.skills }
      pay = Maybe.map refund <| cost skill
      adj = Just sAdj
  in applyM pay (Just model) |> applyM adj |> Maybe.withDefault model

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

talentUpdate = update talentSelector

update : SelectorLens m s -> (s -> m -> m) -> SelectorMsg s -> m -> ( m, Cmd (SelectorMsg s))
update lens updater msg model =
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
                    update lens updater nextMsg newModel
                        |> \( model_, cmds ) -> ( model_ , Cmd.batch [ cmd, cmds ] )

                Nothing ->
                    ( newModel, cmd )

        SelectTree newSelection ->
          --( (FieldLens.compose lens selectionLens).set newSelection model |> updater newSelection
          --, Cmd.none )
          ( (FieldLens.compose lens selectionLens).set newSelection model
            |> case newSelection of
                    Just s -> updater s
                    Nothing -> identity
          , Cmd.none )

type alias TalentPrerequisite = Model -> Talent -> Bool
type TalPre = TalPre String (Model -> Bool) (List Aptitude)

talentList1 =
  let statThresh: ModelLens -> Int -> Model -> Bool
      statThresh sLens val = (<=) val << Stats.value << FieldLens.get sLens
      sAnd: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
      sAnd f1 f2 = \m -> f1 m && f2 m
      sOr f1 f2 = \m -> f1 m || f2 m
      talentCheck name = Dict.member name << .talents
      skillCheck name = Dict.member name << .skills
      skillThresh name value = .skills >> Dict.get name >> Maybe.map (\(_, v) -> v >= value) >> Maybe.withDefault False
      psyThresh value = \m -> m.psyRating >= value
  in
  [ TalPre "Ambidextrous"
           (statThresh agility 30)
           [StatApt WS, StatApt BS]
  , TalPre "Blind Fighting"
           (statThresh perception 30)
           [StatApt Per, Fieldcraft]
  , TalPre "Bodyguard"
           (statThresh agility 35)
           [StatApt Ag, Defence]
--
  , TalPre "Catfall"
           (statThresh agility 35)
           [StatApt Ag, Fieldcraft]
--
  , TalPre "Clues from the Crowds"
           (statThresh fellowship 30)
           [General, Social ]
--
  , TalPre "Die Hard"
           (statThresh willpower 30)
           [StatApt Will, Defence ]
--
  , TalPre "Disarm "
           (statThresh agility 30)
           [StatApt WS, Defence]
--
  , TalPre "Double Team"
           (\_ -> True)
           [General, Offence ]
--
  , TalPre "Enemy (choose)"
           (\_ -> True)
           [General, Social]
--
  , TalPre "Ferric Summons"
           (\m -> Set.member "Ferric Lure Implants" m.implants  && Set.member "Mechanicus Implants" m.traits )
           [StatApt Will, Tech]
--
  , TalPre "Flagellant"
           (statThresh willpower 30)
           [Offence, StatApt Tou]
--
  , TalPre "Frenzy"
           (\_ -> True)
           [StatApt Str, Offence]
--
  , TalPre "Grenadier"
           (statThresh ballisticSkill 35)
           [StatApt BS, Finesse]
--
  , TalPre "Iron Jaw"
           (statThresh toughness 40)
           [StatApt Tou, Defence]
--
  , TalPre "Jaded"
           (statThresh willpower 40)
           [StatApt Will, Defence]
--
  , TalPre "Keen Intuition"
           (statThresh intelligence 35)
           [StatApt Per, Social]
--
  , TalPre "Leap Up"
           (statThresh agility 30)
           [StatApt Ag, General]
--
  , TalPre "Leaping Dodge"
           --"Ag 35, Rank 2 in the Dodge skill"
           (sAnd (statThresh agility 35) (skillThresh "Dodge" 10))
           [StatApt Ag, Defence]
--
  --, TalPre "Mounted Warrior"
  --         --"Rank 2 (Trained) in any Operate skill or Rank 2(Trained) in Survival skill, BS 30 or WS 30"
  --         --TODO Read description in rulebook
  --         (\_ -> False)
  --         [StatApt WS / StatApt BS, Offence]
--
  , TalPre "Nowhere to Hide"
           (statThresh perception 30)
           [StatApt Per, Offence]
--
  , TalPre "Peer (choose)"
           (statThresh fellowship 30)
           [StatApt Fell, Social]
--
  , TalPre "Quick Draw"
           (\_ -> True)
           [StatApt Ag, Finesse]
--
  , TalPre "Rapid Reload"
           (\_ -> True)
           [StatApt Ag, Fieldcraft ]
--
  , TalPre "Resistance (choose)"
           (\_ -> True)
           [StatApt Tou, Defence]
--
  --, TalPre "Skilled Rider"
  --         "Rank 2 in any Operate skill "
  --         -- TODO read description in rulebook
  --         [StatApt Ag, Fieldcraft]
--
  , TalPre "Sound Constitution"
           (\_ -> True)
           [StatApt Tou, General]
--
  , TalPre "Takedown"
           (\_ -> True)
           [StatApt WS, Offence]
--
  , TalPre "Technical Knock"
           (statThresh intelligence 30)
           [StatApt Int, Tech]
--
  , TalPre "Warp Sense"
           --"Psy Rating, Psyniscience, Per 30 "
           ( sAnd (statThresh perception 30)
           <| sAnd (Dict.member "Psyniscience" << .skills)
           <| psyThresh 0
           )
           -- TODO make psy rating
           [StatApt Per, Psyker]
--
  , TalPre "Weapon Training (choose)"
           (\_ -> True)
           [General, Finesse]
--
  , TalPre "Weapon-Tech"
           --"Tech Use +10, Int 40"
           (sAnd (statThresh intelligence 40) (skillThresh "Dodge" 10))
           [StatApt Int, Tech]
--
  , TalPre "Ambassador Imperialis"
           --"Fellowship 35, Intelligence 35"
           (sAnd (statThresh fellowship 35) (statThresh intelligence 35))
           [StatApt Per, Social]
--
  , TalPre "Archivator"
           --"StatApt Int 40"
           (statThresh intelligence 40)
           [Knowledge, Social]
--
  , TalPre "Armor-Monger"
           --"Int 35, Tech-Use, Trade (Armourer)"
           ( sAnd (statThresh intelligence 35)
           <| sAnd (skillCheck "Tech-Use") (skillCheck "Trade (Armourer)")
           )
           [StatApt Int, Tech]
--
  , TalPre "Battle Rage"
           --"Frenzy"
           (talentCheck "Frenzy")
           [StatApt Str, Defence]
--
  , TalPre "Bulging Biceps"
           --"S 45"
           (statThresh strength 45)
           [StatApt Str, Offence]
--
  , TalPre "Bulwark of Faith"
           --"WP 45, Iron Faith"
           (sAnd (statThresh willpower 45) <| talentCheck "Iron Faith")
           [Defence, StatApt Will]
--
  , TalPre "Combat Master"
           --"WS 30"
           (statThresh weaponSkill 30)
           [StatApt WS, Defence]
--
  --, TalPre "Constant Vigilance (choose)"
  --         "Int 35 or Per 35, Awareness +10"
  --         -- TODO Spec TalPre with spec dependent prerequisite
  --         [StatApt Per, Defence]
--
  , TalPre "Contact Network"
           --"Cover-Up, Int 35"
           (sAnd (statThresh intelligence 35) <| talentCheck "Cover-Up")
           [StatApt Fell, Leadership]
--
  , TalPre "Coordinated Interrogation"
           --"S 40 or WP 40, Clues from the Crowds, Rank 1 (Known) Interrogation"
           (statThresh strength 40 |> sOr (statThresh willpower 40)
                               |> sAnd (talentCheck "Clues from the Crouds")
                               |> sAnd (skillCheck "Interrogation"))
           [StatApt Int, Social]
--
  , TalPre "Counter Attack"
           --"WS 40"
           (statThresh weaponSkill 40)
           [StatApt WS, Defence]
--
  , TalPre "Cover-Up"
           --"Int 35"
           (statThresh intelligence 35)
           [StatApt Int, Knowledge]
--
  , TalPre "Daemonhunter"
           --"Forbidden Lore (Daemonology), WP 40"
           (skillCheck "Forbidden Lore (Daemonology)" |> sAnd (statThresh willpower 40))
           [Offence, StatApt Will]
--
  , TalPre "Daemonologist"
           --"Psy rating 3, WP 45, Forbidden Lore (Daemonology)"
           (statThresh willpower 45 |> sAnd (skillCheck "Forbidden Lore (Daemonology)")
                                    |> sAnd (psyThresh 3))
           [Psyker, StatApt Will]
--
  , TalPre "Deny the Witch"
           (statThresh willpower 35)
           [StatApt Will, Defence]
--
  , TalPre "Devastating Assault"
           (statThresh weaponSkill 35)
           [StatApt WS, Offence]
--
  , TalPre "Double Tap"
           --"Two-Weapon Wielder (Ranged)"
           (talentCheck "Two-Weapon Wielder (Ranged)")
           [Finesse, Offence]
--
  , TalPre "Exotic Weapon Training" -- TODO Spec Talent
           (\_ -> True)
           [StatApt Int, Finesse]
--
  , TalPre "Face in a Crowd"
           --"Fel 35, Clues from the Crowds"
           (statThresh fellowship 35 |> sAnd (talentCheck "Clues from the Crowds"))
           [StatApt Fell, Social]
--
  --, TalPre "Field Vivisection" -- TODO Spec Talent
  --         --"BS or WS 40, Forbidden Lore (Xenos-Any), Rank 2 in the Medicae skill"
  --         ( statThresh ballisticSkill 40 |> sOr (statThresh weaponSkill 40)
  --         |> sAnd (skillThresh "Medicae" 10)
  --         |> sAnd (skillCheck "Forbidden Lore (Xenos)") -- TODO match forbidden lore
  --         )
  --         [StatApt BS/StatApt WS, Knowledge] -- TODO aptitudes
--
  , TalPre "Hard Target"
           (statThresh agility 40)
           [StatApt Ag, Defence]
--
  , TalPre "Harden Soul"
           --"WP 35, 10 Corruption points" -- TODO add corruption points
           (statThresh willpower 35 |> sAnd (\m -> m.corruption >= 10))
           [Defence, StatApt Will]
--
  , TalPre "Hardy"
           (statThresh toughness 40)
           [StatApt Tou, Defence]
--
  , TalPre "Hatred (choose)" -- TODO Spec talent
           (\_ -> True)
           [StatApt WS, Social]
--
  , TalPre "Hip Shooting"
           --"BS 40, Ag 40"
           (statThresh ballisticSkill 40 |> sAnd (statThresh agility 40))
           [StatApt BS, Finesse]
--
  --, TalPre "Hotshot Pilot" -- TODO: Spec Talent?
  --         " Rank 2 in Survival or any Operate skill, Ag 35 "
  --         (statThresh agility 35)
  --         [StatApt Ag, Tech]
--
  , TalPre "Independent Targeting"
           (statThresh ballisticSkill 40)
           [StatApt BS, Finesse]
--
  , TalPre "Inescapable Attack (Melee)"
           --"WS 40, Per 35"
           (statThresh weaponSkill 40 |> sAnd (statThresh perception 35))
           [StatApt WS , Finesse]

  , TalPre "Inescapable Attack (Ranged)"
           --"BS 40, Per 35"
           (statThresh ballisticSkill 40 |> sAnd (statThresh perception 35))
           [StatApt BS, Finesse]
--
  , TalPre "Inspiring Aura"
           --"Halo of Command"
           (talentCheck "Halo of Command")
           [Leadership, StatApt Will]
--
  , TalPre "Iron Resolve"
           --"Resistance (Fear), Jaded"
           (talentCheck "Resistance (Fear)" |> sAnd (talentCheck "Jaded"))
           [Defence, StatApt Will]
--
  , TalPre "Killing Strike"
           --"WS 50"
           (statThresh weaponSkill 50)
           [StatApt WS, Offence]
--
  , TalPre "Lexographer"
           --"Rank 3 in Linguistics (Any)"
           (\m -> Dict.foldl (\key (_, value) acc ->
                               if key |> startsWith "Linguistics" then max value acc
                                                                  else acc
                             )
                             0 m.skills >= 20
           )
           [StatApt Int, Knowledge]
--
  , TalPre "Luminen Shock"
           --"Luminen Capacitors, Mechanicus Implants"
           (\m -> Set.member "Luminen Capacitors" m.implants  && Set.member "Mechanicus Implants" m.traits )
           [StatApt WS, Tech]
--
  , TalPre "Maglev Transcendence"
           --"Maglev Coils, Mechanicus Implants"
           (\m -> Set.member "Maglev Coils" m.implants  && Set.member "Mechanicus Implants" m.traits )
           [StatApt Int, Tech]
--
  , TalPre "Marksman"
           (statThresh ballisticSkill 35)
           [StatApt BS, Finesse]
--
  , TalPre "Mechadendrite Use (choose)" -- TODO Spec Skill
           (\m -> Set.member "Mechanicus Implants" m.traits)
           [StatApt Int, Tech]
--
  , TalPre "One-on-One"
           (statThresh weaponSkill 40)
           [Finesse, StatApt WS]
--
  , TalPre "Penitent Psyker"
           --"Psy rating, Strong Minded, WP 40"
           (psyThresh 0 |> sAnd (talentCheck "Strong Minded")
                        |> sAnd (statThresh willpower 40)
           )
           [Psyker, Defence]
--
  , TalPre "Precision Killer (Ranged)"
           (statThresh ballisticSkill 40)
           [StatApt BS, Finesse]

  , TalPre "Precision Killer (Melee)"
           (statThresh weaponSkill 40)
           [StatApt WS, Finesse]
--
  , TalPre "Prosanguine"
           --"Auto Sanguine Implants, Mechanicus Implants"
           (\m -> Set.member "Auto Sanguine Implants" m.implants  && Set.member "Mechanicus Implants" m.traits )
           [StatApt Tou, Tech]
--
  , TalPre "Purity of Hatred"
           --"Hatred (Any)"
           (List.any (startsWith "Hatred") << Dict.keys << .talents  )
           [Offence, StatApt Will]
--
  , TalPre "Rites of Banishment"
           --"Common Lore (Imperial Creed) +10 or Forbidden Lore (Daemonology)"
           (skillThresh "Common Lore (Imperial Creed)" 10 |> sOr (skillCheck "Forbidden Lore (Daemonology)"))
           [Offence, StatApt Will]
--
  , TalPre "Strong Minded"
           --"WP 30, Resistance (Psychic Powers)"
           (statThresh willpower 30 |> sAnd (talentCheck "Resistance (Psychic Powers)"))
           [StatApt Will, Defence]
--
  , TalPre "Swift Attack"
           (statThresh weaponSkill 30)
           [StatApt WS, Finesse]
--
  , TalPre "Tainted Psyker"
           --"Psy rating, Rank 2 (Trained) in Psyniscience Skill, 10 Corruption points"
           (psyThresh 0 |> sAnd (skillThresh "Psyniscience" 10)
                        |> sAnd (\m -> m.corruption >= 10))
           [Knowledge, Psyker]
--
  , TalPre "Two-Weapon Wielder (Ranged)"
           (\_ -> True)
           [StatApt BS, Finesse]

  , TalPre "Two-Weapon Wielder (Melee)"
           (\_ -> True)
           [StatApt WS, Finesse]
--
  , TalPre "Unarmed Specialist"
           --"Ambidextrous, Ag 35, WS 35"
           (statThresh agility 35 |> sAnd (statThresh weaponSkill 35)
                                  |> sAnd (talentCheck "Ambidextrous")
           )
           [StatApt Str, Offence]
--
  , TalPre "Warp Conduit"
           --"Psy Rating, Strong Minded, WP 50"
           (psyThresh 0 |> sAnd (talentCheck "Strong Minded")
                        |> sAnd (statThresh willpower 50))
           [StatApt Will, Psyker]
--
  , TalPre "Whirlwind of Death"
           (statThresh weaponSkill 40)
           [StatApt WS, Finesse]
--
  , TalPre "Witch Finder"
           --"Rank 2 (Trained) in the Forbidden Lore (Psykers)skill, WP 45"
           (skillThresh "Forbidden Lore (Psykers)" 10 |> sAnd (statThresh willpower 45))
           [Knowledge, StatApt Per]
--
  --, TalPre "Xenosavant" -- TODO FUKKEN XENOS
  --         "Rank 3 in Forbidden Lore (Xenos-Any) "
  --         [StatApt Int, Knowledge]
--
  , TalPre "Adamantium Faith"
           --"Jaded, Resistance (Fear), WP 45"
           (talentCheck "Jaded" |> sAnd (talentCheck "Resistance (Fear)")
                                |> sAnd (statThresh willpower 45)
           )
           [StatApt Will, Defence]
--
  --, TalPre "Aegis of Contempt" -- TODO Read rulebook
  --         "Shared Destiny, Shield of Contempt, Hatred (any)"
  --         [Defence, Leadership]
--
  , TalPre "Assassin Strike"
           --"Ag 40, Acrobatics"
           (statThresh agility 40 |> sAnd (skillCheck "Acrobatics"))
           [StatApt WS, Fieldcraft]
--
  , TalPre "Bastion of Iron Will"
           --"Psy Rating, Strong Minded, WP 40"
           (psyThresh 0 |> sAnd (talentCheck "Strong Minded")
                        |> sAnd (statThresh willpower 40))
           [StatApt Will, Psyker]
--
  --, TalPre "Blademaster" -- TODO MELEE SPEC!!!
  --         "WS 30, Weapon Training (any Melee)"
  --         [StatApt WS, Finesse]
--
  , TalPre "Crushing Blow"
           (statThresh weaponSkill 40)
           [StatApt WS, Offence]
--
  --, TalPre "Daemonic Disruption" -- TODO Elite advance
  --         "Bane of the Daemon, WP 50, Untouchable elite advance"
  --         [StatApt Will, General]
--
  --, TalPre "Dark Soul" -- TODO Read rulebook
  --         "Hardened Soul, 20 Corruption points"
  --         [StatApt Tou, StatApt Will]
--
  , TalPre "Deathdealer (Ranged)"
           (statThresh ballisticSkill 45)
           [StatApt Per, Finesse]

  , TalPre "Deathdealer (Melee)"
           (statThresh weaponSkill 45)
           [StatApt Per, Finesse]
--
  , TalPre "Delicate Interrogation"
           --"Fel 50, Coordinated Interrogation"
           (statThresh fellowship 50 |> sAnd (talentCheck "Coordinated Interrogation"))
           [StatApt Int, Finesse]
--
  , TalPre "Divine Protection"
           (statThresh ballisticSkill 45 |> sAnd (statThresh willpower 35))
           [General, Finesse]
--
  , TalPre "Eye of Vengeance"
           (statThresh ballisticSkill 50)
           [StatApt BS, Offence]
--
  , TalPre "Favored by the Warp"
           (statThresh willpower 35)
           [StatApt Will, Psyker]
--
  , TalPre "Flash of Insight"
           --"Int 40, Contact Network, Coordinated Interrogation"
           (statThresh intelligence 40 |> sAnd (talentCheck "Contact Network")
                                       |> sAnd (talentCheck "Coordinated Interrogation")
           )
           [StatApt Per, Knowledge]
--
  , TalPre "Halo of Command"
           (statThresh fellowship 40 |> sAnd (statThresh willpower 40))
           [StatApt Fell, Leadership]
--
  , TalPre "Hammer Blow"
           (talentCheck "Crushing Blow")
           [StatApt Str, Offence]
--
  --, TalPre "Hull Down" -- TODO Read Rule book spec talent
  --         "Rank 2 in Survival or any Operate skill"
  --         [StatApt Ag, Fieldcraft]
--
  --, TalPre "Indomitable Conviction" -- TODO Read Rulebook
  --         "Shared Destiny, StatApt Str through Conviction, Resistance (Fear), Jaded"
  --         [Leadership, StatApt Will]
--
  , TalPre "Infused Knowledge"
           --"Int 40, Lore (anyone)"
           (statThresh intelligence 40
           |> sAnd (.talents >> Dict.keys >> List.any (String.contains "Lore")))
           [StatApt Int, Knowledge]
--
  , TalPre "Instrument of His Will"
           (statThresh willpower 50)
           [Offence, StatApt Will]
--
  --, TalPre "Into the Jaws of Hell"
  --         "Adamantium Faith, Halo of Command, Will of the Inquisitor" -- TODO WTF is will of the inquisitor
  --         [Leadership, StatApt Will]
--
  , TalPre "Iron Faith"
           (talentCheck "Iron Resolve")
           [Defence, StatApt Will]
--
  , TalPre "Lightning Attack"
           (talentCheck "Swift Attack")
           [StatApt WS, Finesse]
--
  , TalPre "Luminen Blast"
           --"Luminen Shock, Luminen Capacitors, Mechanicus Implants"
           (talentCheck "Luminen Shock") -- should be enough
           [StatApt BS, Tech]
--
  --, TalPre "Mastery (choose)" -- TODO HOLY FUCK
  --         "Rank 4 in selected skill"
  --         [StatApt Int, Knowledge]
--
  , TalPre "Mighty Shot"
           (statThresh ballisticSkill 40)
           [StatApt BS, Offence]
--
  , TalPre "Never Die"
           (statThresh willpower 50
           |> sAnd (statThresh toughness 50))
           [StatApt Tou, Defence]
--
  , TalPre "Preternatural Speed"
           (statThresh weaponSkill 40
           |> sAnd (statThresh agility 50))
           [StatApt Ag, Offence]
--
  --, TalPre "Push the Limit"
  --         "Rank 2 in Survival or any Operate skill, Tech-Use" -- TODO Rulebook
  --         [StatApt Per, Tech]
--
  , TalPre "Sanctic Purity"
           --"Daemonologist, Favoured by the Warp, WP 50"
           (talentCheck "Daemonologist"
           |> sAnd (talentCheck "Favoured by the Warp")
           |> sAnd (statThresh willpower 50))
           [Psyker, StatApt Will]
--
  , TalPre "Shield Wall"
           --"Ambidextrous, WS 40"
           (talentCheck "Ambidextrous"
           |> sAnd (statThresh weaponSkill 40))
           [Defence, StatApt WS]
--
  , TalPre "Sprint"
           (\_ -> True)
           [StatApt Ag, Fieldcraft]
--
  , TalPre "Step Aside"
           --"Ag 40, Dodge or Parry"
           (statThresh agility 40 |> sAnd (skillCheck "Dodge")
                                  |> sAnd (skillCheck "Parry")
           )
           [StatApt Ag, Defence]
--
  , TalPre "Superior Chirurgeon"
           --"Rank 2 in Medicae skill"
           (skillThresh "Medicae" 10)
           [StatApt Int, Fieldcraft]
--
  , TalPre "Target Selection"
           (statThresh ballisticSkill 50)
           [StatApt BS, Finesse]
--
  , TalPre "Thunder Charge"
           (statThresh strength 50)
           [StatApt Str, Offence]
--
  , TalPre "True Grit"
           (statThresh toughness 40)
           [StatApt Tou, Defence]
--
  , TalPre "Two-Weapon Master"
           --"Ag 45, Ambidextrous, BS 40 or WS 40, Two-Weapon Wielder (Melee, Ranged)"
           (sAnd (statThresh agility 45)
            <| sAnd (talentCheck "Ambidextrous")
            <| sOr (statThresh ballisticSkill 40 |> sAnd (talentCheck "Two-Weapon Wielder (Ranged)"))
                   (statThresh weaponSkill 40 |> sAnd (talentCheck "Two-Weapon Wielder (Melee)"))
           )
           [Finesse, Offence]
--
  , TalPre "Warp Lock"
           --"Psy Rating, Strong Minded, WP 50"
           (psyThresh 0 |> sAnd (talentCheck "Strong Minded") |> sAnd (statThresh willpower 50))
           [StatApt Will, Psyker]
--
  --, TalPre "Weapon Intuition"
  --         "Exotic Weapon Training (Any)" -- TODO Spec Talent Dependency
  --         [StatApt Int, Finesse]
  ]
