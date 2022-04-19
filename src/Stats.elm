module Stats exposing (..)

type StatName = WS
          | BS
          | Str
          | Tou
          | Ag
          | Int
          | Per
          | Will
          | Fell
          | Infl

type Aptitude =
    StatApt StatName
  | Defence
  | Fieldcraft
  | Finesse
  | General
  | Knowledge
  | Leadership
  | Offence
  | Psyker
  | Social
  | Tech

aptToString: Aptitude -> String
aptToString apt =
  case apt of
    StatApt stat -> fullName stat
    Defence -> "Defence"
    Fieldcraft -> "Fieldcraft"
    Finesse -> "Finesse"
    General -> "General"
    Knowledge -> "Knowledge"
    Leadership -> "Leadership"
    Offence -> "Offence"
    Psyker -> "Psyker"
    Social -> "Social"
    Tech -> "Tech"



fullName: StatName -> String
fullName s =
  case s of
    WS -> "Weapon Skill"
    BS -> "Ballistic Skill"
    Str -> "Strength"
    Tou -> "Toughness"
    Ag -> "Agility"
    Int -> "Intelligence"
    Per -> "Perception"
    Will -> "Willpower"
    Fell -> "Fellowship"
    Infl -> "Influence"

type alias Stat = (StatName, Int)