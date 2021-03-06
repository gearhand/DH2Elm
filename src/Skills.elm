module Skills exposing (Skill, filtered, logic, acrobatics, getUpCost, getDownCost)

import Array exposing (Array)
import Dict exposing (Dict)
import Stats exposing (Aptitude(..), StatName(..))

type alias Skill = { name: String
                   , localizedName: String
                   , aptitudes: List Aptitude
                   , specs: Array String
                   }

forbiddenLoreSpecs =
  Array.fromList
  [ "Archeotech"
  , "Chaos Marines"
  , "Criminal Organizations & Contraband"
  , "Demonology"
  , "Heresy"
  , "Horus heresy and Long War"
  , "Inquisition"
  , "Mutants"
  , "Officio Assasinorum"
  , "Pirates"
  , "Psykers"
  , "Warp"
  , "Xenos"
  ]

commonLoreSpecs =
  Array.fromList
  [ "Sororitas"
  , "Arbitres"
  , "Astra Telepatica"
  , "Mechanicus"
  , "Administratum"
  , "Sector Askellon" -- Sector name can change?
  , "Chartists captains"
  , "Titans"
  , "Ecclesiarchy"
  , "Imperial Creed"
  , "Astra Militarum"
  , "Imperial Fleet"
  , "Imperium"
  , "Navigators"
  , "Planetary Defence"
  , "Free Traders"
  , "Schola Progenum"
  , "Tech"
  , "Criminal"
  , "War history"
  ]

scholarLoreSpecs =
  Array.fromList
  [ "Astromancy|Astronomy"
  , "Beasts"
  , "Bureaucracy"
  , "Chemistry"
  , "Cryptology"
  , "Heraldic"
  , "Imperial Free Trader Patents"
  , "Justice"
  , "Legends"
  , "Numerology"
  , "Occultism"
  , "Philosophy"
  , "Tactica Imperialis"
  ]

operateSpecs =
  Array.fromList
  [ "Land"
  , "Air"
  , "Space"
  ]

tradeSpecs =
  Array.fromList
  [ "Agronomics"
  , "Archeology"
  , "Armourer"
  , "Weaponsmithing"
  , "Astrography"
  , "Chemistry"
  , "Cryptography"
  , "Cooking"
  , "Star Exploration"
  , "Linguistics"
  , "Mortikator"
  , "Performance"
  , "Geology"
  , "Carving"
  , "Sculpture"
  , "Spaceship Building"
  , "Farseeing"
  , "Technomancy"
  , "Void Travelling"
  ]

linguisticsSpecs =
  Array.fromList
  [ "Astartes Runes"
  , "Chaos Symbols"
  , "Aeldar"
  , "High Gothic"
  , "Imperial Codes"
  , "Low Gothic"
  , "Mercenaries Lang"
  , "Necrontir"
  , "Orkz"
  , "Tech-lingua"
  , "Tau"
  , "Criminals"
  , "Xenos marks"
  ]

navigationSpecs = Array.fromList [ "Land", "Space", "Warp" ]

acrobatics = Skill "Acrobatics" "????????????????????" [StatApt Ag, General] Array.empty
athletics = Skill "Athletics" "????????????????" [StatApt Str, General] Array.empty
awareness = Skill "Awareness" "????????????????????????" [StatApt Per, Fieldcraft] Array.empty
security = Skill "Security" "????????????????????????" [StatApt Int, Tech] Array.empty
survival = Skill "Survival" "??????????????????" [StatApt Per, Fieldcraft] Array.empty
inquiry = Skill "Inquiry" "???????????????? | ???????? ????????????????" [StatApt Fell, Social] Array.empty
interrogation = Skill "Interrogation" "????????????" [StatApt Will, Social] Array.empty
intimidate = Skill "Intimidate" "??????????????????????" [StatApt Str, Social] Array.empty
command = Skill "Command" "????????????????????????" [StatApt Fell, Leadership] Array.empty
commerce = Skill "Commerce" "??????????????????" [StatApt Int, Knowledge] Array.empty
sleightOfHand = Skill "Sleight of Hand" "???????????????? ??????" [StatApt Ag, Knowledge] Array.empty
logic = Skill "Logic" "????????????" [StatApt Int, Knowledge] Array.empty
medicae = Skill "Medicae" "????????????????" [StatApt Int, Fieldcraft] Array.empty
deceive = Skill "Deceive" "??????????" [StatApt Fell, Social] Array.empty
charm = Skill "Charm" "??????????????" [StatApt Fell, Social] Array.empty
parry = Skill "Parry" "??????????????????????" [StatApt WS, Defence] Array.empty
scrutiny = Skill "Scrutiny" "???????????????????????????????? | ???????????????? ?? ??????????????" [StatApt Per, General] Array.empty
psyniscience = Skill "Psyniscience" "???????????????????? | ??????-?????????? | ????????????????????" [StatApt Per, Psyker] Array.empty
stealth = Skill "Stealth" "????????????????????" [StatApt Ag, Fieldcraft] Array.empty
techUse = Skill "Tech-Use" "????????????????????????????" [StatApt Int, Tech] Array.empty
dodge = Skill "Dodge" "??????????????????" [StatApt Ag, Defence] Array.empty
forbiddenLore = Skill "Forbidden Lore" "?????????????????? ????????????" [StatApt Int, Knowledge] forbiddenLoreSpecs
linguistics = Skill "Linguistics" "??????????????????????" [StatApt Int, General] linguisticsSpecs
navigate = Skill "Navigate" "??????????????????" [StatApt Int, Fieldcraft] navigationSpecs
commonLore = Skill "Common Lore" "?????????? ???????????? | ?????????????????? ????????????" [StatApt Int, General] commonLoreSpecs
trade = Skill "Trade" "??????????????" [StatApt Int, General] tradeSpecs
operate = Skill "Operate" "????????????????????" [StatApt Ag, Fieldcraft] operateSpecs
scholasticLore = Skill "Scholastic Lore" "???????????? ????????????" [StatApt Int, Knowledge] scholarLoreSpecs

skillArray =
  Array.fromList
  [ acrobatics
  , athletics
  , awareness
  , security
  , survival
  , inquiry
  , interrogation
  , intimidate
  , command
  , commerce
  , sleightOfHand
  , logic
  , medicae
  , deceive
  , charm
  , parry
  , scrutiny
  , psyniscience
  , stealth
  , techUse
  , dodge
  ]

specSkillArray =
  Array.fromList
  [ forbiddenLore
  , linguistics
  , navigate
  , commonLore
  , trade
  , operate
  , scholasticLore
  ]

filtered: Dict String (Skill, Int) -> Array Skill
filtered skills = Array.append (Array.filter (\s -> not <| Dict.member s.name skills) skillArray) specSkillArray

skillProgression: Array (Array Int)
skillProgression =
  Array.fromList <| List.map Array.fromList
  [ [ 300, 600, 900, 1200 ]
  , [ 200, 400, 600, 800 ]
  , [ 100, 200, 300, 400 ]
  ]

getCost: (Int, Int) -> Maybe Int
getCost (apts, lvl) =
  Array.get apts skillProgression |> Maybe.andThen (Array.get (lvl // 10))

getUpCost: List Aptitude -> (Skill, Int) -> Maybe Int
getUpCost charApts (skill, lvl) =
  getCost (Stats.aptsCounter charApts skill.aptitudes, lvl + 10)

getDownCost: List Aptitude -> (Skill, Int) -> Maybe Int
getDownCost charApts (skill, lvl) =
  getCost (Stats.aptsCounter charApts skill.aptitudes, lvl)











