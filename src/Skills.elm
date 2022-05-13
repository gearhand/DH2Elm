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

acrobatics = Skill "Acrobatics" "Акробатика" [StatApt Ag, General] Array.empty
athletics = Skill "Athletics" "Атлетика" [StatApt Str, General] Array.empty
awareness = Skill "Awareness" "Бдительность" [StatApt Per, Fieldcraft] Array.empty
security = Skill "Security" "Безопасность" [StatApt Int, Tech] Array.empty
survival = Skill "Survival" "Выживание" [StatApt Per, Fieldcraft] Array.empty
inquiry = Skill "Inquiry" "Дознание | Сбор сведений" [StatApt Fell, Social] Array.empty
interrogation = Skill "Interrogation" "Допрос" [StatApt Will, Social] Array.empty
intimidate = Skill "Intimidate" "Запугивание" [StatApt Str, Social] Array.empty
command = Skill "Command" "Командование" [StatApt Fell, Leadership] Array.empty
commerce = Skill "Commerce" "Коммерция" [StatApt Int, Knowledge] Array.empty
sleightOfHand = Skill "Sleight of Hand" "Ловкость рук" [StatApt Ag, Knowledge] Array.empty
logic = Skill "Logic" "Логика" [StatApt Int, Knowledge] Array.empty
medicae = Skill "Medicae" "Медицина" [StatApt Int, Fieldcraft] Array.empty
deceive = Skill "Deceive" "Обман" [StatApt Fell, Social] Array.empty
charm = Skill "Charm" "Обаяние" [StatApt Fell, Social] Array.empty
parry = Skill "Parry" "Парирование" [StatApt WS, Defence] Array.empty
scrutiny = Skill "Scrutiny" "Проницательность | Внимание к деталям" [StatApt Per, General] Array.empty
psyniscience = Skill "Psyniscience" "Псинистика | Пси-наука | Психонаука" [StatApt Per, Psyker] Array.empty
stealth = Skill "Stealth" "Скрытность" [StatApt Ag, Fieldcraft] Array.empty
techUse = Skill "Tech-Use" "Техпользование" [StatApt Int, Tech] Array.empty
dodge = Skill "Dodge" "Уклонение" [StatApt Ag, Defence] Array.empty
forbiddenLore = Skill "Forbidden Lore" "Запретные знания" [StatApt Int, Knowledge] forbiddenLoreSpecs
linguistics = Skill "Linguistics" "Лингвистика" [StatApt Int, General] linguisticsSpecs
navigate = Skill "Navigate" "Навигация" [StatApt Int, Fieldcraft] navigationSpecs
commonLore = Skill "Common Lore" "Общие знания | Обыденные знания" [StatApt Int, General] commonLoreSpecs
trade = Skill "Trade" "Ремесло" [StatApt Int, General] tradeSpecs
operate = Skill "Operate" "Управление" [StatApt Ag, Fieldcraft] operateSpecs
scholasticLore = Skill "Scholastic Lore" "Учёные знания" [StatApt Int, Knowledge] scholarLoreSpecs

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











