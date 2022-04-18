module Skills exposing (..)

import Array exposing (Array)
import FieldLens exposing (FieldLens)
import Stats exposing (Aptitude(..), StatName(..))
import Array.NonEmpty

type alias Skill = { name: String
                   , localizedName: String
                   , aptitudes: List Aptitude
                   , specs: List String
                   }

forbiddenLoreSpecs =
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
  [ "Land"
  , "Air"
  , "Space"
  ]

tradeSpecs =
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

navigationSpecs = [ "Land", "Space", "Warp" ]

acrobatics = Skill "Acrobatics" "Акробатика" [StatApt Ag, General] []
athletics = Skill "Athletics" "Атлетика" [StatApt Str, General] []
awareness = Skill "Awareness" "Бдительность" [StatApt Per, Fieldcraft] []
security = Skill "Security" "Безопасность" [StatApt Int, Tech] []
survival = Skill "Survival" "Выживание" [StatApt Per, Fieldcraft] []
inquiry = Skill "Inquiry" "Дознание | Сбор сведений" [StatApt Fell, Social] []
interrogation = Skill "Interrogation" "Допрос" [StatApt Will, Social] []
forbiddenLore = Skill "Forbidden Lore" "Запретные знания" [StatApt Int, Knowledge] forbiddenLoreSpecs
intimidate = Skill "Intimidate" "Запугивание" [StatApt Str, Social] []
command = Skill "Command" "Командование" [StatApt Fell, Leadership] []
commerce = Skill "Commerce" "Коммерция" [StatApt Int, Knowledge] []
linguistics = Skill "Linguistics" "Лингвистика" [StatApt Int, General] linguisticsSpecs
sleightOfHand = Skill "Sleight of Hand" "Ловкость рук" [StatApt Ag, Knowledge] []
logic = Skill "Logic" "Логика" [StatApt Int, Knowledge] []
medicae = Skill "Medicae" "Медицина" [StatApt Int, Fieldcraft] []
navigate = Skill "Navigate" "Навигация" [StatApt Int, Fieldcraft] navigationSpecs
deceive = Skill "Deceive" "Обман" [StatApt Fell, Social] []
charm = Skill "Charm" "Обаяние" [StatApt Fell, Social] []
commonLore = Skill "Common Lore" "Общие знания | Обыденные знания" [StatApt Int, General] commonLoreSpecs
parry = Skill "Parry" "Парирование" [StatApt WS, Defence] []
scrutiny = Skill "Scrutiny" "Проницательность | Внимание к деталям" [StatApt Per, General] []
psyniscience = Skill "Psyniscience" "Псинистика | Пси-наука | Психонаука" [StatApt Per, Psyker] []
trade = Skill "Trade" "Ремесло" [StatApt Int, General] tradeSpecs
stealth = Skill "Stealth" "Скрытность" [StatApt Ag, Fieldcraft] []
techUse = Skill "Tech-Use" "Техпользование" [StatApt Int, Tech] []
dodge = Skill "Dodge" "Уклонение" [StatApt Ag, Defence] []
operate = Skill "Operate" "Управление" [StatApt Ag, Fieldcraft] operateSpecs
scholasticLore = Skill "Scholastic Lore" "Учёные знания" [StatApt Int, Knowledge] scholarLoreSpecs

skillArray_ =
  [ acrobatics
  , athletics
  , awareness
  , security
  , survival
  , inquiry
  , interrogation
  , forbiddenLore
  , intimidate
  , command
  , commerce
  , linguistics
  , sleightOfHand
  , logic
  , medicae
  , navigate
  , deceive
  , charm
  , commonLore
  , parry
  , scrutiny
  , psyniscience
  , trade
  , stealth
  , techUse
  , dodge
  , operate
  , scholasticLore
  ]

skillArray =
  case skillArray_ of
    (head::tail) -> List.foldl (\v a -> Array.NonEmpty.push v a) (Array.NonEmpty.fromElement head) tail
    [] -> Debug.todo "Skill array creation error"

get idx = Array.NonEmpty.getSelected <| Array.NonEmpty.setSelectedIndex idx skillArray

skillProgression: Array (Array Int)
skillProgression =
  Array.fromList <| List.map Array.fromList
  [ [ 300, 600, 900, 1200 ]
  , [ 200, 400, 600, 800 ]
  , [ 100, 200, 300, 400 ]
  ]

getCost: (Int, Int) -> Int
getCost (apts, lvl) =
  case Array.get apts skillProgression |> Maybe.andThen (Array.get lvl) of
    Just x -> x
    Nothing -> Debug.todo "Cannot get cost"













