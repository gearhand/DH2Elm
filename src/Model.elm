module Model exposing (..)

import Dict exposing (Dict)
import Maybe exposing (andThen)
import Skills exposing (Skill)
import Stats exposing (Aptitude)
import Talents
import Util exposing (applyM)

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
  , talentSelector: Talents.Selector
  , talents: List String
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
