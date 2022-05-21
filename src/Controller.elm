module Controller exposing (..)


import Dict
import FieldLens
import Maybe exposing (andThen)
import Model exposing (Model, ModelLens, ModelPredicate(..), Talent, getTalentCost)
import Skills exposing (Skill)
import Stats
import Util exposing (SelectorMsg, applyM)
type Msg = StatUp ModelLens
         | StatDown ModelLens
         | RmSkill String
         | AddTemp Skill
         | AddSpec String Skill
         | SkillUp String
         | SkillDown String
         | UpdateExp Int
         | TalentSelect (SelectorMsg Talent)
         | TalentRemove Talent
         | Placeholder String
         | Empty

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

statUp: Model -> ModelLens -> Model
statUp model lens =
  let (val, _, apts) = FieldLens.get lens model
      aCnt = Stats.aptsCounter model.aptitudes apts
      cost = Stats.getCost aCnt val
      pay = Maybe.map payCost cost
  in applyM pay (Just model) |> Maybe.map (FieldLens.set lens (val+1)) |> Maybe.withDefault model

statDown: Model -> ModelLens -> Model
statDown model lens =
  let (val, _, apts) = FieldLens.get lens model
      aCnt = Stats.aptsCounter model.aptitudes apts
      cost = Stats.getCost aCnt (val - 1)
      pay = Maybe.map refund cost
  in applyM pay (Just model) |> Maybe.map (FieldLens.set lens (val - 1)) |> Maybe.withDefault model

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StatUp f -> (statUp model f, Cmd.none)
    StatDown f -> (statDown model f, Cmd.none)
    SkillUp sName -> (skillUp model sName, Cmd.none)
    SkillDown sName -> (skillDown model sName, Cmd.none)
    RmSkill sName -> (skillRm model sName, Cmd.none)
    AddTemp tmp -> ({ model | temp = Just tmp }, Cmd.none)
    AddSpec sName skill -> (skillAdd model sName skill, Cmd.none)
    UpdateExp exp -> ({ model | freeExp = exp }, Cmd.none)
    TalentSelect selectMsg ->
      let cost s m = getTalentCost ((Stats.aptsCounter m.aptitudes s.aptitudes), s.tier - 1)
          pay s m = case (cost s m) of
                      Just c -> payCost c m
                      Nothing -> m
          check (ModelPredicate p) m = p m
          updater s m =
            if check s.predicate m
               then { m | talents = Dict.insert s.name s m.talents } |> pay s
               else m
          (newModel, newCmd) = Model.talentUpdate updater selectMsg model
       in (newModel, Cmd.map TalentSelect newCmd)
    TalentRemove talent ->
      let cost s m = getTalentCost ((Stats.aptsCounter m.aptitudes s.aptitudes), s.tier - 1)
          ref s m = case (cost s m) of
                      Just c -> refund c m
                      Nothing -> m
       in ({ model | talents = Dict.remove talent.name model.talents } |> ref talent, Cmd.none)
    _ -> (model, Cmd.none)
