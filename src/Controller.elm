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
         | TalentSpecFill String
         | TalentSpec Talent
         | Placeholder String
         | Chained Msg Msg
         | Empty

payCost: Maybe Int -> Model -> Model
payCost mCost model =
  case mCost of
    Just cost -> { model | spentExp = model.spentExp + cost, freeExp = model.freeExp - cost }
    Nothing -> model

refund: Maybe Int -> Model -> Model
refund mCost model =
  case mCost of
    Just cost -> { model | spentExp = model.spentExp - cost, freeExp = model.freeExp + cost }
    Nothing -> model

noCmd model = (model, Cmd.none)

skillUp: Model -> String -> Model
skillUp model sName =
  let skill = Dict.get sName model.skills
      cost  = andThen <| Skills.getUpCost model.aptitudes
      sAdj (s,lvl) m = { m | skills = Dict.insert sName (s, lvl + 10) m.skills }
      pay = payCost <| cost skill
      adj = Maybe.map sAdj skill |> Maybe.withDefault identity
  in pay model |> adj

skillDown: Model -> String -> Model
skillDown model sName =
  let skill = Dict.get sName model.skills
      cost  = andThen <| Skills.getDownCost model.aptitudes
      sAdj (s,lvl) m = { m | skills = Dict.insert sName (s, lvl - 10) m.skills }
      pay = refund <| cost skill
      adj = Maybe.map sAdj skill |> Maybe.withDefault identity
  in pay model |> adj

skillAdd: Model -> String -> Skill -> Model
skillAdd model sName skill =
  let skill_ = Dict.get sName model.skills
      cost  = Skills.getDownCost model.aptitudes (skill, 0) -- We need current lvl cost, so it's downCost function
      sAdj s m = { m | skills = Dict.insert sName (s, 0) m.skills, temp = Nothing }
      pay = payCost cost
      adj = sAdj skill
  in case skill_ of
    Just _ -> model
    Nothing -> pay model |> adj

skillRm: Model -> String -> Model
skillRm model sName =
  let skill = Dict.get sName model.skills
      cost  = andThen <| Skills.getDownCost model.aptitudes
      sAdj m = { m | skills = Dict.remove sName m.skills }
      pay = refund <| cost skill
      adj = sAdj
  in pay model |> adj

statUp: Model -> ModelLens -> Model
statUp model lens =
  let (val, _, apts) = FieldLens.get lens model
      aCnt = Stats.aptsCounter model.aptitudes apts
      cost = Stats.getCost aCnt val
      pay = payCost cost
  in pay model |> FieldLens.set lens (val+1)

statDown: Model -> ModelLens -> Model
statDown model lens =
  let (val, _, apts) = FieldLens.get lens model
      aCnt = Stats.aptsCounter model.aptitudes apts
      cost = Stats.getCost aCnt (val - 1)
      pay = refund cost
  in pay model |> FieldLens.set lens (val - 1)


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let talentCost s m = getTalentCost ((Stats.aptsCounter m.aptitudes s.aptitudes), s.tier - 1)
  in
  case Debug.log "Update: " msg of
    StatUp f -> statUp model f |> noCmd
    StatDown f -> statDown model f |> noCmd
    SkillUp sName -> skillUp model sName |> noCmd
    SkillDown sName -> skillDown model sName |> noCmd
    RmSkill sName -> skillRm model sName |> noCmd
    AddTemp tmp -> { model | temp = Just tmp } |> noCmd
    AddSpec sName skill -> skillAdd model sName skill |> noCmd
    UpdateExp exp -> { model | freeExp = exp } |> noCmd
    TalentSpecFill txt -> { model | talentSpecInput = txt } |> noCmd
    TalentSelect selectMsg ->
      let pay s m = payCost (talentCost s m) m
          check (ModelPredicate p) m = p m
          updater s m =
            if check s.predicate m
               then { m | talents = Dict.insert s.name s m.talents }
                    |> case s.spec of
                         Nothing -> pay s
                         Just _ -> identity
               else m
          (newModel, newCmd) = Model.talentUpdate updater selectMsg model
       in (newModel, Cmd.map TalentSelect newCmd)
    TalentRemove talent ->
      let ref s m = refund (talentCost s m) m
       in { model | talents = Dict.remove talent.name model.talents } |> ref talent |> noCmd
    TalentSpec talent ->
      let talentModified = { talent | spec = Nothing, name = talent.name ++ " (" ++ model.talentSpecInput ++ ")" }
          pay s m = payCost (talentCost s m) m
       in case Dict.get talentModified.name model.talents of
            Nothing -> { model | talents =
                               Dict.insert talentModified.name talentModified
                               <| Dict.remove talent.name model.talents
                       } |> pay talent |> noCmd
            Just _ -> { model | talents = Dict.remove talent.name model.talents } |> noCmd
    Chained msg1 msg2 ->
      let (model1, cmd1) = update msg1 model
          (model2, cmd2) = update msg2 model1
       in (model2, Cmd.batch [ cmd1, cmd2 ])
    _ -> model |> noCmd
