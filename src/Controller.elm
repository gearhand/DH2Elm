module Controller exposing (..)


--import Charsheet exposing (statDown, statUp)
import Model exposing (Model, ModelLens)
import Skills exposing (Skill)
import Talents exposing (Talent)
import Util exposing (SelectorMsg)
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
--
--update: Msg -> Model -> (Model, Cmd Msg)
--update msg model =
--  case msg of
--    StatUp f -> (statUp model f, Cmd.none)
--    StatDown f -> (statDown model f, Cmd.none)
--    SkillUp sName -> (skillUp model sName, Cmd.none)
--    SkillDown sName -> (skillDown model sName, Cmd.none)
--    RmSkill sName -> (skillRm model sName, Cmd.none)
--    AddTemp tmp -> ({ model | temp = Just tmp }, Cmd.none)
--    AddSpec sName skill -> (skillAdd model sName skill, Cmd.none)
--    UpdateExp exp -> ({ model | freeExp = exp }, Cmd.none)
--    --TalentSelect selectMsg ->
--    --  let (newSelect, newCmd) = Talents.update selectMsg model.talentSelector
--    --   in ({model | talentSelector = newSelect}, Cmd.map TalentSelect newCmd)
--    TalentSelect selectMsg ->
--      let (newSelect, newCmd) = Talents.update (Debug.log "Event: " selectMsg) model.talentSelector
--       in case selectMsg of
--            Talents.SelectTree (Just talent) -> ({model | talentSelector = newSelect, talents = talent.name :: model.talents}, Cmd.map TalentSelect newCmd)
--            _ -> ({model | talentSelector = newSelect}, Cmd.map TalentSelect newCmd)
--    TalentSubmit ->
--      ( { model | talents =
--            case model.talentSelector.selection of
--              Just talent -> talent.name :: model.talents
--              Nothing -> model.talents
--        }
--      , Cmd.none
--      )
--    _ -> (model, Cmd.none)