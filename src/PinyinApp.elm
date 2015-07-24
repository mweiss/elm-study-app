module PinyinApp where


-- The main app loop... Note that there may be a way to remove
-- some of this boiler plate bullshit like the port and external actions
-- being empty for now.

main =
  fst viewsAndTasks

port requests : Signal (Task Error ())
port requests = snd viewsAndTasks

externalActions : Signal Action
externalActions = Signal.constant NoAction

viewsAndTasks =
  StartApp.start
    { 
      initialState = init,
      update = update,
      view = view
    }
    externalActions

-- MODEL


type alias Sentence =
  {
    chinese : String,
    pinyin  : String
  }

type alias Lesson =
  {
    sentences : List Sentence,
    currentSentence : Idx
  }
type alias AppModel =
  {
  
  } 


type alias CharacterModel a = {
  chinese  : String,
  pinyin   : Maybe String,
  answers  : List a,
  selected : Bool
}
