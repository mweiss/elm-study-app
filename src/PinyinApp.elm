module PinyinApp where

import CharacterModel exposing (CharacterModel, Sentence, workbookProblemToSentence, sentenceModel, grade)
import WorkbookProblems exposing (workbookProblems)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import StartApp exposing (LoopbackFun)
import Signal exposing (Signal)
import Task exposing (Task)
import Date exposing (Date)
import Dict exposing (Dict)
import Time exposing (Time)
import LocalStorage
import Window
import Results exposing (Results, CharacterResult, resultsToString, resultsFromString, shouldReviewSentence)
import ListUtils exposing (find, forceTail)
-- The main app loop... Note that there may be a way to remove
-- some of this boiler plate bullshit like the port and external actions
-- being empty for now.

main =
  fst viewsAndTasks

port requests : Signal (Task LocalStorage.Error ())
port requests = snd viewsAndTasks

externalActions : Signal Action
externalActions = Signal.map (\x -> DimensionChange x) Window.dimensions

viewsAndTasks =
  StartApp.start
    { initialState = init
    , update = update
    , view = view
    }
    externalActions

-- MODEL

type alias Lesson =
  { sentences : List Sentence
  , currentSentence : Maybe (List (CharacterModel String))
  , currentSentenceIdx : Int
  }

type alias AppModel =
  { lesson : Lesson
  , results : Results
  , loadState : LoadState
  }

type LoadState = NotLoaded | Loading | Loaded
type Action = InputPinyin String | NoAction | NextSentence | NextCharacter | LoadResults Results | DimensionChange (Int, Int)

selectFirst : List (CharacterModel answer) -> List (CharacterModel answer)
selectFirst l = case List.head l of
  Nothing -> l
  Just v  -> {v | selected <- True} :: (forceTail l)

updateLesson : Int -> Lesson -> Lesson
updateLesson i l = 
  let selectedSentence = List.head (List.drop i l.sentences)
      cs = case selectedSentence of
        Nothing -> Nothing
        Just a -> Just (selectFirst (sentenceModel a))
  in  {l | currentSentence <- cs, currentSentenceIdx <- i}

-- TODO: for now, I'm going to initialize this with all the sentences and no results,
-- but I need to be smarter about how it's done
init : AppModel
init =
  { lesson = updateLesson 0
    { sentences = List.map workbookProblemToSentence workbookProblems
    , currentSentence = Nothing
    , currentSentenceIdx = 0
    }
  , results = { words = Dict.empty }
  , loadState = NotLoaded
  }

transformStorageToAction : Maybe Results -> Task LocalStorage.Error Action
transformStorageToAction r = 
  case r of
    Nothing -> Task.succeed (LoadResults { words = Dict.empty })
    Just a -> Task.succeed (LoadResults a)

initResults : Task LocalStorage.Error Action
initResults = (LocalStorage.getJson Results.resultsDecoder "results") `Task.andThen` transformStorageToAction

-- UPDATE
update : LoopbackFun LocalStorage.Error Action -> Time.Time -> Action -> AppModel -> (AppModel, (Maybe (Task LocalStorage.Error ())))
update loopback t action model =
  case model.loadState of
    NotLoaded -> ({model | loadState <- Loading}, Just (loopback initResults))
    Loading -> case action of
      LoadResults r ->
        ( { model | loadState <- Loaded,
                    results <- r,
                    lesson <- updateLesson (findNextLessonSentenceIndex model.lesson r t) model.lesson
          }
        , Nothing
        )
      _ -> ({ model | loadState <- Loaded}, Nothing)
    Loaded -> case action of
      InputPinyin s -> (updateInputPinyin s model, Nothing)
      NextCharacter -> let newModel = tryToMoveToNextCharacter model t
        in (newModel, Just (loopback <| saveResults newModel.results))
      _ -> (model, Nothing)

saveResults : Results -> Task LocalStorage.Error Action
saveResults r =
  (LocalStorage.set "results" (Results.resultsToString r)) `Task.andThen` transformSaveToAction

transformSaveToAction : String -> Task LocalStorage.Error Action
transformSaveToAction s = Task.succeed NoAction

-- grade the current answer
-- record if it's correct or incorrect
tryToMoveToNextCharacter : AppModel -> Time.Time -> AppModel
tryToMoveToNextCharacter model time =
  case model.lesson.currentSentence of
    Nothing -> model
    Just currentSentence ->
      let lesson = model.lesson
          results = model.results
          currentSentenceWithAnswer = addAnswerToSentence currentSentence
          lessonWithAnswer = { lesson | currentSentence <- Just currentSentenceWithAnswer }
          characterResult = gradeSentence currentSentenceWithAnswer time
          updatedResults = if not (alreadyAnswered currentSentenceWithAnswer) then updateResults results characterResult else results
          updatedLesson = if isCorrect characterResult then (moveToNextCharacter lessonWithAnswer updatedResults time) else lessonWithAnswer
      in { model | lesson <- updatedLesson, results <- updatedResults}

alreadyAnswered : List (CharacterModel answer) -> Bool
alreadyAnswered sentence =
  case List.head (List.filter (\x -> x.selected) sentence) of
    Nothing -> True
    Just cm -> List.length cm.answers > 1

isCorrect : Maybe (String, CharacterResult) -> Bool
isCorrect cr = case cr of
  Nothing -> False
  Just (_ , v) -> v.correct

moveToNextCharacter : Lesson -> Results -> Time.Time -> Lesson
moveToNextCharacter lesson results time =
  case lesson.currentSentence of
    Nothing -> lesson
    Just currentSentence ->
      let selectedIndex = List.foldr 
            (\c i -> if (snd c).selected then (fst c) else i)
            (List.length currentSentence)
            (List.indexedMap (,) currentSentence)
          deselectedSegment = List.map (\x -> {x | selected <- False}) (List.take (selectedIndex + 1) currentSentence)
          selectedSegment = snd <| List.foldl
            (\cm n -> 
              let beenSelected = (fst n)
                  soFar = (snd n)
                  hasPinyin = case cm.pinyin of
                    Nothing -> False
                    Just _ -> True
              in  if (not beenSelected) && hasPinyin 
                  then (True,  soFar ++ [{cm | selected <- True}])
                  else (beenSelected, soFar ++ [{cm | selected <- False}])
            )
            (False, [])
            (List.drop (selectedIndex + 1) currentSentence)
          fullSentence = deselectedSegment ++ selectedSegment
      in if not (List.isEmpty (List.filter (\x -> x.selected) fullSentence))
         then {lesson | currentSentence <- Just fullSentence}
         else updateLesson (findNextLessonSentenceIndex lesson results time) lesson

findNextLessonSentenceIndex : Lesson -> Results -> Time.Time -> Int
findNextLessonSentenceIndex lesson results time =
  let nextIndex = (lesson.currentSentenceIdx + 1) % (List.length lesson.sentences)
      rotatedSentenceList = (List.drop nextIndex lesson.sentences) ++ (List.take nextIndex lesson.sentences)
      indexOffset = fst <| List.foldr
        (\sentence indexes ->
          case indexes of
            (-1, a) ->
              if shouldReviewSentence (sentenceModel sentence) results (Date.fromTime time)
                then (a, a + 1)
                else (-1, a + 1)
            (_, _)  -> indexes
        )
        (-1, 0)
        rotatedSentenceList
      nextValidIndex = indexOffset + nextIndex
  in  if nextValidIndex == -1 then nextIndex else nextValidIndex 

updateResults : Results -> Maybe (String, CharacterResult) -> Results
updateResults results cr =
  case cr of
    Nothing -> results
    Just result -> 
      let updateFunc = (\old -> case old of
        Nothing -> Just ([(snd result)])
        Just oldValue -> Just ((snd result) :: oldValue)
      )
      in { results | words <- Dict.update (fst result) updateFunc results.words }

addAnswerToSentence : List (CharacterModel String) -> List (CharacterModel String)
addAnswerToSentence sentence = List.map 
  (\cm -> if cm.selected 
    then case cm.currentAnswer of
      Nothing -> {cm | answers <- "" :: cm.answers}
      Just a  -> {cm | answers <- a :: cm.answers, currentAnswer <- Nothing}
    else cm)
  sentence

gradeSentence : List (CharacterModel String) -> Time.Time  -> Maybe (String, CharacterResult)
gradeSentence sentence time = List.foldr
  (\cm tuple -> 
    if cm.selected 
    then Just (cm.chinese, { date = Date.fromTime time, correct = (grade cm) == CharacterModel.Correct })
    else tuple
  )
  Nothing
  sentence

updateInputPinyin : String -> AppModel -> AppModel
updateInputPinyin input model =
  let lesson = model.lesson
  in { lesson = { lesson | currentSentence <- updateCurrentSentence input lesson.currentSentence}
     , results = model.results
     , loadState = model.loadState
     }

updateInput : String -> List (CharacterModel String) -> List (CharacterModel String)
updateInput input sentence =
  let h = List.head sentence
  in  case h of
    Nothing -> []
    Just v  -> 
      if v.selected
      then ({v | currentAnswer <- Just input}) :: (forceTail sentence)
      else v :: (updateInput input (forceTail sentence))

updateCurrentSentence : String -> Maybe (List (CharacterModel String)) -> Maybe (List (CharacterModel String))
updateCurrentSentence input sentence =
  case sentence of
    Nothing -> Nothing
    Just s  -> Just (updateInput input s)

-- VIEW

view : Signal.Address Action -> AppModel -> Html
view address model =
  case model.loadState of
    NotLoaded -> Html.text "not loaded"
    Loading -> Html.text "loading..."
    Loaded ->
      case model.lesson.currentSentence of
        Nothing -> Html.text "Invalid sentence"
        Just currentSentence -> 
          Html.div [] 
            [ sentenceView address currentSentence
            , inputView address currentSentence
            -- , debugView address model
            ]

debugView : Signal.Address Action -> AppModel -> Html
debugView address model = Html.div [] [Html.text (toString (findNextLessonSentenceIndex model.lesson model.results 0, model.results))]

sentenceView : Signal.Address Action -> List (CharacterModel String) -> Html
sentenceView address model = Html.div [] <| List.map characterView model

characterView : CharacterModel String -> Html
characterView cm = 
  let className = case grade cm of
        CharacterModel.Correct -> "correct"
        CharacterModel.Incorrect -> "incorrect"
        CharacterModel.NotGraded -> "notGraded"
        CharacterModel.DoNotGrade -> "doNotGrade"
      classList = if cm.selected then [(className, True), ("selected", True)] else [(className, True)]
      pinyin = if not <| List.isEmpty cm.answers 
        then case cm.pinyin of 
          Nothing -> ""
          Just p -> p
        else ""
  in  Html.div 
        [ Html.Attributes.class "sentenceView"] 
        [ Html.div [Html.Attributes.class "pinyin"] [Html.text pinyin]
        , Html.div [Html.Attributes.classList classList] [Html.text cm.chinese]
        ]

inputView : Signal.Address Action -> List (CharacterModel String) -> Html
inputView address model = 
  let changeHandler = Html.Events.on "input" Html.Events.targetValue (\str -> Signal.message address (InputPinyin str))
      enterHandler  = Html.Events.onKeyDown address (\i -> if i == 13 then NextCharacter else NoAction)
      currentAnswer = case find (\x -> x.selected) model of
        Nothing -> "not found"
        Just char -> case char.currentAnswer of
          Nothing -> ""
          Just a -> a
  in  Html.input [changeHandler, enterHandler, Html.Attributes.autofocus True, Html.Attributes.value currentAnswer] []


