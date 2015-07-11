module PinyinApp where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp
import Dict exposing (Dict)
import List
import String
import Array
import Regex exposing (Regex)

main =
  StartApp.start
    { 
      model = init,
      update = update,
      view = view
    }


-- MODEL

type alias Sentence =
  {
    chinese : String,
    pinyin  : String
  }

type alias Model =
  { 
    currentSentenceIdx : Int,
    currentAnswer : List Int,
    workset : List Sentence
  }

currentSentence : Model -> Maybe Sentence
currentSentence model = (Array.get model.currentSentenceIdx (Array.fromList model.workset))

isCurrentAnswerComplete : Model -> Bool
isCurrentAnswerComplete model =
  let ms = (currentSentence model)
      currentSentenceLength = case ms of
                                Nothing -> 0
                                Just s -> (List.length (String.words s.pinyin))
  in List.length model.currentAnswer >= currentSentenceLength 

init : Model
init = 
  {
    currentSentenceIdx = 0,
    currentAnswer = [],
    workset = [
      {
        chinese = "先生，您貴姓？",
        pinyin  = "xīan shēng nín guì xìng"
      },
      {
        chinese = "他姓什麼？叫什麼名字？是哪國人？",
        pinyin  = "tā xìng shén me jiào shén me míng zi shì nǎ guó rén"
      },
      {
        chinese = "我是中國人，你是美國人，她呢？",
        pinyin  = "wǒ shì zhōng guó rén nǐ shì měi guó rén tā ne"
      },
      {
        chinese = "我姓王，不姓李，誰姓李？",
        pinyin  = "wǒ xìng wáng bú xìng lǐ shéi xìng lǐ"
      },
      {
        chinese = "王先生，你好，你是英國人嗎？",
        pinyin  = "wáng xiān sheng nǐ hǎo nǐ shì yīng guó rén ma"
      }
    ]
  }


-- CHARACTER MODEL

type alias CharacterModel = {
  chinese : String,
  pinyin  : Maybe String,
  answer  : Maybe Int,
  selected : Bool
}

containsRegex : String -> String -> Bool
containsRegex r w = Regex.contains (Regex.regex r) w

-- This is a helper method which removes punctuation from the input string and returns a list
-- of valid hanzi characters.  This is used to match the input string with the pinyin string, which does not
-- include the same punctuation.
-- e.g If we have the string "你好！你好嗎？", the output will be [0, 1, 3, 4, 5].
validHanziIndexList : String -> List Int
validHanziIndexList chinese =
  let indexedChineseSentenceList = List.indexedMap (,) (String.split "" chinese)
      filteredList = List.filter (\ (i, w) -> not (containsRegex "[，？。.,?a-zA-Z]" w)) indexedChineseSentenceList
  in  List.map (\ (i, w) -> i) filteredList

sentenceModel : Sentence -> List Int -> List CharacterModel
sentenceModel sentence answer = 
  let hanziIndexList = validHanziIndexList sentence.chinese
      hanziIndexAndPinyin = Dict.fromList (List.map2 (,) hanziIndexList (List.indexedMap (,) (String.words sentence.pinyin)))
      indexedAnswers = Dict.fromList (List.indexedMap (,) answer)
  in List.indexedMap (\ i c -> case Dict.get i hanziIndexAndPinyin of
                                 Nothing       -> { 
                                                    chinese  = c,
                                                    pinyin   = Nothing,
                                                    answer   = Nothing,
                                                    selected = False
                                                  }
                                 Just (idx, p) -> { 
                                                    chinese  = c,
                                                    pinyin   = Just p,
                                                    answer   = (Dict.get idx indexedAnswers),
                                                    selected = (List.length answer) == idx
                                                  }
                     ) (String.split "" sentence.chinese)

-- GRADE

type Grade = Correct | Incorrect | NotGraded | DoNotGrade

pinyinToTone : String -> Int
pinyinToTone word =
  if | containsRegex "[āēīōūǖĀĒĪŌŪǕ]" word  -> 1
     | containsRegex "[áéíóúǘÁÉÍÓÚǗ]" word  -> 2
     | containsRegex "[ǎěǐǒǔǚǍĚǏǑǓǙ]" word  -> 3
     | containsRegex "[àèìòùǜÀÈÌÒÙǛ]" word  -> 4
     | otherwise                            -> 0

grade : CharacterModel -> Grade
grade characterModel =
  case characterModel.pinyin of
    Nothing -> DoNotGrade
    Just p  -> case characterModel.answer of
                 Nothing -> NotGraded
                 Just a  -> if (pinyinToTone p) == a then Correct else Incorrect


-- UPDATE

type Action = InputPinyin Int | NoAction | NextSentence

update : Action -> Model -> Model
update action model =
  case action of
    NoAction          -> model
    NextSentence      -> updateNextSentence model
    InputPinyin tone  -> updatePinyin tone model

updatePinyin : Int -> Model -> Model
updatePinyin tone model =
  if isCurrentAnswerComplete model
  then model
  else {
         currentAnswer      = List.append model.currentAnswer [tone],
         workset            = model.workset,
         currentSentenceIdx = model.currentSentenceIdx
       }

updateNextSentence : Model -> Model
updateNextSentence model =
  if isCurrentAnswerComplete model
  then {
          currentAnswer      = [],
          workset            = model.workset,
          currentSentenceIdx = (model.currentSentenceIdx + 1) % (List.length model.workset)
        }
  else model

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let onKeyPress = Html.Events.onKeyDown address (\ i -> if  | i == 0x31 -> InputPinyin 1
                                                             | i == 0x32 -> InputPinyin 2
                                                             | i == 0x33 -> InputPinyin 3
                                                             | i == 0x34 -> InputPinyin 4
                                                             | i == 0x30 -> InputPinyin 0
                                                             | i == 0x20 || i == 0x2F -> NextSentence
                                                             | otherwise -> NoAction
                                                  )
  in div [ Html.Attributes.class "chineseSentence", onKeyPress, Html.Attributes.tabindex 0] (sentenceView model)

sentenceView : Model -> List Html
sentenceView model =
  let sentence = currentSentence model
      sModel = case sentence of
                  Nothing -> []  -- If we have an invalid index, we'll display nothing
                  Just s  -> sentenceModel s model.currentAnswer
  in List.map characterModelToHtml sModel

characterModelToHtml : CharacterModel -> Html
characterModelToHtml characterModel =
  let className = case grade characterModel of
                    Correct    -> "correct"
                    Incorrect  -> "incorrect"
                    NotGraded  -> "notGraded"
                    DoNotGrade -> "doNotGrade"
      classList = if characterModel.selected then [(className, True), ("selected", True)] else [(className, True)]
  in span [Html.Attributes.classList classList] [text characterModel.chinese]