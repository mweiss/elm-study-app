module PinyinApp where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import StartApp
import Dict exposing (Dict)
import List
import String
import Regex exposing (Regex)

main =
  StartApp.start
    { 
      model = init,
      update = update,
      view = view
    }


-- MODEL

type alias Model =
  { 
    sentenceChinese : String,
    sentencePinyin : String,
    answersSoFar : List Int
  }

init : Model
init = 
  {
    sentenceChinese = "先生，您貴姓？",
    sentencePinyin = "xīan shēng nín guì xìng",
    answersSoFar = [1, 1, 2, 3]
  }

-- UPDATE

type Action = Increment | Decrement

update : Action -> Model -> Model
update action model = model


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [ Html.Attributes.class "chineseSentence" ] (chineseSentenceView model)

containsRegex : String -> String -> Bool
containsRegex r w = Regex.contains (Regex.regex r) w

pinyinToTone : String -> Int
pinyinToTone word =
  if | containsRegex "[āēīōūǖĀĒĪŌŪǕ]" word  -> 1
     | containsRegex "[áéíóúǘÁÉÍÓÚǗ]" word  -> 2
     | containsRegex "[ǎěǐǒǔǚǍĚǏǑǓǙ]" word  -> 3
     | containsRegex "[àèìòùǜÀÈÌÒÙǛ]" word  -> 4
     | otherwise                            -> 0

-- This is a helper method which removes punctuation from the input string and returns a list
-- of valid hanzi characters.  This is used to match the input string with the pinyin string, which does not
-- include the same punctuation.
-- e.g If we have the string "你好！你好嗎？", the output will be [0, 1, 3, 4, 5].
validHanziIndexList : String -> List Int
validHanziIndexList sentenceChinese =
  let indexedChineseSentenceList = (List.indexedMap (,) (String.split "" sentenceChinese))
      filteredList = List.filter (\ (i, w) -> not (containsRegex "[，？。.,?]" w)) indexedChineseSentenceList
  in  List.map (\ (i, w) -> i) filteredList

-- This is a helper method which takes a dictionary of indexes to corrections, and returns a list of
-- characters to their corrections.  True is correct, False is wrong, and Nothing means it either
-- can't be graded or hasn't been graded yet.
correctedChineseSentence : Dict Int (Maybe Bool) -> String -> List (String, Maybe Bool)
correctedChineseSentence correctAnswers sentencePinyin = 
  List.indexedMap (findCorrectedAnswer correctAnswers) (String.split "" sentencePinyin)

-- This annotates each character with whether it's correct (Just True), false (Just False), or not
-- corrected yet.
findCorrectedAnswer : Dict Int (Maybe Bool) -> Int -> String -> (String, Maybe Bool)
findCorrectedAnswer dict i str =
  let maybe = Dict.get i dict
  in case maybe of
       Nothing           -> (str, Nothing)
       Just Nothing      -> (str, Nothing)
       Just (Just True)  -> (str, Just True)
       Just (Just False) -> (str, Just False)

htmlForCharacterCorrections : (String, Maybe Bool) -> Html
htmlForCharacterCorrections (s, maybe) =
  let className = case maybe of
                    Nothing    -> "nothing"
                    Just True  -> "correct"
                    Just False -> "wrong"
  in span [Html.Attributes.class className] [text s]

correctedAnswersSoFar : Model -> Dict Int Bool
correctedAnswersSoFar model =
  let pinyinWords = String.words model.sentencePinyin
      pinyinTones = List.map pinyinToTone pinyinWords
      correctAnswers = List.map2 (\ a b -> a == b) model.answersSoFar pinyinTones
  in Dict.fromList (List.indexedMap (,) correctAnswers)

chineseSentenceView : Model -> List Html
chineseSentenceView model =
  let correctedAnswersSoFarDict = correctedAnswersSoFar model
      hanziIndexList = validHanziIndexList model.sentenceChinese
      hanziIndexCorrectionList = List.indexedMap (\ i p -> (p, Dict.get i correctedAnswersSoFarDict)) hanziIndexList
      hanziIndexCorrectectionDict = Dict.fromList hanziIndexCorrectionList
      characterCorrectionList = correctedChineseSentence hanziIndexCorrectectionDict model.sentenceChinese
  in  List.map htmlForCharacterCorrections characterCorrectionList


