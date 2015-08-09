module Results (Results, CharacterResult, resultsToString, resultsFromString, resultsDecoder, determineDateToReviewHanzi, shouldReviewSentence) where

import CharacterModel exposing (CharacterModel, Sentence, workbookProblemToSentence, sentenceModel, grade)
import Json.Decode as JSD exposing ((:=), decodeString)
import Json.Encode as JSE 
import Date exposing (Date)
import Dict exposing (Dict)
import List exposing (map)
import Time exposing (Time)

type alias Results = { words : Dict String (List CharacterResult) }

type ReviewGroup = NotSeen | ReviewNow | ReviewInOneDay | ReviewInThreeDays | ReviewInOneWeek | ReviewInThirtyDays

type alias CharacterResult =
  { date : Date
  , correct : Bool
  }

-- Results serializing / deserializing
resultsToString : Results -> String
resultsToString results =
  let words = JSE.object (map (\x -> (fst x, JSE.list (map characterResultsToString (snd x)))) (Dict.toList results.words))
      fullResult = JSE.object [("words", words)]
  in JSE.encode 0 fullResult

characterResultsToString : CharacterResult -> JSE.Value
characterResultsToString cr =
  JSE.object [("date", JSE.float (Date.toTime cr.date)), ("correct", JSE.bool cr.correct)]

resultsDecoder : JSD.Decoder Results
resultsDecoder =
  let characterResults = JSD.object2 
        CharacterResult
          (JSD.customDecoder ("date" := JSD.float) (\v -> Result.Ok (Date.fromTime v)))
          ("correct" := JSD.bool)
  in JSD.object1 Results ("words" := JSD.dict (JSD.list characterResults))

resultsFromString : String -> Results
resultsFromString results =
  case decodeString resultsDecoder results of
    Result.Err _ -> { words = Dict.empty }
    Result.Ok v -> v

-- General tuple utility for retrieving the third value of a four tuple
thd : (a, b, c, d) -> c
thd v = case v of
  (_, _, r, _) -> r

day : Time
day = Time.hour * 24

foldCharacterResults : CharacterResult -> (Maybe Date, Maybe Date, Int, Bool) -> (Maybe Date, Maybe Date, Int, Bool)
foldCharacterResults cr currentTpl =
  case currentTpl of
    (Just minDate, Just maxDate, a, False) ->
      if cr.correct
        then (Just cr.date, Just maxDate, a + 1, False)
        else (Just minDate, Just maxDate, a, True) 
    (Nothing, Nothing, a, False) -> 
      if cr.correct
        then (Just cr.date, Just cr.date, a + 1, False)
        else (Nothing, Nothing, a, True)
    (_, _, _, True) -> currentTpl
            
-- Returns the review group and the last date the question was answered correctly, or Nothing if the question
-- hasn't been answered or the last answer was incorrect.
determineReviewGroupByHanzi : String -> Results -> (Maybe Date, ReviewGroup)
determineReviewGroupByHanzi c results = 
  case Dict.get c results.words of
    Nothing -> (Nothing, NotSeen)
    Just cresults -> 
      let dateRangeAndCountTpl = List.foldr 
            foldCharacterResults
            (Nothing, Nothing, 0, False)
            (List.sortWith (\a b -> compare (Date.toTime a.date) (Date.toTime b.date)) cresults)
      in  case dateRangeAndCountTpl of
            (Nothing, Nothing, _, _) -> (Nothing, ReviewNow)
            (Just minDate, Just maxDate, consecutiveCorrect, _) ->
              let timeDifference = (Date.toTime maxDate) - (Date.toTime minDate)
                  reviewGroup = 
                    if | timeDifference < day || consecutiveCorrect <= 1 -> ReviewInOneDay
                       | timeDifference < (3 * day) || consecutiveCorrect <= 2 -> ReviewInThreeDays
                       | timeDifference < (7 * day) || consecutiveCorrect <= 3 -> ReviewInOneWeek
                       | otherwise -> ReviewInThirtyDays
              in (Just maxDate, reviewGroup)

determineDateToReviewHanzi : String -> Results -> Maybe Date
determineDateToReviewHanzi c results =
  case determineReviewGroupByHanzi c results of
    (Nothing, _) -> Nothing
    (Just latestDate, reviewGroup) ->
      let days = case reviewGroup of
            ReviewInOneDay -> day
            ReviewInThreeDays -> day * 3
            ReviewInOneWeek -> day * 7
            ReviewInThirtyDays -> day * 30
      in  Just (Date.fromTime ((Date.toTime latestDate) + days))


foldDatesForReview : Results -> (CharacterModel a) -> Maybe Date -> Maybe Date
foldDatesForReview results cm md =
  case md of
    Nothing -> Nothing
    Just minDate ->
      case cm.pinyin of 
        Nothing -> Just minDate
        Just _ -> 
          case determineDateToReviewHanzi cm.chinese results of
            Nothing -> Nothing
            Just date -> Just (Date.fromTime (min (Date.toTime minDate) (Date.toTime date)))
    
determineDateToReviewSentence : List (CharacterModel a) -> Results -> Maybe Date
determineDateToReviewSentence sentence results =
  List.foldr (foldDatesForReview results) (Just (Date.fromTime (day * 365 * 10000))) sentence

shouldReviewSentence : List (CharacterModel a) -> Results -> Date -> Bool
shouldReviewSentence sentence results now =
  case determineDateToReviewSentence sentence results of
    Nothing -> True
    Just date -> (Date.toTime date) <= (Date.toTime now)
