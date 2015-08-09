module Results (Results, CharacterResult, resultsToString, resultsFromString, resultsDecoder) where

import Json.Decode as JSD exposing ((:=), decodeString)
import Json.Encode as JSE 
import Date exposing (Date)
import Dict exposing (Dict)
import List exposing (map)

type alias Results = { words : Dict String (List CharacterResult) }

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