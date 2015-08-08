module Main where

import CharacterModel exposing (WorkbookProblem, workbookDecoder, grade, inputToPinyin)
import WorkbookProblems exposing (workbookProblems)
import List
import String
import Html
import Regex exposing (Regex)

import ElmTest.Assertion as A
import ElmTest.Run as R
import ElmTest.Runner.Console exposing (runDisplay)
import ElmTest.Test exposing (..)

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response)
import IO.Runner as Run

import Task exposing (Task)
import Http exposing (Error)

testHttp : Task Error (List WorkbookProblem)
testHttp = Http.get workbookDecoder "workbook_problems.json"

tests : List Test
tests = [ test "Empty list" (A.assertEqual [] (CharacterModel.tokenizePinyin ""))
        , test "Single element" (A.assertEqual ["wǒ"] (CharacterModel.tokenizePinyin "wǒ"))
        , test "Single element with punctuation" (A.assertEqual ["wǒ", "."] (CharacterModel.tokenizePinyin "wǒ."))
        , test "Simple sentence" 
            (A.assertEqual
              ["Xiān", "shēng", ",", "nín", "guì", "xìng", "?"]
              (CharacterModel.tokenizePinyin "Xiānshēng, nín guìxìng?")
            )
        , test "A larger sentence"
            (A.assertEqual
              ["Wǒ", "shì", "zhōng", "guó", "rén", ",", "nǐ", "shì", "měi", "guó", "rén", ",", "tā", "ne", "?"]
              (CharacterModel.tokenizePinyin "Wǒ shì zhōngguórén, nǐ shì měiguó rén, tā ne?"))
        , test "A test to make sure that 'a' gets parsed"
            (A.assertEqual
              ["Zǎo", ",", "zhào", "xiǎo", "jiě", ",", "qù", "shàng", "kè", "a", "?"]
              (CharacterModel.tokenizePinyin "Zǎo, zhào xiǎojiě, qù shàngkè a?"))
        ] ++ workbookProblemTests ++ inputToPinyinTests ++ gradingTests

workbookProblemTests : List Test
workbookProblemTests = List.map convertWorkbookProblemsToTest workbookProblems

inputToPinyinTests : List Test
inputToPinyinTests =
  [ test "Simple input to pinyin test" (A.assertEqual "xiān" (inputToPinyin  "xian1"))
  ]
gradingTests : List Test
gradingTests =
  [ test "Simple grading test" 
      (A.assertEqual 
        CharacterModel.Correct 
        (grade {chinese = "先", pinyin = Just "Xiān", answers = ["xian1"], selected = False, currentAnswer = Nothing}))
  ]

convertWorkbookProblemsToTest : WorkbookProblem -> Test
convertWorkbookProblemsToTest workbookProblem =
  let sentence = {chinese = workbookProblem.chinese, pinyin = workbookProblem.pinyin}
      sentenceModel = CharacterModel.sentenceModel sentence
      chineseSplit = (String.split "" sentence.chinese)
  in  test ("Test sentence " ++ workbookProblem.chinese ++ ", " ++ workbookProblem.pinyin)
        (A.assertEqual (List.length sentenceModel) (List.length chineseSplit))

console = runDisplay <| Suite "All Tests" tests

port requests : Signal Request
port requests = Run.run responses console

port responses : Signal Response
