/**
 *  This is a script to conver the workbook problems json file into an elm file.  It's difficult to integrate tasks
 *  with the current version of Elm-Test and IO, so instead I'm importing this file as an elm module and testing it
 *  that way.
 */
var fs = require('fs');

var workbookProblems = fs.readFileSync('workbook_problems.json', 'utf8');
var escaped_problems = JSON.stringify(workbookProblems);

var newFile = 
  "module WorkbookProblems (workbookProblems) where\n" +
  "\n" +
  "import CharacterModel exposing (WorkbookProblem, workbookDecoder)\n" +
  "import Json.Decode as JSD\n" +
  "\n" +
  "workbookProblems : List WorkbookProblem \n" +
  "workbookProblems = case (JSD.decodeString workbookDecoder " + escaped_problems + ") of\n" +
  "  Result.Ok a -> a\n" +
  "  Result.Err a -> []\n";

fs.writeFileSync('src/WorkbookProblems.elm', newFile);
