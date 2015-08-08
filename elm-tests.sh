#!/usr/bin/env bash -e
elm-make --yes src/TestCharacterModel.elm --output raw-test.js
bash elm-io.sh raw-test.js test.js
iojs test.js
rm -f test.js raw-test.js
