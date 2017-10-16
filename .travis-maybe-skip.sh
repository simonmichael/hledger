#!/bin/bash
# Maybe skip (terminate) this travis build, if it seems unnecessary.
# Emulates appveyor's more powerful skip/only feature.

set -e

CHANGED_FILES=`git diff --name-only master...${TRAVIS_COMMIT}`
  # TODO works only on master ? See eg this failure:
  # https://travis-ci.org/simonmichael/hledger/builds/288453508

# only files not matching this extended regex will trigger a build
#SKIP_PAT="(^site/con|do.nload)"
SKIP_PAT="\.md$"

# doesn't handle empty CHANGED_FILES correctly, shouldn't matter
SKIP=True
for F in $CHANGED_FILES; do
  if ! [[ $F =~ $SKIP_PAT ]]; then
    SKIP=False
    break
  fi
done

# standard travis helper, not in scope for some reason  
travis_terminate() {
  set +e
  pkill -9 -P $$ &> /dev/null || true
  exit $1
}

if [[ $SKIP == True ]]; then
  echo "Only skippable files found, exiting."
  travis_terminate 0
  exit 1
else
  echo "Non-skippable files found, continuing with build."
fi
