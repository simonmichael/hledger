#!/bin/bash
# Maybe skip (terminate) this travis build, if it seems unnecessary.
# Emulates appveyor's more powerful skip/only feature.

set -e

# works only on master ? 
CHANGED_FILES=`git diff --name-only master...${TRAVIS_COMMIT}`

# only files not matching this pattern will trigger a build
# extended regex, don't quote regex metachars
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

if [[ $SKIP == True ]]; then
  echo "Only skippable files found, exiting."
  travis_terminate 0
  exit 1
else
  echo "Non-skippable files found, continuing with build."
fi
