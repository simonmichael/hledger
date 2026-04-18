#!/bin/sh
# Compile the haskell tools in this directory.

cd "$(dirname "$0")" || exit
for f in [a-z]*.hs; do
    printf "compiling $f.. "
    stack script --resolver nightly-2026-04-17 --optimize --no-run -- "$f"
done
