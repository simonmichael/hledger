# * project task scripts managed with https://github.com/casey/just.
# This hopes to gradually replace Makefile and bake.

@help:
    just -lu

@check:
    just --fmt --unstable --check

@fmt:
    just -q check || just --fmt --unstable

# ** hledger version numbers

# First 0-2 parts of a dotted version number.
@versionMajorPart VER:
    echo {{ VER }} | sed -E 's/([[:digit:]]+(\.[[:digit:]]+)?).*/\1/'

# Third part of a dotted version number, if any.
@versionMinorPart VER:
    echo {{ VER }} | sed -E -e 's/^[[:digit:]]+(\.[[:digit:]]+(\.)?)?//' -e 's/^([[:digit:]]+).*/\1/'

# Fourth part of a dotted version number, if any.
@versionFourthPart VER:
    echo {{ VER }} | sed -E -e 's/^[[:digit:]]+(\.[[:digit:]]+(\.[[:digit:]]+(\.)?))//' -e 's/^([[:digit:]]+).*/\1/'

# Does this dotted version number have a .99 third part and no fourth part ?
@versionIsDev VER:
    V={{ VER }}
    test "$(versionMinorPart "$V")" = 99 -a -z "$(versionFourthPart "$V")"

# Does this dotted version number have a .99 third part and a fourth part ?
@versionIsPreview VER:
    V={{ VER }}
    test "$(versionMinorPart "$V")" = 99 -a -n "$(versionFourthPart "$V")"

# Increment a major version number to the next.
@majorVersionIncrement VER:
    python3 -c "print($1 + 0.01)"

# Appropriate release branch name for the given version number.
@versionReleaseBranch VER:
    #!/bin/bash
    V={{ VER }}
    MAJOR=$(versionMajorPart "$V")
    if versionIsDev "$V"; then
      echo "$V is not a releasable version" >&2
      exit 1
    elif versionIsPreview "$V"; then
      echo "$(majorVersionIncrement "$MAJOR")-branch"
    else
      echo "$MAJOR-branch"
    fi
