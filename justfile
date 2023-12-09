# * project task scripts managed with https://github.com/casey/just.
# This aspires to gradually replace Makefile and bake.

@help:
    just -lu

@check:
    just --fmt --unstable --check

@fmt:
    just -q check || just --fmt --unstable

# ** releasing

# Symlink/copy important files temporarily in .relfiles/.
relfiles:
  #!/usr/bin/env bash
  echo "linking/copying important release files in .relfiles/ for convenient access..."
  mkdir -p .relfiles
  cd .relfiles
  for f in \
      ../stack.yaml \
      ../Shake.hs \
      ../hledger-install/hledger-install.sh \
      ../CHANGES.md \
      ../hledger/CHANGES.md \
      ../hledger-ui/CHANGES.md \
      ../hledger-web/CHANGES.md \
      ../hledger-lib/CHANGES.md \
      ../doc/github-release.md \
      ../doc/ANNOUNCE \
      ../doc/ANNOUNCE.masto \
      ../site/src/release-notes.md \
      ../site/src/install.md \
  ; do ln -sf $f .; done
  cp ../doc/RELEASING.md ./RELEASING2.md   # temp copy which can be edited without disruption

# Prepare to release today, creating/switching to release branch, updating versions, manuals, changelogs etc.
relprep VER:
  #!/usr/bin/env bash
  [[ -z {{VER}} ]] && usage
  BRANCH=$(just _versionReleaseBranch {{VER}})
  COMMIT="-c"
  echo "Switching to $BRANCH, auto-creating it if needed..."
  _gitSwitchAutoCreate "$BRANCH"
  echo "Bumping all version strings to {{VER}} ..."
  ./Shake setversion {{VER}} $COMMIT
  echo "Updating all command help texts for embedding..."
  ./Shake cmdhelp $COMMIT
  echo "Updating all dates in man pages..."
  ./Shake mandates
  echo "Generating all the manuals in all formats...."
  ./Shake manuals $COMMIT
  echo "Updating CHANGES.md files with latest commits..."
  ./Shake changelogs $COMMIT

# Push the current branch to github to generate release binaries.
@relbin:
  # assumes the github remote is named "github"
  git push -f github HEAD:binaries

# *** hledger version numbers
# See doc/RELEASING.md > Glossary.

# First 0-2 parts of a dotted version number.
@_versionMajorPart VER:
    echo {{ replace_regex(VER, '(\d+(\.\d+)?).*', '$1') }}

# Third part of a dotted version number, if any.
@_versionMinorPart VER:
    echo {{ if VER =~ '\d+(\.\d+){2,}' { replace_regex(VER, '\d+\.\d+\.(\d+).*', '$1') } else { '' } }}

# Fourth part of a dotted version number, if any.
@_versionFourthPart VER:
    echo {{ if VER =~ '\d+(\.\d+){3,}' { replace_regex(VER, '\d+(\.\d+){2}\.(\d+).*', '$2') } else { '' } }}

# Does this dotted version number have a .99 third part and no fourth part ?
@_versionIsDev VER:
    echo {{ if VER =~ '(\d+\.){2}99$' { 'y' } else { '' } }}

# Does this dotted version number have a .99 third part and a fourth part ?
@_versionIsPreview VER:
    echo {{ if VER =~ '(\d+\.){2}99\.\d+' { 'y' } else { '' } }}

# Increment a major version number to the next.
# @majorVersionIncrement MAJORVER:
#     python3 -c "print({{MAJORVER}} + 0.01)"

# Appropriate release branch name for the given version number.
_versionReleaseBranch VER:
    #!/usr/bin/env bash
    MAJOR=$(just _versionMajorPart {{ VER }})
    if [[ $(just _versionIsDev {{VER}}) == y ]] then
      echo "{{VER}} is not a releasable version" >&2
      exit 1
    elif [[ $(just _versionIsPreview {{VER}}) == y ]] then
      # echo "$(just majorVersionIncrement "$MAJOR")-branch"
      echo "{{VER}} is not a releasable version" >&2
      exit 1
    else
      echo "$MAJOR-branch"
    fi

# *** git

# Does the named branch exist in this git repo ?
@_gitBranchExists BRANCH:
  git branch -l {{BRANCH}} | grep -q {{BRANCH}}

# Switch to the named git branch, creating it if it doesn't exist.
_gitSwitchAutoCreate BRANCH:
  #!/usr/bin/env bash
  if just _gitBranchExists {{BRANCH}}; then
    git switch {{BRANCH}}
  else
    git switch -c {{BRANCH}}
  fi

# ** misc

# Show last week's activity, for TWIH
@lastweek:
  echo "hledger time last 7 days including today (this should be run on a Friday):"
  tt bal hledger -DTS -b '6 days ago' --drop 2
  echo
  echo "By activity type, percentage:"
  tt bal hledger -DTS -b '6 days ago' --pivot t -% -c 1% | tail +1
  echo
  echo "Time log details:"
  tt print hledger -b '6 days ago' | grep -E '^[^ ]|hledger'
  echo
  echo "main repo:"
  git log --format='%C(yellow)%cd %ad %Cred%h%Creset %s %Cgreen(%an)%Creset%C(bold blue)%d%Creset' --date=short --since="6 days ago" --reverse
  echo
  echo "site repo:"
  git -C site log --format='%C(yellow)%cd %ad %Cred%h%Creset %s %Cgreen(%an)%Creset%C(bold blue)%d%Creset' --date=short --since="6 days ago" --reverse
  echo
  echo "finance repo:"
  git -C finance log --format='%C(yellow)%cd %ad %Cred%h%Creset %s %Cgreen(%an)%Creset%C(bold blue)%d%Creset' --date=short --since="6 days ago" --reverse
  echo

# Show a bunch of debug messages.
@_dbgmsgs:
  rg --sort path -t hs 'dbg.*?(".*")' -r '$1' -o

# # Extract Hledger.hs examples to jargon.j.
# @_jargon:
#   rg '^ *> (.*)' -or '$1' hledger-lib/Hledger.hs > jargon.j
#   echo "wrote jargon.j"

# Extract ledger/hledger/beancount commit stats to project-commits.j.
# See also https://hledger.org/reporting-version-control-stats.html
@_projectcommits:
  printf "account ledger\naccount hledger\naccount beancount\n\n" >project-commits.j
  for p in ledger hledger beancount; do git -C ../$p log --format="%cd (%h) %s%n  ($p)  1%n" --date=short --reverse >> project-commits.j; done
  echo "wrote project-commits.j"

