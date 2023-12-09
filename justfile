#!/usr/bin/env just
# * Light project scripts, without file dependendencies
# using https://github.com/casey/just 0.16.
# https://docs.rs/regex/1.5.4/regex/#syntax Regexps
# https://just.systems/man/en/chapter_31.html Functions
# https://cheatography.com/linux-china/cheat-sheets/justfile Cheatsheet
# https://github.com/casey/just/discussions
# See also Makefile, Shake.hs.


# ** prelude

@help:
    just -lu

@_check:
    just --fmt --unstable --check

@_fmt:
    just -q _check || just --fmt --unstable


# ** vars

# GHC-compiled executables require a locale (and not just C) or they
# will die on encountering non-ascii data. Set LANG to something if not already set.
# export LANG? := 'en_US.UTF-8'

# command to run during profiling (time and heap)
PROFCMD := 'stack exec --profile -- hledger balance -f examples/10000x1000x10.journal >/dev/null'

#PROFRTSFLAGS := '-p'
PROFRTSFLAGS := '-P'

# # command to run during "make coverage"
# COVCMD := 'test'
# COVCMD := '-f test-wf.csv print'

# misc. system tools
BROWSE := 'open'
# VIEWHTML := '{{ BROWSE }}'
# VIEWPDF := '{{ BROWSE }}'
# PRINT := 'lpr'

#GHC := 'ghc'
GHCI := 'ghci' #-package ghc-datasize #-package ghc-heap-view
# GHCPKG := 'ghc-pkg'
# HADDOCK := 'haddock'
# CABAL := 'cabal'
# CABALINSTALL := 'cabal install -w {{ GHC }}'

# Which stack command (and in particular, stack yaml/GHC version) to use for building etc. ?
STACK := 'stack'
#STACK := 'stack --stack-yaml=stack8.10.yaml'
# Or override temporarily with an env var:
# STACK := '"stack --stack-yaml=stack8.10.yaml" make functest'

# Which stack command (stack yaml, GHC version) to use for ghci[d] operations ?
STACKGHCI := STACK
#STACKGHCI := 'stack --stack-yaml=stack9.2.yaml'

# if using an unreleased stack with a newer hpack than the one mentioned in */*.cabal,
# it will give warnings. To silence these, put the old hpack-X.Y in $PATH and uncomment:
#STACK := 'stack --with-hpack=hpack-0.20'

# --threads := '16 sometimes gives "commitAndReleaseBuffer: resource vanished (Broken pipe)" but seems harmless'
# --timeout := 'N is not much use here - can be defeated by multiple threads, unoptimised builds, '
# slow hackage index or compiler setup on first build, etc.
SHELLTESTOPTS := '--execdir --threads=64 --exclude=/_'

# make sure shelltest is a released version of shelltestrunner
# run shell tests using the executable specified in tests
# SHELLTEST := 'COLUMNS=80 PATH=~/.local/bin:/usr/bin:/bin shelltest {{ SHELLTESTOPTS }}'
# run shell tests using the stack build of hledger
#SHELLTESTSTK := 'shelltest -w `stack exec which hledger` {{ SHELLTESTOPTS }}'
SHELLTESTSTK := 'COLUMNS=80 ' + STACK + ' exec -- shelltest ' + SHELLTESTOPTS

WATCHEXEC := 'watchexec'

PACKAGES := '\
    hledger-lib \
    hledger \
    hledger-ui \
    hledger-web \
    '

BINARIES := '\
    hledger \
    hledger-ui \
    hledger-web
    '

INCLUDEPATHS := '\
    -ihledger-lib \
    -ihledger \
    -ihledger-ui \
    -ihledger-web \
    -ihledger-web/app \
    '

MAIN := 'hledger/app/hledger-cli.hs'

# All source files in the project (plus a few strays like Setup.hs & hlint.hs).
# Used eg for building tags. Doesn't reliably catch all source files.
SOURCEFILES := '\
    dev.hs                    \
    hledger/*hs               \
    hledger/app/*hs           \
    hledger/bench/*hs         \
    hledger/test/*hs          \
    hledger/Hledger/*hs       \
    hledger/Hledger/*/*hs     \
    hledger/Hledger/*/*/*hs   \
    hledger-*/*hs             \
    hledger-*/app/*hs         \
    hledger-*/test/*hs        \
    hledger-*/Hledger/*hs     \
    hledger-*/Hledger/*/*hs   \
    hledger-*/Hledger/*/*/*hs \
    hledger-lib/Text/*/*hs    \
    '
#    hledger-*/src/*hs         \

HPACKFILES := '\
    hledger/*package.yaml \
    hledger-*/*package.yaml \
    '

CABALFILES := '\
    hledger/hledger.cabal \
    hledger-*/*.cabal \
    '

MANUALSOURCEFILES := '\
    doc/common.m4 \
    */*.m4.md \
    '

MANUALGENFILES := '\
    hledger*/hledger*.{1,5,info,txt} \
    '

COMMANDHELPFILES := '\
    hledger/Hledger/Cli/Commands/*.md \
    '

WEBTEMPLATEFILES := '\
    hledger-web/templates/* \
    '

WEBCODEFILES := '\
    hledger-web/static/*.js \
    hledger-web/static/*.css \
    '

DOCSOURCEFILES := '\
    README.md \
    CONTRIBUTING.md' \
    + MANUALSOURCEFILES \
    + COMMANDHELPFILES

TESTFILES := '\
    hledger/test/*.test \
    hledger/test/*/*.test \
    hledger/test/*/*/*.test \
    '

# # file(s) which require recompilation for a build to have an up-to-date version string
# VERSIONSOURCEFILE := 'hledger/Hledger/Cli/Version.hs'

# Two or three-part version string, set as program version in builds made by this makefile.
# We use hledger CLI's current version (XXX for all packages, which isn't quite right).
export VERSION := `cat hledger/.version`

# Flags for ghc builds.

# Warnings to see during dev tasks like make ghci*. See also the warnings in package.yamls.
# XXX redundant with package.yamls ?
WARNINGS := '\
    -Wall \
    -Wno-incomplete-uni-patterns \
    -Wno-missing-signatures \
    -Wno-orphans \
    -Wno-type-defaults \
    -Wno-unused-do-bind \
    '

# if you have need to try building in less memory
GHCLOWMEMFLAGS := '' # '+RTS -M200m -RTS'

# ghc-only builds need the macro definitions generated by cabal
# from cabal's dist or dist-sandbox dir, hopefully there's just one:
#CABALMACROSFLAGS := '-optP-include -optP hledger/dist*/build/autogen/cabal_macros.h'
# or from stack's dist dir:
#CABALMACROSFLAGS := '-optP-include -optP hledger/.stack-work/dist/*/*/build/autogen/cabal_macros.h'
CABALMACROSFLAGS := ''

BUILDFLAGS := \
    '-rtsopts ' \
    + WARNINGS \
    + GHCLOWMEMFLAGS \
    + CABALMACROSFLAGS \
    + ' -DDEVELOPMENT' \
    + ' -DVERSION=\"'+VERSION+'\"' \
    + INCLUDEPATHS \

#    -fplugin Debug.Breakpoint \
#    -fhide-source-paths \

# PROFBUILDFLAGS := '-prof -fprof-auto -osuf hs_p'

TIME := "{{ shell date +'%Y%m%d%H%M' }}"
MONTHYEAR := "{{ shell date +'%B %Y' }}"


# ** misc

# sym-link some directories required by hledger-web dev builds
mkwebdirs:
    echo "#ln -sf hledger-web/config  # disabled, causes makeinfo warnings"
    ln -sf hledger-web/messages
    ln -sf hledger-web/static
    ln -sf hledger-web/templates


# ** GHCI

# run GHCI on hledger-lib + hledger
@ghci:
    {{ STACKGHCI }} exec -- {{ GHCI }} {{ BUILDFLAGS }} hledger/Hledger/Cli.hs

# run GHCI on hledger-lib + hledger with profiling/call stack information
@ghci-prof:
    stack build --profile hledger --only-dependencies
    {{ STACKGHCI }} exec -- {{ GHCI }} {{ BUILDFLAGS }} -fexternal-interpreter -prof -fprof-auto hledger/Hledger/Cli.hs

# # run GHCI on hledger-lib + hledger + dev.hs script
# @ghci-dev:
#     {{ STACKGHCI }} exec -- {{ GHCI }} {{ BUILDFLAGS }} -fno-warn-unused-imports -fno-warn-unused-binds dev.hs

# run GHCI on hledger-lib + hledger + hledger-ui
@ghci-ui:
    {{ STACKGHCI }} exec -- {{ GHCI }} {{ BUILDFLAGS }} hledger-ui/Hledger/UI/Main.hs

# run GHCI on hledger-lib + hledger + hledger-web
@ghci-web:
    {{ STACKGHCI }} exec -- {{ GHCI }} {{ BUILDFLAGS }} hledger-web/app/main.hs

# run GHCI on hledger-lib + hledger + hledger-web + hledger-web test suite
@ghci-web-test:
    {{ STACKGHCI }} exec -- {{ GHCI }} {{ BUILDFLAGS }} hledger-web/test/test.hs

# # better than stack exec ?
# # XXX does not see changes to files
# # run GHCI on hledger-lib + test runner
# ghci-lib-test:
#     {{ STACKGHCI }} ghci --ghc-options="\'-rtsopts {{ WARNINGS }} -ihledger-lib  -DDEVELOPMENT -DVERSION=\"1.26.99\"\'" hledger-lib/test/unittest.hs

# run GHCI on all the hledger
# ghci-all:
#     {{ STACK }} exec -- {{ GHCI }} {{ BUILDFLAGS }} \
#         hledger-ui/Hledger/UI/Main.hs \
#         hledger-web/app/main.hs \

# run GHCI on hledger-lib doctests
@ghci-doctest:
    cd hledger-lib; {{ STACKGHCI }} ghci hledger-lib:test:doctest

# run GHCI on Shake.hs
@ghci-shake:
    {{ STACK }} exec {{ SHAKEDEPS }} -- ghci Shake.hs


# ** ghcid

# run ghcid on hledger-lib + hledger
@ghcid:
    ghcid -c 'just ghci'

# run ghcid autobuilder on hledger-lib + hledger + hledger-ui
@ghcid-ui:
    ghcid -c 'just ghci-ui'

# run ghcid autobuilder on hledger-lib + hledger + hledger-web
@ghcid-web:
    ghcid -c 'just ghci-web'

# run ghcid autobuilding and running hledger-web with sample journal on port 5001 
@ghcid-web-run:
    ghcid -c 'just ghci-web' --test ':main -f examples/sample.journal --port 5001 --serve'

# # run ghcid autobuilding and running the test command
# ghcid-test:
#     ghcid -c 'just ghci' --test ':main test -- --color=always'

# # run ghcid autobuilding and running the test command with this TESTPATTERN
# ghcid-test-%:
#     ghcid -c 'just ghci' --test ':main test -- --color=always -p$*'

# run ghcid autobuilding and running hledger-lib doctests
@ghcid-doctest:
    ghcid -c 'cd hledger-lib; {{ STACK }} ghci hledger-lib:test:doctest' --test ':main' --reload hledger-lib

GHCIDRESTART := '--restart Makefile --restart Makefile.local'
GHCIDRELOAD  := '--reload t.j --reload t.timedot'
GHCIDCMD     := ':main -f t.j bal date:today -S'

# # run ghcid autobuilding and running a custom GHCI command with reload/restart on certain files - customise this
# ghcid-watch watch:
#     ghcid -c 'just ghci' --test '{{ GHCIDCMD }}' {{ GHCIDRELOAD }} {{ GHCIDRESTART }}

# keep synced with Shake.hs header
SHAKEDEPS := '\
    --package base-prelude \
    --package directory \
    --package extra \
    --package process \
    --package regex \
    --package safe \
    --package shake \
    --package time \
    '
#    --package hledger-lib \  # for Hledger.Utils.Debug

# run ghcid autobuilder on Shake.hs
ghcid-shake:
    stack exec {{ SHAKEDEPS }} -- ghcid Shake.hs


# ** dev.hs script

# #    hledger-lib/Hledger/Read/TimeclockReaderPP.hs
# # build the dev.hs script for quick experiments (with ghc) 
# dev:
#     {{ STACK }} ghc -- {{ CABALMACROSFLAGS }} -ihledger-lib dev.hs \

# # to get profiling deps installed, first do something like:
# # stack build --library-profiling hledger-lib timeit criterion
# # build the dev.hs script with profiling support 
# devprof:
#     {{ STACK }} ghc -- {{ CABALMACROSFLAGS }} -ihledger-lib dev.hs -rtsopts -prof -fprof-auto -osuf p_o -o devprof

# # get a time & space profile of the dev.hs script 
# dev-profile:
#     time ./devprof +RTS -P \
#     && cp devprof.prof devprof.prof.{{ TIME }} \
#     && profiterole devprof.prof

# # get heap profiles of the dev.hs script 
# dev-heap:
#     time ./devprof +RTS -hc -L1000 && cp devprof.hp devprof-hc.hp && hp2ps devprof-hc.hp
#     time ./devprof +RTS -hr -L1000 && cp devprof.hp devprof-hr.hp && hp2ps devprof-hr.hp

# dev-heap-upload:
#     curl -F "file=@devprof-hc.hp" -F "title='hledger parser'" http://heap.ezyang.com/upload
#     curl -F "file=@devprof-hr.hp" -F "title='hledger parser'" http://heap.ezyang.com/upload


# ** special builds

# build the hledger package showing GHC codegen times/allocations
@buildtimes:
    time ({{ STACK }} build hledger --force-dirty --ghc-options='-fforce-recomp -ddump-timings' 2>&1 | grep -E '\bCodeGen \[.*time=')

# # build an unoptimised hledger at bin/hledger.EXT.unopt (default: git describe)
# build-unopt *EXT:
#     #!/usr/bin/env bash
#     ext={{ if EXT == '' { `git describe --tags` } else { EXT } }}
#     exe="bin/hledger.$ext.unopt"
#     {{ STACK }} --verbosity=error install --ghc-options=-O0 hledger --local-bin-path=bin
#     mv bin/hledger "$exe"
#     echo "$exe"

# # build hledger with profiling enabled at bin/hledgerprof
# hledgerprof:
# # {{ STACK }} --verbosity=error install --local-bin-path=bin hledger
#     {{ STACK }} build --profile hledger
# # hledger-lib --ghc-options=-fprof-auto
# #    @echo "to profile, use {{ STACK }} exec --profile -- hledger ..."

# # build "bin/hledgercov" for coverage reports (with ghc) 
# hledgercov:
#     {{ STACK }} ghc {{ MAIN }} -fhpc -o bin/hledgercov -outputdir .hledgercovobjs {{ BUILDFLAGS }}



# ** installing

# # copy the current ~/.local/bin/hledger to bin/old/hledger-VER
# @copy-as VER:
# 	cp ~/.local/bin/hledger bin/old/hledger-{{ VER }}; echo "bin/hledger-{{ VER }}"

# stack install, then copy the hledger executables to bin/old/hledger*-VER
@installas VER:
	{{ STACK }} install --local-bin-path bin/old
	for e in hledger hledger-ui hledger-web ; do cp bin/old/$e bin/old/$e-{{ VER }}; echo "bin/$e-{{ VER }}"; done

# # make must be GNU Make 4.3+
# .PHONY: shellcompletions
# # update shell completions in hledger package
# shellcompletions:
#     make -C hledger/shell-completion/ clean-all all


# ** pushing

# push to github CI, wait for tests to pass, then push to master
@push:
    tools/push


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
    [[ -z {{ VER }} ]] && usage
    BRANCH=$(just _versionReleaseBranch {{ VER }})
    COMMIT="-c"
    echo "Switching to $BRANCH, auto-creating it if needed..."
    _gitSwitchAutoCreate "$BRANCH"
    echo "Bumping all version strings to {{ VER }} ..."
    ./Shake setversion {{ VER }} $COMMIT
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


# *** hledger version number helpers
# (as hidden recipes, since just doesn't support global custom functions)
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
    if [[ $(just _versionIsDev {{ VER }}) == y ]] then
      echo "{{ VER }} is not a releasable version" >&2
      exit 1
    elif [[ $(just _versionIsPreview {{ VER }}) == y ]] then
      # echo "$(just majorVersionIncrement "$MAJOR")-branch"
      echo "{{ VER }} is not a releasable version" >&2
      exit 1
    else
      echo "$MAJOR-branch"
    fi


# *** git helpers

# Does the named branch exist in this git repo ?
@_gitBranchExists BRANCH:
    git branch -l {{ BRANCH }} | grep -q {{ BRANCH }}

# Switch to the named git branch, creating it if it doesn't exist.
_gitSwitchAutoCreate BRANCH:
    #!/usr/bin/env bash
    if just _gitBranchExists {{ BRANCH }}; then
      git switch {{ BRANCH }}
    else
      git switch -c {{ BRANCH }}
    fi


# ** misc

# run some tests to validate the development environment
# check-setup:
#     run some tests to validate the development environment\
#     )
#     @echo sanity-checking developer environment:
#     @({{ SHELLTEST }} checks \
#         && echo $@ PASSED) || echo $@ FAILED

# show the sorted, unique files matched by SOURCEFILES
@listsourcefiles:
    for f in {{ SOURCEFILES }}; do echo $f; done | sort | uniq

# show the sorted, unique subdirectories containing hs files
@listsourcedirs:
    find . -name '*hs' | sed -e 's%[^/]*hs$%%' | sort | uniq

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
@_projectcommits:
    # https://hledger.org/reporting-version-control-stats.html
    printf "account ledger\naccount hledger\naccount beancount\n\n" >project-commits.j
    for p in ledger hledger beancount; do git -C ../$p log --format="%cd (%h) %s%n  ($p)  1%n" --date=short --reverse >> project-commits.j; done
    echo "wrote project-commits.j"
