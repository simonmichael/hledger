# One of two project scripts files (Makefile, Shake.hs).
# This one was a reboot of Makefile.old.
# It is probably used by hledger developers/contributors but not end-users.
# It requires GNU Make (https://www.gnu.org/software/make/).
# Also, some rules may require:
# - stack (http://haskell-lang.org/get-started, installs libs and runs ghc)
# - shelltestrunner (hackage, runs functional tests)
# - quickbench (hackage/stackage, runs benchmarks)
# - ghcid (hackage/stackage, recompiles and optionally runs tests on file change)
# - hasktags (hackage, generates tag files for code navigation)
# - profiterole (hackage/stackage, simplifies profiles)
# - profiteur (hackage/stackage, renders profiles as html)
# - hpack (hackage/stackage, generates cabal files from package.yaml files)
# - perl
#
# Kinds of hledger builds:
#
# - stack build: built with stack
#   (hledger/.stack-work/dist/ARCH/CABAL/build/hledger/hledger,
#   .stack-work/install/ARCH/SNAPSHOT/GHC/bin/hledger, installs to ~/.local/bin)
# - cabal (v1) build: built with cabal (and maybe a sandbox)
#   (hledger/dist/build/hledger/hledger, installs to ~/.cabal/bin)
# - ghc-only build: built quickly with ghc only, unoptimised, with DEVELOPMENT flag
#   (hledgerdev)
#
# This makefile mostly uses stack to get things done (slow but robust).
# It may sometimes (still ?) use ghc only, or cabal, when easier.

# see also: https://gmsl.sourceforge.io/

# XXX do we need this ?
#SHELL=/bin/bash
#.SHELLFLAGS="-O extglob -c" # example

###############################################################################
# MAKEFILE HELP SYSTEM

# This defines the def-help* functions for generating makefile help
# (see the file for more details), and a "help" target (our default).
# Every useful rule in this makefile should use def-help to describe itself.
# "make" or "make help" will show these descriptions.
-include Makefile.helpsys

# Some calls and dummy targets to augment the default help output.
# Also, help-SUBSTR and SUBSTR-help targets to show only matching help.
$(call def-help-heading,Main rules in the hledger project Makefile:)
$(call def-help-subheading,HELP:)
dummy1: $(call def-help,[help], list documented rules in this makefile )
help-%: $(call def-help,help-SECTION, list documented rules containing some string )
	@make help 2>&1 | grep -i $*
%-help: $(call def-help,SECTION-help, same but easier to type (can append "-help" to any "make RULE") )
	@make help 2>&1 | grep -i $*
dummy2: $(call def-help,RULE -n, show what RULE would do )

###############################################################################
# VARS

# GHC-compiled executables require a locale (and not just C) or they
# will die on encountering non-ascii data. Set LANG to something if not already set.
export LANG?=en_US.UTF-8

# command to run during profiling (time and heap)
PROFCMD=stack exec --profile -- hledger balance -f examples/10000x1000x10.journal >/dev/null

#PROFRTSFLAGS=-p
PROFRTSFLAGS=-P

# # command to run during "make coverage"
# COVCMD=test
# COVCMD=-f test-wf.csv print

# misc. system tools
BROWSE=open
# VIEWHTML=$(BROWSE)
VIEWPS=$(BROWSE)
# VIEWPDF=$(BROWSE)
# PRINT=lpr

#GHC=ghc
GHCI=ghci #-package ghc-datasize #-package ghc-heap-view
# GHCPKG=ghc-pkg
# HADDOCK=haddock
# CABAL=cabal
# CABALINSTALL=cabal install -w $(GHC)

# can override this with an env var, eg:
# STACK="stack --stack-yaml=stack8.10.yaml" make functest
STACK ?= stack

# if using an unreleased stack with a newer hpack than the one mentioned in */*.cabal,
# it will give warnings. To silence these, put the old hpack-X.Y in $PATH and uncomment:
#STACK=stack --with-hpack=hpack-0.20

# --threads=16 sometimes gives "commitAndReleaseBuffer: resource vanished (Broken pipe)" but seems harmless
# --timeout=N is not much use here - can be defeated by multiple threads, unoptimised builds, 
# slow hackage index or compiler setup on first build, etc.
SHELLTESTOPTS=--execdir --threads=64 --exclude=/_

# make sure shelltest is a released version of shelltestrunner
# run shell tests using the executable specified in tests
# SHELLTEST=COLUMNS=80 PATH=~/.local/bin:/usr/bin:/bin shelltest $(SHELLTESTOPTS)
# run shell tests using the stack build of hledger
#SHELLTESTSTK=shelltest -w `stack exec which hledger` $(SHELLTESTOPTS)
SHELLTESTSTK=COLUMNS=80 $(STACK) exec -- shelltest $(SHELLTESTOPTS)

# # used for make auto, http://joyful.com/repos/searchpath
# SP=sp

PACKAGES=\
	hledger-lib \
	hledger \
	hledger-ui \
	hledger-web \

BINARIES=\
	hledger \
	hledger-ui \
	hledger-web \

INCLUDEPATHS=\
	-ihledger-lib \
	-ihledger-lib/other/ledger-parse \
	-ihledger \
	-ihledger-ui \
	-ihledger-web \
	-ihledger-web/app \

MAIN=hledger/app/hledger-cli.hs

# All source files in the project (plus a few strays like Setup.hs & hlint.hs).
# Used eg for building tags. Doesn't reliably catch all source files.
SOURCEFILES:= \
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
#	hledger-*/src/*hs         \

# show the sorted, unique files matched by SOURCEFILES
sourcefiles:
	@for f in $(SOURCEFILES); do echo $$f; done | sort | uniq

# show the sorted, unique subdirectories containing hs files
sourcedirs:
	@find . -name '*hs' | sed -e 's%[^/]*hs$$%%' | sort | uniq

HPACKFILES:= \
	hledger/*package.yaml \
	hledger-*/*package.yaml \

CABALFILES:= \
	hledger/hledger.cabal \
	hledger-*/*.cabal \

MANUALSOURCEFILES:= \
	doc/common.m4 \
	*/*.m4.md \

MANUALGENFILES:= \
	hledger*/hledger*.{1,5,info,txt} \

COMMANDHELPFILES:= \
	hledger/Hledger/Cli/Commands/*.md \

WEBTEMPLATEFILES:= \
	hledger-web/templates/* \

WEBCODEFILES:= \
	hledger-web/static/*.js \
	hledger-web/static/*.css \

DOCSOURCEFILES:= \
  README.md \
  CONTRIBUTING.md \
	$(MANUALSOURCEFILES) \
	$(COMMANDHELPFILES) \

# # file(s) which require recompilation for a build to have an up-to-date version string
# VERSIONSOURCEFILE=hledger/Hledger/Cli/Version.hs

# Two or three-part version string, set as program version in builds made by this makefile.
# We use hledger CLI's current version (XXX for all packages, which isn't quite right).
VERSION=$(shell cat hledger/.version)

# the number of commits since the last tag
PATCHLEVEL=$(shell git describe --tags --match 'hledger-[0-9]*' --long | awk -F- '{print $$3}')
#PATCHLEVEL:=$(shell git describe --tags --match 'hledger-web-[0-9]*' --long | awk -F- '{print $$4}')
# the number of commits since the last_release tag
#PATCHLEVEL:=$(shell git rev-list last_release..HEAD | wc -l)

# flags for ghc builds

WARNINGS:=\
	-Wall \
	-fno-warn-unused-do-bind \
	-fno-warn-name-shadowing \
	-fno-warn-missing-signatures \
	-fno-warn-orphans \
	-fno-warn-type-defaults \

# if you have need to try building in less memory
# GHCLOWMEMFLAGS= #+RTS -M200m -RTS

# ghc-only builds need the macro definitions generated by cabal
# from cabal's dist or dist-sandbox dir, hopefully there's just one:
#CABALMACROSFLAGS=-optP-include -optP hledger/dist*/build/autogen/cabal_macros.h
# or from stack's dist dir:
#CABALMACROSFLAGS=-optP-include -optP hledger/.stack-work/dist/*/*/build/autogen/cabal_macros.h

BUILDFLAGS=\
	-rtsopts \
	$(WARNINGS) \
	$(INCLUDEPATHS) \
	$(GHCLOWMEMFLAGS) $(CABALMACROSFLAGS) \
	-DPATCHLEVEL=$(PATCHLEVEL) \
	-DDEVELOPMENT \
	-DVERSION="\"$(VERSION)\"" \
#	-fhide-source-paths \

# PROFBUILDFLAGS:=-prof -fprof-auto -osuf hs_p

TIME=$(shell date +"%Y%m%d%H%M")
MONTHYEAR=$(shell date +'%B %Y')

###############################################################################
$(call def-help-subheading,INSTALLING:)

install: \
	$(call def-help,install, download dependencies and install hledger executables to ~/.local/bin or equivalent (with stack))
	$(STACK) install

###############################################################################
$(call def-help-subheading,BUILDING:)

# EXTRAINSTALLARGS=

build: \
	$(call def-help,build, download dependencies and build hledger executables (with stack))
	$(STACK) build

# check-setup: \
# 	$(call def-help,check-setup,\
# 	run some tests to validate the development environment\
# 	)
# 	@echo sanity-checking developer environment:
# 	@($(SHELLTEST) checks \
# 		&& echo $@ PASSED) || echo $@ FAILED

link-web-dirs: config messages static templates \
	$(call def-help,link-web-dirs,\
	\
	)

config: \
	$(call def-help,config,\
	\
	)
	ln -sf hledger-web/$@

messages: \
	$(call def-help,messages,\
	\
	)
	ln -sf hledger-web/$@

static: \
	$(call def-help,static,\
	\
	)
	ln -sf hledger-web/$@

templates: \
	$(call def-help,templates,\
	\
	)
	ln -sf hledger-web/$@

save-hledger-unopt: \
	$(call def-help,save-hledger-fast, build an unoptimised hledger executable named with git describe in bin )
	$(STACK) --verbosity=error install hledger --local-bin-path=bin && mv bin/hledger{,-`git describe --tags`-unopt}
	@echo "built bin/hledger-`git describe --tags`-unopt"

save-hledger-unopt-%: \
	$(call def-help,save-hledger-fast-EXT, build an unoptimised hledger executable named with the given extension in bin )
	$(STACK) --verbosity=error install hledger --local-bin-path=bin && mv bin/hledger{,-$*-unopt}
	@echo "built bin/hledger-$*-unopt"

save-hledger: \
	$(call def-help,save-hledger, build an optimised hledger executable named with git describe in bin )
	$(STACK) --verbosity=error install hledger --local-bin-path=bin && mv bin/hledger{,-`git describe --tags`}
	@echo "built bin/hledger-`git describe --tags`"

save-hledger-%: \
	$(call def-help,save-hledger-EXT, build an optimised hledger executable named with the given suffix in bin )
	$(STACK) --verbosity=error install hledger --local-bin-path=bin && mv bin/hledger{,-$*}
	@echo "built bin/hledger-$*"

hledgerprof: \
	$(call def-help,hledgerprof, build a hledger executable with profiling enabled (with stack) )
	$(STACK) build --profile hledger
# hledger-lib --ghc-options=-fprof-auto
#	@echo "to profile, use $(STACK) exec --profile -- hledger ..."

hledgercov: \
	$(call def-help,hledgercov, build "bin/hledgercov" for coverage reports (with ghc) )
	$(STACK) ghc $(MAIN) -fhpc -o bin/hledgercov -outputdir .hledgercovobjs $(BUILDFLAGS)

#	hledger-lib/Hledger/Read/TimeclockReaderPP.hs
dev: dev.hs $(SOURCEFILES) \
	$(call def-help,dev, build the dev.hs script for quick experiments (with ghc) )
	$(STACK) ghc -- $(CABALMACROSFLAGS) -ihledger-lib dev.hs \

# to get profiling deps installed, first do something like:
# stack build --library-profiling hledger-lib timeit criterion
devprof: dev.hs $(SOURCEFILES) \
	$(call def-help,devprof, build the dev.hs script with profiling support )
	$(STACK) ghc -- $(CABALMACROSFLAGS) -ihledger-lib dev.hs -rtsopts -prof -fprof-auto -osuf p_o -o devprof

dev-profile: devprof \
	$(call def-help,dev-profile, get a time & space profile of the dev.hs script )
	time ./devprof +RTS -P \
	&& cp devprof.prof devprof.prof.$(TIME) \
	&& profiterole devprof.prof

dev-heap: devprof \
	$(call def-help,dev-heap, get heap profiles of the dev.hs script )
	time ./devprof +RTS -hc -L1000 && cp devprof.hp devprof-hc.hp && hp2ps devprof-hc.hp
	time ./devprof +RTS -hr -L1000 && cp devprof.hp devprof-hr.hp && hp2ps devprof-hr.hp

dev-heap-upload:
	curl -F "file=@devprof-hc.hp" -F "title='hledger parser'" http://heap.ezyang.com/upload
	curl -F "file=@devprof-hr.hp" -F "title='hledger parser'" http://heap.ezyang.com/upload


tools/generatejournal: tools/generatejournal.hs \
		$(call def-help,tools/generatejournal, build the generatejournal tool )
	$(STACK) ghc tools/generatejournal.hs

ghcid: $(call def-help,ghcid, start ghcid autobuilder on hledger-lib + hledger)
	ghcid -c 'make ghci'

ghcid-ui: $(call def-help,ghcid-ui, start ghcid autobuilder on hledger-lib + hledger + hledger-ui)
	ghcid -c 'make ghci-ui'

ghcid-web: $(call def-help,ghcid-web, start ghcid autobuilder on hledger-lib + hledger + hledger-web)
	ghcid -c 'make ghci-web'

ghcid-web-run: $(call def-help,ghcid-web-run, start ghcid autobuilding and running hledger-web with sample journal on port 5001 )
	ghcid -c 'make ghci-web' --test ':main -f examples/sample.journal --port 5001 --serve'

ghcid-test: $(call def-help,ghcid-test, start ghcid autobuilding and running the test command)
	ghcid -c 'make ghci' --test ':main test -- --color=always'

ghcid-test-%: $(call def-help,ghcid-test-TESTPATTERN, start ghcid autobuilding and running the test command with this TESTPATTERN)
	ghcid -c 'make ghci' --test ':main test -- --color=always -p$*'

ghcid-doctest: $(call def-help,ghcid-doctest, start ghcid autobuilding and running hledger-lib doctests)
	ghcid -c 'cd hledger-lib; $(STACK) ghci hledger-lib:test:doctest' --test ':main' --reload hledger-lib

GHCIDRESTART=--restart Makefile --restart Makefile.local
GHCIDRELOAD=--reload t.j --reload t.timedot
GHCIDCMD=:main -f t.j bal date:today -S

ghcid-watch watch: $(call def-help,ghcid-watch, start ghcid autobuilding and running a custom GHCI command with reload/restart on certain files - customise this)
	ghcid -c 'make ghci' --test '$(GHCIDCMD)' $(GHCIDRELOAD) $(GHCIDRESTART)

# keep synced with Shake.hs header
SHAKEDEPS= \
	--package base-prelude \
	--package directory \
	--package extra \
	--package process \
	--package regex \
	--package safe \
	--package shake \
	--package time \
#	--package hledger-lib \  # for Hledger.Utils.Debug

ghcid-shake: $(call def-help,ghcid-shake, start ghcid autobuilder on Shake.hs)
	stack exec $(SHAKEDEPS) -- ghcid Shake.hs

# run default GHCI
STACKGHCI=$(STACK)
# run latest GHCI for modern features
#STACKGHCI=stack --stack-yaml=stack8.10.yaml

# multi-package GHCI prompts
ghci: $(call def-help,ghci, start ghci REPL on hledger-lib + hledger)
	$(STACKGHCI) exec -- $(GHCI) $(BUILDFLAGS) hledger/Hledger/Cli/Main.hs

ghci-prof: $(call def-help,ghci-prof, start ghci REPL on hledger-lib + hledger with profiling/call stack information)
	stack build --profile hledger --only-dependencies
	$(STACKGHCI) exec -- $(GHCI) $(BUILDFLAGS) -fexternal-interpreter -prof -fprof-auto hledger/Hledger/Cli/Main.hs

ghci-dev: $(call def-help,ghci-dev, start ghci REPL on hledger-lib + hledger + dev.hs script)
	$(STACKGHCI) exec -- $(GHCI) $(BUILDFLAGS) -fno-warn-unused-imports -fno-warn-unused-binds dev.hs

ghci-ui: $(call def-help,ghci-ui, start ghci REPL on hledger-lib + hledger + hledger-ui)
	$(STACKGHCI) exec -- $(GHCI) $(BUILDFLAGS) hledger-ui/Hledger/UI/Main.hs

ghci-web: link-web-dirs $(call def-help,ghci-web, start ghci REPL on hledger-lib + hledger + hledger-web)
	$(STACKGHCI) exec -- $(GHCI) $(BUILDFLAGS) hledger-web/app/main.hs

ghci-web-test: link-web-dirs $(call def-help,ghci-web-test, start ghci REPL on hledger-lib + hledger + hledger-web + hledger-web test suite)
	$(STACKGHCI) exec -- $(GHCI) $(BUILDFLAGS) hledger-web/test/test.hs

# ghci-all: $(call def-help,ghci-all, start ghci REPL on all the hledger)
# 	$(STACK) exec -- $(GHCI) $(BUILDFLAGS) \
# 		hledger-ui/Hledger/UI/Main.hs \
# 		hledger-web/app/main.hs \

ghci-doctest: $(call def-help,ghci-doctest, start ghci REPL on hledger-lib doctests)
	cd hledger-lib; $(STACKGHCI) ghci hledger-lib:test:doctest

ghci-shake: $(call def-help,ghci-shake, start ghci REPL on Shake.hs)
	stack exec $(SHAKEDEPS) -- ghci Shake.hs

copy-bins-to-%: $(call def-help,copy-bins-to-VER, save ~/.local/bin/hledger* as hledger*-VER)
	V=$*; for B in $(BINARIES); do cp ~/.local/bin/$$B ~/.local/bin/$$B.$$V; done

###############################################################################
$(call def-help-subheading,TESTING:)

test: functest bench \
	$(call def-help,test, run default tests: functional tests (including unit tests) and benchmarks )

# For quieter tests add --silent. It may hide troubleshooting info.
# For very verbose tests add --verbosity=debug. It seems hard to get something in between.
STACKTEST=$(STACK) test
# When doing build testing, save a little time and output noise by not
# running tests & benchmarks. Comment this out if you want to run them.
SKIPTESTSBENCHS=--no-run-tests --no-run-benchmarks

buildplantest: $(call def-help,buildplantest, stack build --dry-run all hledger packages ensuring an install plan with default snapshot) \
	buildplantest-stack.yaml

buildplantest-all: $(call def-help,buildplantest-all, stack build --dry-run all hledger packages ensuring an install plan with each ghc version/stackage snapshot )
	for F in stack*.yaml; do make --no-print-directory buildplantest-$$F; done

buildplantest-%: $(call def-help,buildplantest-STACKFILE, stack build --dry-run all hledger packages ensuring an install plan with the given stack yaml file; eg make buildplantest-stack8.2.yaml )
	$(STACK) build --dry-run --test --bench --stack-yaml=$*

buildtest: $(call def-help,buildtest, force-rebuild all hledger packages/modules quickly ensuring no warnings with default snapshot) \
	buildtest-stack.yaml

buildtest-all: $(call def-help,buildtest-all, force-rebuild all hledger packages/modules quickly ensuring no warnings with each ghc version/stackage snapshot )
	for F in stack*.yaml; do make --no-print-directory buildtest-$$F; done

buildtest-%: $(call def-help,buildtest-STACKFILE, force-rebuild all hledger packages/modules quickly ensuring no warnings with the given stack yaml file; eg make buildtest-stack8.2.yaml )
	$(STACK) build --test --bench $(SKIPTESTSBENCHS) --fast --force-dirty --ghc-options=-fforce-recomp --ghc-options=-Werror --stack-yaml=$*

incr-buildtest: $(call def-help,incr-buildtest, build any outdated hledger packages/modules quickly ensuring no warnings with default snapshot. Wont detect warnings in up-to-date modules.) \
	incr-buildtest-stack.yaml

incr-buildtest-all: $(call def-help,incr-buildtest-all, build any outdated hledger packages/modules quickly ensuring no warnings with each ghc version/stackage snapshot. Wont detect warnings in up-to-date modules. )
	for F in stack*.yaml; do make --no-print-directory incr-buildtest-$$F; done

incr-buildtest-%: $(call def-help,incr-buildtest-STACKFILE, build any outdated hledger packages/modules quickly ensuring no warnings with the stack yaml file; eg make buildtest-stack8.2.yaml. Wont detect warnings in up-to-date modules. )
	$(STACK) build --test --bench $(SKIPTESTSBENCHS) --fast --ghc-options=-Werror --stack-yaml=$*

stack-clean-all: $(call def-help,stack-clean-all, do a stack clean --full with all ghc versions for paranoia/troubleshooting )
	for F in stack*.yaml; do $(STACK) clean --full --stack-yaml=$$F; done

ghcversions: $(call def-help,ghcversions, show the ghc versions used by all stack files )
	for F in stack*.yaml; do $(STACK) --stack-yaml=$$F --no-install-ghc exec -- ghc --version; done 2>&1 | grep -v 'To install the correct GHC'

pkgtest: $(call def-help,pkgtest, run the test suites in each package )
	@($(STACKTEST) && echo $@ PASSED) || (echo $@ FAILED; false)

# doctest with ghc 8.4 on mac requires a workaround, see hledger-lib/package.yaml.
# Or, could run it with ghc 8.2: 
#	@($(STACKTEST) --stack-yaml stack8.2.yaml hledger-lib:test:doctest && echo $@ PASSED) || (echo $@ FAILED; false)
doctest: $(call def-help,doctest, run the doctests in hledger-lib module/function docs )
	@($(STACKTEST) hledger-lib:test:doctest && echo $@ PASSED) || (echo $@ FAILED; false)

unittest: $(call def-help,unittest, run the unit tests in hledger-lib )
	@($(STACKTEST) hledger-lib:test:unittest && echo $@ PASSED) || (echo $@ FAILED; false)

# assumes an up to date hledger executable is built.
# I think we don't do it automatically to minimise unnecessary rebuilding.
builtintest: $(call def-help,builtintest, run hledgers built in test command)
	@($(STACK) exec hledger test && echo $@ PASSED) || (echo $@ FAILED; false)

# hledger executable to functional test: by default the development build
# in this directory, can be overridden by env var.
# eg: FUNCTESTEXE=hledger-1.20 make functest
FUNCTESTEXE ?= `$(STACK) exec -- which hledger`

#functest: addons hledger/test/addons/hledger-addon 
functest: hledger/test/addons/hledger-addon \
	$(call def-help,functest, build hledger quickly and quietly run the functional tests (and some unit tests) )
	@$(STACK) build --fast hledger
	@($(SHELLTESTSTK) --hide-successes -w $(FUNCTESTEXE) hledger/test/ bin/ \
		&& echo $@ PASSED) || (echo $@ FAILED; false)

functest-%: hledger/test/addons/hledger-addon \
	$(call def-help,functest-PAT, build hledger quickly and run just the functional tests matching PAT )
	@stack build --fast hledger
	@($(SHELLTESTSTK) -w $(FUNCTESTEXE) hledger/test/ -i "$*" \
		&& echo $@ PASSED) || (echo $@ FAILED; false)

ADDONEXTS=pl py rb sh hs lhs rkt exe com bat
hledger/test/addons/hledger-addon: \
	$(call def-help,hledger/test/addons/hledger-addon,\
	generate dummy add-ons for testing (hledger-addon the rest)\
	)
	rm -rf hledger/test/addons/hledger-*
	printf '#!/bin/sh\necho add-on: $$0\necho args: $$*\n' >hledger/test/addons/hledger-addon
	for E in '' $(ADDONEXTS); do \
		cp hledger/test/addons/hledger-addon hledger/test/addons/hledger-addon.$$E; done
	for F in addon. addon2 addon2.hs addon3.exe addon3.lhs addon4.exe add reg; do \
		cp hledger/test/addons/hledger-addon hledger/test/addons/hledger-$$F; done
	mkdir hledger/test/addons/hledger-addondir
	chmod +x hledger/test/addons/hledger-*

# hlinttest hlint: $(call def-help,hlinttest (or hlint),generate a hlint report)
# 	hlint --hint=hlint --report=hlint.html $(SOURCEFILES)

haddocktest: $(call def-help,haddocktest, run haddock to make sure it can generate docs without dying )
	@(make --quiet haddock && echo $@ PASSED) || (echo $@ FAILED; false)

cabalfiletest: $(call def-help,cabalfiletest, run cabal check to test cabal file syntax )
	@(make --no-print-directory cabalcheck && echo $@ PASSED) || (echo $@ FAILED; false)

test-stack%yaml:
	$(STACK) --stack-yaml stack$*yaml clean
	$(STACK) --stack-yaml stack$*yaml build --ghc-options="$(WARNINGS) -Werror" --test --bench --haddock --no-haddock-deps

travistest: $(call def-help,travistest, run tests similar to our travis CI tests)
	$(STACK) clean
	$(STACK) build --ghc-options=-Werror --test --haddock --no-haddock-deps hledger-lib
	$(STACK) build --ghc-options=-Werror --test --haddock --no-haddock-deps hledger
	$(STACK) build --ghc-options=-Werror --test --haddock --no-haddock-deps hledger-ui
	$(STACK) build --ghc-options=-Werror --test --haddock --no-haddock-deps hledger-web
	make functest

# committest: hlinttest unittest doctest functest haddocktest buildtest quickcabaltest \
# 	$(call def-help,committest,more thorough pre-commit/pre-push tests)

# releasetest: Clean unittest functest fullcabaltest haddocktest #buildtest doctest \
# 	$(call def-help,releasetest,pre-release tests)

HLEDGERINSTALLSH=$(PWD)/hledger-install/hledger-install.sh 
installtest: $(call def-help,installtest, run hledger-install.sh from another directory)
	(cd; $(HLEDGERINSTALLSH))

###############################################################################
$(call def-help-subheading,BENCHMARKING:)

samplejournals: $(call def-help,samplejournals, regenerate standard sample journals in examples/) \
	examples/sample.journal \
	examples/10x10x10.journal \
	examples/100x100x10.journal \
	examples/1000x1000x10.journal \
	examples/1000x10000x10.journal \
	examples/2000x1000x10.journal \
	examples/3000x1000x10.journal \
	examples/4000x1000x10.journal \
	examples/5000x1000x10.journal \
	examples/6000x1000x10.journal \
	examples/7000x1000x10.journal \
	examples/8000x1000x10.journal \
	examples/9000x1000x10.journal \
	examples/10000x1000x10.journal \
	examples/10000x10000x10.journal \
	examples/100000x1000x10.journal \
	examples/1000000x1000x10.journal \
#	examples/ascii.journal \
#	examples/chinese.journal \
#	examples/mixed.journal \

examples/sample.journal:
	true # XXX should probably regenerate this

examples/10x10x10.journal: tools/generatejournal
	tools/generatejournal 10 10 10 >$@

examples/100x100x10.journal: tools/generatejournal
	tools/generatejournal 100 100 10 >$@

examples/1000x1000x10.journal: tools/generatejournal
	tools/generatejournal 1000 1000 10 >$@

examples/1000x10000x10.journal: tools/generatejournal
	tools/generatejournal 1000 10000 10 >$@

examples/2000x1000x10.journal: tools/generatejournal
	tools/generatejournal 2000 1000 10 >$@

examples/3000x1000x10.journal: tools/generatejournal
	tools/generatejournal 3000 1000 10 >$@

examples/4000x1000x10.journal: tools/generatejournal
	tools/generatejournal 4000 1000 10 >$@

examples/5000x1000x10.journal: tools/generatejournal
	tools/generatejournal 5000 1000 10 >$@

examples/6000x1000x10.journal: tools/generatejournal
	tools/generatejournal 6000 1000 10 >$@

examples/7000x1000x10.journal: tools/generatejournal
	tools/generatejournal 7000 1000 10 >$@

examples/8000x1000x10.journal: tools/generatejournal
	tools/generatejournal 8000 1000 10 >$@

examples/9000x1000x10.journal: tools/generatejournal
	tools/generatejournal 9000 1000 10 >$@

examples/10000x1000x10.journal: tools/generatejournal
	tools/generatejournal 10000 1000 10 >$@

examples/10000x10000x10.journal: tools/generatejournal
	tools/generatejournal 10000 10000 10 >$@

examples/100000x1000x10.journal: tools/generatejournal
	tools/generatejournal 100000 1000 10 >$@

examples/1000000x1000x10.journal: tools/generatejournal
	tools/generatejournal 1000000 1000 10 >$@

examples/ascii.journal: tools/generatejournal
	tools/generatejournal 3 5 5 >$@

examples/chinese.journal: tools/generatejournal
	tools/generatejournal 3 5 5 --chinese >$@

examples/mixed.journal: tools/generatejournal
	tools/generatejournal 3 5 5 --mixed >$@

# hledger executables to bench test, can be overridden by env var, 
# eg: BENCHEXES=ledger,hledger-1.18,hledger make bench
BENCHEXES ?= hledger

bench: quickbench

quickbench: samplejournals bench.sh $(call def-help,quickbench, benchmark commands in bench.sh with quickbench and $(BENCHEXES))
	@echo; echo "run quick performance benchmarks in bench.sh (approximate, can be skewed):"
	@which -s quickbench && quickbench -w $(BENCHEXES) || echo "quickbench not installed, skipping"

# bench: samplejournals tests/bench.tests tools/simplebench \
# 	$(call def-help,bench,\
# 	run simple performance benchmarks and archive results\
# 	Requires some commands defined in tests/bench.tests and some BENCHEXES defined above.\
# 	)
# 	tools/simplebench -v -ftests/bench.tests $(BENCHEXES) | tee doc/profs/$(TIME).bench
# 	@rm -f benchresults.*
# 	@(cd doc/profs; rm -f latest.bench; ln -s $(TIME).bench latest.bench)

# criterionbench: samplejournals tools/criterionbench \
# 	$(call def-help,criterionbench,\
# 	run criterion benchmark tests and save graphical results\
# 	)
# 	tools/criterionbench -t png -k png

# progressionbench: samplejournals tools/progressionbench \
# 	$(call def-help,progressionbench,\
# 	run progression benchmark tests and save graphical results\
# 	)
# 	tools/progressionbench -- -t png -k png

# prof: samplejournals \
# 	$(call def-help,prof,\
# 	generate and archive an execution profile\
# 	) #bin/hledgerprof
# 	@echo "Profiling: $(PROFCMD)"
# 	-$(PROFCMD) +RTS $(PROFRTSFLAGS) -RTS
# 	mv hledgerprof.prof doc/profs/$(TIME).prof
# 	(cd doc/profs; rm -f latest*.prof; ln -s $(TIME).prof latest.prof)

# viewprof: prof \
# 	$(call def-help,viewprof,\
# 	generate, archive, simplify and display an execution profile\
# 	)
# 	tools/simplifyprof.hs doc/profs/latest.prof

quickprof-%: hledgerprof samplejournals \
		$(call def-help,quickprof-"CMD", run some command against a standard sample journal and display the execution profile )
	$(STACK) exec --profile -- hledger +RTS $(PROFRTSFLAGS) -RTS $* -f examples/1000x1000x10.journal >/dev/null
	profiterole hledger.prof
	@echo
	@head -20 hledger.prof
	@echo ...
	@echo
	@head -20 hledger.profiterole.txt
	@echo ...
	@echo
	@echo "See hledger.prof, hledger.profiterole.txt, hledger.profiterole.html for more."

# heap: samplejournals \
# 	$(call def-help,heap,\
# 	generate and archive a graphical heap profile\
# 	) #bin/hledgerprof
# 	@echo "Profiling heap with: $(PROFCMD)"
# 	$(PROFCMD) +RTS -hc -RTS
# 	mv hledgerprof.hp doc/profs/$(TIME).hp
# 	(cd doc/profs; rm -f latest.hp; ln -s $(TIME).hp latest.hp; \
# 		hp2ps $(TIME).hp; rm -f latest.ps; ln -s $(TIME).ps latest.ps; rm -f *.aux)

# viewheap: heap \
# 	$(call def-help,viewheap,\
# 	\
# 	)
# 	$(VIEWPS) doc/profs/latest.ps

quickheap-%: hledgerprof samplejournals \
		$(call def-help,quickheap-"CMD", run some command against a sample journal and display the heap profile )
	$(STACK) exec -- hledgerprof +RTS -hc -RTS $* -f examples/10000x1000x10.journal >/dev/null
	hp2ps hledgerprof.hp
	@echo generated hledgerprof.ps
	$(VIEWPS) hledgerprof.ps

# quickcoverage: hledgercov \
# 	$(call def-help,quickcoverage,\
# 	display a code coverage text report from running hledger COVCMD\
# 	)
# 	@echo "Generating code coverage text report for hledger command: $(COVCMD)"
# 	tools/runhledgercov "report" $(COVCMD)

# coverage: samplejournals hledgercov \
# 	$(call def-help,coverage,\
# 	generate a code coverage html report from running hledger COVCMD\
# 	)
# 	@echo "Generating code coverage html report for hledger command: $(COVCMD)"
# 	tools/runhledgercov "markup --destdir=doc/profs/coverage" $(COVCMD)
# 	cd doc/profs/coverage; rm -f index.html; ln -s hpc_index.html index.html

# viewcoverage: \
# 	$(call def-help,viewcoverage,\
# 	view the last html code coverage report\
# 	)
# 	$(VIEWHTML) doc/profs/coverage/index.html

###############################################################################
$(call def-help-subheading,DOCUMENTATION: (see also Shake.hs))

# cf http://www.haskell.org/haddock/doc/html/invoking.html
# --ghc-options=-optP-P is a workaround for http://trac.haskell.org/haddock/ticket/284
HADDOCKFLAGS= \
	--haddock-options='--no-warnings' \
	--ghc-options='-optP-P' \

haddock: \
	$(call def-help,haddock, generate haddock docs for the hledger packages )
	$(STACK) haddock --no-haddock-deps --fast --no-keep-going
#	$(STACK) -v

haddock-watch: \
	$(call def-help,haddock-watch, regenerate haddock docs )
	$(STACK) haddock --no-haddock-deps --fast --file-watch

haddock-open: \
	$(call def-help,haddock-open,\
	browse the haddock generated for hledger-lib\
	)
	$(VIEWHTML) hledger/dist/doc/html/hledger-lib/index.html

# sourcegraph: \
# 	$(call def-help,sourcegraph,\
# 	\
# 	)
# 	for p in $(PACKAGES); do (cd $$p; SourceGraph $$p.cabal); done

# #set up the hoogle web interface
# ## We munge haddock and hoogle into a rough but useful framed layout.
# ## For this to work the hoogle cgi must be built with base target "main".
# ## XXX move the framed index building into haddock: ?
# # 	sed -i -e 's%^></HEAD%><base target="main"></HEAD%' site/api/modules-index.html ; \
# # 	cp site/api-frames.html site/api/index.html ; \
# # # 	cp site/hoogle-small.html site/api
# #
# #uses a hoogle source tree configured with --datadir=., patched to fix haddock urls/target frame
# # HOOGLESRC=/usr/local/src/hoogle
# # HOOGLE=$(HOOGLESRC)/dist/build/hoogle/hoogle
# # HOOGLEVER=`$(HOOGLE) --version |tail -n 1 | sed -e 's/Version /hoogle-/'`
# # hoogle: hoogleindex
# # 	if test -f $(HOOGLE) ; then \
# # 		cd site/api && \
# # 		rm -f $(HOOGLEVER) && \
# # 		ln -s . $(HOOGLEVER) && \
# # 		cp -r $(HOOGLESRC)/src/res/ . && \
# # 		cp -p $(HOOGLE) index.cgi && \
# # 		touch log.txt && chmod 666 log.txt ; \
# # 	else \
# # 		echo "Could not find $(HOOGLE) in the hoogle source tree" ; \
# # 	fi
# #
# #generate a hoogle index
# # hoogleindex:
# # 	$(HADDOCK) $(HADDOCKFLAGS) -o site/api --hoogle $(MAIN) && \
# # 	cd site/api && \
# # 	hoogle --convert=main.txt --output=default.hoo

# flaky
site-watch: Shake \
		$(call def-help,site-watch, open a browser on the website (in ./site) and rerender/reload when manuals or site content changes  )
	(ls $(DOCSOURCEFILES) | entr ./Shake -VV manuals) &
	make -C site html-watch

manuals-watch: Shake \
		$(call def-help,manuals-watch, rerender manuals when their source files change  )
	ls $(DOCSOURCEFILES) | entr ./Shake -VV manuals

shakehelp-watch: \
		$(call def-help,shakehelp-watch, rerender Shake.hs's help when it changes)
	ls Shake.hs | entr -c ./Shake.hs

# This rule, for updating the live hledger.org site, gets called by:
# 1. github-post-receive (github webhook handler), when something is pushed
#    to the main or wiki repos on Github. Config:
#     /etc/supervisord.conf -> [program:github-post-receive]
#     /etc/github-post-receive.conf
# 2. cron, nightly. Config: /etc/crontab
# 3. manually (make site).
# This uses the existing Shake executable without rebuilding it, 
# as we don't want to immediately execute new code from any collaborator.
.PHONY: site
site: \
		$(call def-help,site, update the hledger.org website (run on prod) )
	@[ ! -x Shake ] \
		&& echo 'Please run "make Shake" first (manual compilation of Shake.hs is required)' \
		|| ( \
			echo; \
			./Shake -V webmanuals; \
			make -C site html; \
		) 2>&1 | tee -a site.log

###############################################################################
$(call def-help-subheading,RELEASING:)

# old/desired release process:
#  a normal release: echo 0.7   >.version; make release
#  a bugfix release: echo 0.7.1 >.version; make release
#release: releasetest bumpversion tagrelease $(call def-help,release, prepare and test a release and tag the repo )
#publish: hackageupload pushtags $(call def-help,upload, publish latest hackage packages and push tags to github )
#releaseandpublish: release upload $(call def-help,releaseandpublish, release and upload and publish updated docs )

ISCLEAN=git diff-index --quiet HEAD --

# stop if the working directory has uncommitted changes
iscleanwd:
	@$(ISCLEAN) || (echo "please clean the working directory first"; false)

# stop if the given file(s) have uncommitted changes
isclean-%:
	@$(ISCLEAN) $* || (echo "please clean these files first: $*"; false)

# update all cabal files based on latest package.yaml files using stack's built-in hpack
cabal: $(call def-help,cabal, regenerate cabal files from package.yaml files with stack )
	$(STACK) build --dry-run --silent

# Update all cabal files based on latest package.yaml files using a specific hpack version.
# To avoid warnings, this should be the same version as stack's built-in hpack.
cabal-with-hpack-%:
	$(STACK) build --with-hpack hpack-$* --dry-run --silent

# updatecabal: gencabal $(call def-help,updatecabal, regenerate cabal files and commit )
# 	@read -p "please review changes then press enter to commit $(shell ls */*.cabal)"
# 	git commit -m "update cabal files" $(shell ls */*.cabal)

# we use shake for this job; so dependencies aren't checked here
manuals: Shake $(call def-help,manuals, regenerate and commit CLI help and manuals (might need -B) )
	./Shake manuals
	git commit -m ";doc: regen manuals" -m "[ci skip]" hledger*/hledger*.{1,5,info,txt} hledger/Hledger/Cli/Commands/*.txt

tag: $(call def-help,tag, make git release tags for the project and all packages )
	@for p in $(PACKAGES); do make tag-$$p; done
	@make tag-project

tag-%: $(call def-help,tag-PKG, make a git release tag for PKG )
	git tag -fs $*-`cat $*/.version` -m "Release $*-`cat $*/.version`"

tag-project: $(call def-help,tag-project, make a git release tag for the project as a whole )
	git tag -fs `cat .version` -m "Release `cat .version`, https://hledger.org/release-notes.html#hledger-`cat .version | sed -e 's/\./-/g'`"

# hackageupload-dry: \
# 	$(call def-help,hackageupload-dry,\
# 	upload all packages to hackage; dry run\
# 	)
# 	for p in $(PACKAGES); do cabal upload $$p/dist/$$p-$(VERSION).tar.gz -v2 --check; done

hackageupload: \
	$(call def-help,hackageupload, upload all packages to hackage	)
	for p in $(PACKAGES); do stack upload $$p; done

# showreleasestats stats: \
# 	showreleasedays \
# 	showunreleasedchangecount \
# 	showloc \
# 	showtestcount \
# 	showunittestcoverage \
# 	showreleaseauthors \
# 	showunreleasedcodechanges \
# 	showunpushedchanges \
# 	$(call def-help,showreleasestats stats,\
# 	show project stats useful for release notes\
# 	)
# #	showerrors

# FROMTAG=.

# showreleasedays: \
# 	$(call def-help,showreleasedays,\
# 	\
# 	)
# 	@echo Days since last release:
# 	@tools/dayssincetag.hs $(FROMTAG) | head -1 | cut -d' ' -f-1
# 	@echo

# # XXX
# showunreleasedchangecount: \
# 	$(call def-help,showunreleasedchangecount,\
# 	\
# 	)
# 	@echo Commits since last release:
# 	@darcs changes --from-tag $(FROMTAG) --count
# 	@echo

describe: $(call def-help,describe, show a precise git-describe version string )
	@git describe --tags --match 'hledger-[0-9]*' --dirty

# showreleaseauthors: $(call def-help,showreleaseauthors, show author names since last release)
# 	@echo Commit authors since last release:
# 	@git shortlog -sn $(CHANGELOGSTART)..  # TODO undefined

cloc: $(call def-help,cloc, count lines of source code )
	@echo Lines of code including tests:
	@cloc --exclude-lang=HTML --exclude-dir=.stack-work,.idea,dist,old,bin,doc,site,.tutorial-data,static,angular .


	@echo
# `ls $(SOURCEFILES)`

# sloc: \
# 	$(call def-help,sloc,\
# 	\
# 	)
# 	@sloccount hledger-lib hledger hledger-web

# cloc: \
# 	$(call def-help,cloc,\
# 	\
# 	)
# 	@echo
# 	@echo "Lines of code as of `date`:"
# 	@echo
# 	@echo "hledger-lib, hledger"
# 	@cloc -q hledger-lib hledger             2>&1 | grep -v 'defined('
# 	@echo
# 	@echo "hledger-web"
# 	@cloc -q hledger-web                     2>&1 | grep -v 'defined('
# 	@echo
# 	@echo "hledger-lib, hledger, hledger-web"
# 	@cloc -q hledger-lib hledger hledger-web 2>&1 | grep -v 'defined('

# showtestcount: \
# 	$(call def-help,showtestcount,\
# 	\
# 	)
# 	@echo "Unit tests:"
# 	@hledger test 2>&1 | cut -d' ' -f2
# 	@echo "Functional tests:"
# 	@make --no-print functest | egrep '^ Total' | awk '{print $$2}'
# 	@echo

# showunittestcoverage: \
# 	$(call def-help,showunittestcoverage,\
# 	\
# 	)
# 	@echo Unit test coverage:
# 	@make --no-print quickcoverage | grep 'expressions'
# 	@echo

# # showerrors:
# # 	@echo Known errors:
# # 	@awk '/^** errors/, /^** / && !/^** errors/' NOTES.org | grep '^\*\*\* ' | tail +1
# # 	@echo

# # XXX
# showunpushedchanges showunpushed: \
# 	$(call def-help,showunpushedchanges showunpushed,\
# 	\
# 	)
# 	@echo "Changes not yet pushed upstream (to `darcs show repo | grep 'Default Remote' | cut -c 17-`):"
# 	@-darcs push simon@joyful.com:/repos/hledger --dry-run | grep '*' | tac
# 	@echo

# # XXX
# showunreleasedcodechanges showunreleased showchanges: \
# 	$(call def-help,showunreleasedcodechanges showunreleased showchanges,\
# 	\
# 	)
# 	@echo "hledger code changes since last release:"
# 	@darcs changes --from-tag $(FROMTAG) --matches "not (name docs: or name doc: or name site: or name tools:)" | grep '*'
# 	@echo

# # XXX
# showcodechanges: \
# 	$(call def-help,showcodechanges,\
# 	\
# 	)
# 	@echo "hledger code changes:"
# 	@darcs changes --matches "not (name docs: or name site: or name tools:)" | egrep '^ +(\*|tagged)'
# 	@echo

nix-hledger-version: $(call def-help,nix-hledger-version, show which version of hledger has reached nixpkgs)
	@curl -s https://raw.githubusercontent.com/NixOS/nixpkgs/master/pkgs/development/haskell-modules/hackage-packages.nix | grep -A1 'pname = "hledger"'

nix-hledger-versions: $(call def-help,nix-hledger-versions, show versions of all hledger packages in nixpkgs)
	@curl -s https://raw.githubusercontent.com/NixOS/nixpkgs/master/pkgs/development/haskell-modules/hackage-packages.nix | grep -A1 'pname = "hledger'

make nix-view-commits: $(call def-help,nix-view-commits, show recent haskell commits in nixpkgs)
	@open 'https://github.com/search?l=&o=desc&q=%22automatic+Haskell+package+set+update%22+repo%3ANixOS%2Fnixpkgs+filename%3Ahaskell-packages.nix&s=committer-date&type=Commits'

###############################################################################
$(call def-help-subheading,MISCELLANEOUS:)

watch-%: $(call def-help,watch-RULE, run make RULE repeatedly when any committed file changes)
	 @git ls-files | entr -r make $*

Shake: Shake.hs $(call def-help,Shake, ensure the Shake script is compiled )
	./Shake.hs

usage: cabalusage stackusage \
	$(call def-help,usage, show size of various dirs )
	du -sh .git bin data doc extra
	du -sh .

stackusage: \
	$(call def-help,stackusage, show size of stack working dirs if any )
	-du -shc `find . -name '.stack*'`

cabalusage: \
	$(call def-help,cabalusage, show size of cabal working dirs if any )
	-du -shc */dist* 2>/dev/null

# Tag haskell files with hasktags and just list the other main source files
# so they will be traversed by tags-search/tags-query-replace.
# etags:
# 	rm -f TAGS
# 	hasktags -e $(SOURCEFILES)
# 	for f in Makefile $(WEBCODEFILES) $(HPACKFILES) $(CABALFILES) $(DOCSOURCEFILES); do \
# 		printf "\n$$f,1\n" >> TAGS; \
# 	done

# Tag:
# - haskell files, with hasktags
# - everything else not excluded by .ctags, with (exuberant) ctags
# - files currently missed by the above, just their names (docs, hpack, cabal..)
etags:$(call def-help,etags, generate emacs tag files for source code navigation )
	hasktags -e $(SOURCEFILES)
	ctags -a -e -R  
	for f in \
		$(WEBTEMPLATEFILES) \
		$(DOCSOURCEFILES) \
		$(HPACKFILES) \
		$(CABALFILES) \
		Shake.hs \
	; do printf "\n$$f,1\n" >> TAGS; done

cleantags: \
	$(call def-help-hide,cleantags, remove tag files )
	rm -f TAGS tags

stackclean: \
	$(call def-help-hide,stackclean, remove .stack-work/ dirs )
	$(STACK) purge

cleanghco: \
	$(call def-help-hide,cleanghc, remove ghc build leftovers )
	rm -rf `find . -name "*.o" -o -name "*.hi" -o -name "*.dyn_o" -o -name "*.dyn_hi" -o -name "*~" | grep -vE '\.(stack-work|cabal-sandbox|virthualenv)'`
#rm -f `fd -I '\.(hi|o|dyn_hi|dyn_o)$'`

clean: cleanghco \
	$(call def-help,clean, default cleanup (ghc build leftovers) )

Clean: stackclean cleanghco cleantags \
	$(call def-help,Clean, thorough cleanup (stack/ghc leftovers/tags) )

# reverse = $(if $(wordlist 2,2,$(1)),$(call reverse,$(wordlist 2,$(words $(1)),$(1))) $(firstword $(1)),$(1))

###############################################################################
# END

# optional local customisations, not in version control
-include Makefile.local

# show a final message in make help
$(call def-help-heading,)
$(call def-help-heading,See also ./Shake.hs help)
