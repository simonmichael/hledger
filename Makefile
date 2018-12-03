# One of two project scripts files (Makefile, Shake.hs).
# This one is usually the simplest to use.
# It requires GNU Make (https://www.gnu.org/software/make/).
# Also, some rules require:
# - stack (http://haskell-lang.org/get-started, installs libs and runs ghc)
# - shelltestrunner (hackage, runs functional tests)
# - quickbench (hackage/stackage, runs benchmarks)
# - hasktags (hackage, generates tag files for code navigation)
# - profiteur (hackage/stackage, renders profiles as html)
# - hpack (hackage/stackage, generates cabal files from package.yaml files)
# - site/hakyll-std/hakyll-std (generic site-building hakyll script)
# - perl

# This was a reboot of Makefile.old. The old rules were commented out below,
# to be removed or updated over the next while.
#
# Target users: hledger developers, contributors, probably not end-users.
#
# Kinds of hledger builds:
#
# - stack build: built with stack
#   (hledger/.stack-work/dist/ARCH/CABAL/build/hledger/hledger,
#   .stack-work/install/ARCH/SNAPSHOT/GHC/bin/hledger, installs to ~/.local/bin)
# - cabal build: built with cabal (and maybe a sandbox)
#   (hledger/dist/build/hledger/hledger, installs to ~/.cabal/bin)
# - ghc build: built quickly with ghc only, unoptimised, with DEVELOPMENT flag
#   (bin/hledgerdev)
#
# This makefile mostly uses stack to get things done (slow but robust).
# Secondarily it uses ghc directly to do some developer tasks (faster).
# # Also if needed it uses cabal directly for a few tasks.

# XXX do we need this ?
#SHELL=/bin/bash
#.SHELLFLAGS="-O extglob -c" # example

# help system
# Every user-relevant rule in this makefile should use def-help to define
# a help string. Use "make help" to see the available rules.

# def-help* functions for documenting make rules. See the file for usage.
-include help-system.mk

$(call def-help-heading,Main make rules in the hledger project.)
$(call def-help-heading,(not always entirely up to date))
$(call def-help-heading,---------------------------------------)
$(call def-help-heading, )

add-to-help-1: $(call def-help,[help], list documented rules in this makefile )

help-%: $(call def-help,help-SECTION, list documented rules containing some string )
	make help 2>&1 | grep -i $*

%-help: $(call def-help,SECTION-help, same but easier to type (can append "-help" to any "make RULE") )
	@make help 2>&1 | grep -i $*

add-to-help-2: $(call def-help,RULE -n, show what RULE would do )

###############################################################################
# VARS

# GHC-compiled executables require a locale (and not just C) or they
# will die on encountering non-ascii data. Set LANG to something if not already set.
export LANG?=en_US.UTF-8

# command to run during profiling (time and heap)
PROFCMD=stack exec -- hledgerprof balance -f examples/10000x1000x10.journal >/dev/null

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

GHC=ghc
GHCI=ghci #-package ghc-datasize #-package ghc-heap-view
# GHCPKG=ghc-pkg
# HADDOCK=haddock
# CABAL=cabal
# CABALINSTALL=cabal install -w $(GHC)
STACK=stack
# if using an unreleased stack with a newer hpack than the one mentioned in */*.cabal,
# it will give warnings. To silence these, put the old hpack-X.Y in $PATH and uncomment:
#STACK=stack --with-hpack=hpack-0.20

# -j16 sometimes gives "commitAndReleaseBuffer: resource vanished (Broken pipe)" but seems harmless
SHELLTESTOPTS=--execdir -j16 --hide-successes --exclude=/_

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
	hledger-api \

INCLUDEPATHS=\
	-ihledger-lib \
	-ihledger-lib/other/ledger-parse \
	-ihledger \
	-ihledger-ui \
	-ihledger-web \
	-ihledger-web/app \
	-ihledger-api \

MAIN=hledger/app/hledger-cli.hs

# all source files in the project (plus a few strays like Setup.hs & hlint.hs)
SOURCEFILES:= \
	dev.hs \
	hledger/*hs \
	hledger/bench/*hs \
	hledger/Hledger/*hs \
	hledger/Hledger/*/*hs \
	hledger/Hledger/*/*/*hs \
	hledger-*/*hs \
	hledger-*/Hledger/*hs \
	hledger-*/Hledger/*/*hs \
	hledger-lib/other/ledger-parse/Ledger/Parser/*hs \
	hledger-web/**/*.hs \

HPACKFILES:= \
	hledger/*package.yaml \
	hledger-*/*package.yaml \

CABALFILES:= \
	hledger/hledger.cabal \
	hledger-*/*.cabal \

MANUALSOURCEFILES:= \
	doc/lib.m4 \
	*/*.m4.md \

MANUALGENFILES:= \
	hledger*/hledger*.{1,5,info,txt} \

# site/*.md includes website source files and generated web manual files
# WEBDOCFILES:= \
# 	site/*.md \

WEBCODEFILES:= \
	hledger-web/templates/* \
	hledger-web/static/*.js \
	hledger-web/static/*.css \

DOCSOURCEFILES:= \
  README.md \
	$(MANUALSOURCEFILES) \

# files which should be updated when the version changes
VERSIONSENSITIVEFILES=\
	$(HPACKFILES) \
	hledger-api/hledger-api.hs \
	doc/lib.m4 \

# # file(s) which require recompilation for a build to have an up-to-date version string
# VERSIONSOURCEFILE=hledger/Hledger/Cli/Version.hs

# master file defining the current release/build version
VERSIONFILE=.version

# two or three-part version string, whatever's in VERSION
VERSION=$(shell cat $(VERSIONFILE))

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

# # For ghc-only dev builds of hledger-web: enable the language
# # extensions specified in hledger-web.cabal, except for some which are
# # not compatible with hledger-lib and hledger, or little-used; those
# # are enabled with source file pragmas instead. Note: compilation
# # warnings and errors might differ between ghc-only and cabal builds.
# WEBLANGEXTS:=\
# 	-XCPP \
# 	-XMultiParamTypeClasses \
# 	-XQuasiQuotes \
# 	-XRecordWildCards \
# 	-XTemplateHaskell \
# #	-XOverloadedStrings \
# #	-XNoImplicitPrelude \
# #	-XTypeFamilies \
# #	-XGADTs \
# #	-XGeneralizedNewtypeDeriving \
# #	-XFlexibleContexts \
# #	-XEmptyDataDecls \
# #	-XNoMonomorphismRestriction

# PREFERMACUSRLIBFLAGS=-L/usr/lib
# GHCMEMFLAGS= #+RTS -M200m -RTS

# ghc builds need the macro definitions generated by cabal
# from cabal's dist or dist-sandbox dir, hopefully there's just one
#CABALMACROSFLAGS=-optP-include -optP hledger/dist*/build/autogen/cabal_macros.h
# from stack's dist dir, hopefully there's just one. If not (after cabal upgrade), will get warnings. Maybe obsolete ?
#CABALMACROSFLAGS=-optP-include -optP hledger/.stack-work/dist/*/*/build/autogen/cabal_macros.h

BUILDFLAGS1=-rtsopts $(WARNINGS) $(INCLUDEPATHS) $(PREFERMACUSRLIBFLAGS) $(GHCMEMFLAGS) $(CABALMACROSFLAGS) -DPATCHLEVEL=$(PATCHLEVEL) -DDEVELOPMENT
BUILDFLAGS=$(BUILDFLAGS1) -DVERSION="\"$(VERSION)\""
# PROFBUILDFLAGS:=-prof -fprof-auto -osuf hs_p
# # sp needs different quoting:
# AUTOBUILDFLAGS:=$(BUILDFLAGS1) -DVERSION='\"$(VERSION)dev\"' # $(PROFBUILDFLAGS)
# LINUXRELEASEBUILDFLAGS:=-DMAKE $(WARNINGS) $(INCLUDEPATHS) -O2 -static -optl-static -optl-pthread
# MACRELEASEBUILDFLAGS:=-DMAKE $(WARNINGS) $(INCLUDEPATHS) $(PREFERMACUSRLIBFLAGS) -O2 # -optl-L/usr/lib
# #WINDOWSRELEASEBUILDFLAGS:=-DMAKE $(WARNINGS) $(INCLUDEPATHS)

# AUTOBUILD=$(SP) --no-exts --no-default-map $(GHC)  -O0 $(GHCMEMFLAGS)

# # get an accurate binary filename for the current source and platform, slow but reliable. Avoid := here.
# BINARYFILENAME=$(shell touch $(VERSIONSOURCEFILE); runhaskell -ihledger -ihledger-lib $(MAIN) --binary-filename)

# # some other thing for linux binary filenames
# RELEASEBINARYSUFFIX=$(shell echo "-$(VERSION)-`uname`-`arch`" | tr '[:upper:]' '[:lower:]')

TIME=$(shell date +"%Y%m%d%H%M")
MONTHYEAR=$(shell date +'%B %Y')

###############################################################################
$(call def-help-subheading,INSTALLING:)

install: \
	$(call def-help,install, download dependencies and install hledger executables to ~/.local/bin or equivalent (with stack))
	$(STACK) install

# cabal-install: \
# 	$(call def-help,cabal-install,\
# 	cabal install the main hledger packages and all their dependencies\
# 	in the sandbox if any; otherwise in the users package db\
# 	)
# 	$(CABALINSTALL) $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS) --enable-tests

# cabal-install-deps: \
# 	$(call def-help,cabal-install-deps,\
# 	cabal install the dependencies for the main hledger packages, but not the hledger packages \
# 	)
# 	$(CABALINSTALL) $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS) --enable-tests --only-dependencies

# # uninstall: \
# # 	$(call def-help,uninstall,\
# # 	unregister all packages, assuming they are defined lowest-dependency first\
# # 	avoids some reinstall noise when repeatedly doing make install\
# # 	)
# # 	-for p in $(call reverse,$(PACKAGES)); do $(GHCPKG) unregister $$p; done

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

# auto: auto---version \
# 	$(call def-help,auto,\
# 	auto-recompile and run (something, eg --help or unit tests) whenever a module changes\
# 	)

# auto-%: sp \
# 	$(call def-help,auto-%,\
# 	\
# 	)
# 	$(AUTOBUILD) $(MAIN) -o bin/hledgerdev $(AUTOBUILDFLAGS) --run $*

# autoweb: sp link-web-dirs \
# 	$(call def-help,autoweb,\
# 	\
# 	)
# 	$(AUTOBUILD) hledger-web/app/main.hs -o bin/hledger-webdev $(AUTOBUILDFLAGS) $(WEBLANGEXTS) --run -B --port 5001 --base-url http://localhost:5001 -f webtest.j

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

# sp: \
# 	$(call def-help,sp,\
# 	check for sp and explain how to get it if not found.\
# 	)
# 	@/usr/bin/env which sp >/dev/null || \
# 	  (echo '"sp" is required for auto-compilation. darcs get http://joyful.com/darcsden/simon/searchpath, make it (cabal install-ing any needed packages) and add it to your PATH'; exit 1)

# force a compile even if binary exists, since we don't specify dependencies for these
.PHONY: bin/hledgerdev bin/hledgerprof # bin/hledger-webdev

# NB requires cabal macros generated by cabal build in hledger/
bin/hledgerdev hledgerdev: \
	$(call def-help,hledgerdev, build an unoptimised "bin/hledgerdev" executable quickly (with ghc) )
	$(GHC) $(MAIN) -o bin/hledgerdev $(BUILDFLAGS)

# bin/hledgerdev.ghc-%: $(SOURCEFILES) \
# 	$(call def-help,bin/hledgerdev.ghc-%,\
# 	build a GHC-version-specific hledger binary without disturbing with other GHC version builds\
# 	)
# 	ghc-$*  $(MAIN) -o $@ $(BUILDFLAGS)  -outputdir .ghc-$*

# bin/hledgerdev.ghcall: \
# 	bin/hledgerdev.ghc-7.6.1 \
# 	bin/hledgerdev.ghc-7.4.1 \
# 	bin/hledgerdev.ghc-7.2.2 \
# 	bin/hledgerdev.ghc-7.0.4 \
# 	$(call def-help,bin/hledgerdev.ghcall,\
# 	build hledger with the main supported GHC versions\
# 	)

hledgerprof: \
	$(call def-help,hledgerprof, build "hledgerprof" for profiling (with stack) )
	$(STACK) build hledger-lib hledger --library-profiling --executable-profiling --ghc-options=-fprof-auto
	cp `$(STACK) exec which hledger`{,prof}
	@echo to profile, use $(STACK) exec -- hledgerprof ...

# bin/hledgerprof: \
# 	$(call def-help,bin/hledgerprof,\
# 	build the "production" cabal build with profiling enabled.\
# 	)
# 	rm -f bin/hledgerprof
# 	cabal install --enable-profiling --ghc-options=-fprof-auto ./hledger-lib ./hledger \
# 		&& mv  $(CABALINSTALLDIR)/bin/hledger bin/hledgerprof

# # build the dev build with profiling enabled.
# # not working with cabal sandbox
# # bin/hledgerdev-prof:
# # 	$(GHC) $(BUILDFLAGS) $(PROFBUILDFLAGS) $(MAIN) -o $@

hledgercov: \
	$(call def-help,hledgercov, build "bin/hledgercov" for coverage reports (with ghc) )
	$(GHC) $(MAIN) -fhpc -o bin/hledgercov -outputdir .hledgercovobjs $(BUILDFLAGS)

#	hledger-lib/Hledger/Read/TimeclockReaderPP.hs
dev: dev.hs $(SOURCEFILES) \
	$(call def-help,dev, build the dev.hs script for quick experiments (with ghc) )
	$(STACK) ghc -- $(CABALMACROSFLAGS) -ihledger-lib dev.hs \

# dev0: dev.hs $(SOURCEFILES) \
# 	$(call def-help,dev, build the dev.hs script for quick experiments (with ghc -O0) )
# 	$(STACK) ghc -- -O0 $(CABALMACROSFLAGS) -ihledger-lib dev.hs -o dev0 \

# dev2: dev.hs $(SOURCEFILES) \
# 	$(call def-help,dev, build the dev.hs script for quick experiments (with ghc -O2) )
# 	$(STACK) ghc -- -O2 $(CABALMACROSFLAGS) -ihledger-lib dev.hs -o dev2 \

# to get profiling deps installed, first do something like:
# stack build --library-profiling hledger-lib timeit criterion
devprof: dev.hs $(SOURCEFILES) \
	$(call def-help,devprof, build the dev.hs script with profiling support )
	$(STACK) ghc -- $(CABALMACROSFLAGS) -ihledger-lib dev.hs -rtsopts -prof -fprof-auto -osuf p_o -o devprof

dev-profile: devprof \
	$(call def-help,dev-profile, get a time & space profile of the dev.hs script )
	time ./devprof +RTS -P \
	&& cp devprof.prof devprof.prof.$(TIME) \
	&& profiteur devprof.prof

dev-heap: devprof \
	$(call def-help,dev-heap, get heap profiles of the dev.hs script )
	time ./devprof +RTS -hc -L1000 && cp devprof.hp devprof-hc.hp && hp2ps devprof-hc.hp
	time ./devprof +RTS -hr -L1000 && cp devprof.hp devprof-hr.hp && hp2ps devprof-hr.hp

dev-heap-upload:
	curl -F "file=@devprof-hc.hp" -F "title='hledger parser'" http://heap.ezyang.com/upload
	curl -F "file=@devprof-hr.hp" -F "title='hledger parser'" http://heap.ezyang.com/upload

# # build other executables quickly

# bin/hledger-webdev: link-web-dirs \
# 	$(call def-help,bin/hledger-webdev,\
# 	\
# 	)
# 	$(GHC) -o $@ $(BUILDFLAGS) $(WEBLANGEXTS) hledger-web/app/main.hs

# bin/hledger-web-production: \
# 	$(call def-help,bin/hledger-web-production,\
# 	\
# 	)
# 	$(GHC) -o $@ $(BUILDFLAGS) $(WEBLANGEXTS) hledger-web/app/main.hs

# linuxbinaries: 	linuxbinary-hledger \
# 		linuxbinary-hledger-web \
# 	$(call def-help,linuxbinaries,\
# 	build portable releaseable binaries for gnu/linux\
# 	)
# 	@echo 'Please check the binaries look portable, then make compressbinaries:'
# 	-file bin/*`arch`

# linuxbinary-hledger: \
# 	$(call def-help,linuxbinary-hledger,\
# 	work around for inconsistently-named (why ?) hledger/app/hledger-cli.hs\
# 	)
# 	$(GHC) hledger/app/hledger-cli.hs -o bin/$*$(RELEASEBINARYSUFFIX) $(LINUXRELEASEBUILDFLAGS)

# linuxbinary-%: \
# 	$(call def-help,linuxbinary-%,\
# 	\
# 	)
# 	$(GHC) $*/$*.hs -o bin/$*$(RELEASEBINARYSUFFIX) $(LINUXRELEASEBUILDFLAGS)

# macbinaries:    macbinary-hledger \
# 		macbinary-hledger-web \
# 	$(call def-help,macbinaries,\
# 	\
# 	)
# 	@echo 'Please check the binaries are portable, then make compressbinaries'
# 	otool -L bin/*`arch`

# macbinary-%: \
# 	$(call def-help,macbinary-%,\
# 	build a deployable mac binary for the specified hledger package, munging\
# 	the link command to use only standard osx libs.  Specifically we link\
# 	without the non-standard GMP framework, which causes no apparent harm.\
# 	Clunky, does the link twice.\
# 	)
# 	BINARY=`echo $(BINARYFILENAME) | sed -e 's/hledger/$*/'` ; \
# 	LINKCMD=`$(GHC) -v  $*/$*.hs $(MACRELEASEBUILDFLAGS) -o bin/$$BINARY 2>&1 | egrep "bin/gcc.*bin/$$BINARY"` ; \
# 	PORTABLELINKCMD=`echo $$LINKCMD | sed -e 's/ -framework GMP//'` ; \
# 	echo $$PORTABLELINKCMD; $$PORTABLELINKCMD

# windowsbinaries: install \
# 	$(call def-help,windowsbinaries,\
# 	Run this on a windows machine or in a wine session, and probably in a\
# 	separate copy of the repo (hledger-win).\
# 	Builds and gather deployable binaries for windows, if cygwin tools are\
# 	present and all packages are buildable. Otherwise, cabal install each\
# 	package and gather the binaries by hand.\
# 	)
# 	cp ~/.cabal/bin/hledger.exe bin/`echo $(BINARYFILENAME) | dos2unix`
# 	-cp ~/.cabal/bin/hledger-web.exe bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-web/' | dos2unix`
# 	@echo 'Please check the binaries are portable, then make compressbinaries'
# 	ls -l bin/*`arch`

# wine-cmd: \
# 	$(call def-help,wine-cmd,\
# 	various ways of getting a wine shell\
# 	command-line windows command prompt. Works eg in an emacs shell buffer.\
# 	)
# 	wine cmd

# wine-cmd2: \
# 	$(call def-help,wine-cmd2,\
# 	as above but try to cd somewhere useful (doesnt work), also ctrl-d exits quickly\
# 	)
# 	(echo c:; echo cd \\mingw\\msys\\1.0; cat) | wine cmd

# wine-cmd-window: \
# 	$(call def-help,wine-cmd-window,\
# 	windows command prompt in a new window\
# 	)
# 	wineconsole cmd &

# wine-mintty: \
# 	$(call def-help,wine-mintty,\
# 	msys bash shell in a mintty window\
# 	)
# 	wine c:/mingw/msys/1.0/bin/mintty - &

# compressbinaries: \
# 	$(call def-help,compressbinaries,\
# 	\
# 	)
# 	cd bin; for f in *-windows-*.exe ; do echo zipping $$f; rm -f $$f.zip; zip $$f.zip $$f; done
# #	for f in bin/*-{linux,mac-}* ; do echo gzipping $$f; gzip -q $$f >$$f.gz; done
# #	gzip bin/*`arch`

# tools/unittest: tools/unittest.hs \
# 	$(call def-help,tools/unittest,\
# 	build the standalone unit test runner. Requires test-framework, which\
# 	may not work on windows.\
# 	)
# 	$(GHC) -threaded -O2 tools/unittest.hs

# tools/doctest: tools/doctest.hs \
# 	$(call def-help,tools/doctest,\
# 	build the doctest runner\
# 	)
# 	$(GHC) tools/doctest.hs

# tools/criterionbench: tools/criterionbench.hs \
# 	$(call def-help,tools/criterionbench,\
# 	build the criterion-based benchmark runner. Requires criterion.\
# 	)
# 	$(GHC) tools/criterionbench.hs

# tools/progressionbench: tools/progressionbench.hs \
# 	$(call def-help,tools/progressionbench,\
# 	build the progression-based benchmark runner. Requires progression.\
# 	)
# 	$(GHC) tools/progressionbench.hs

tools/generatejournal: tools/generatejournal.hs \
		$(call def-help,tools/generatejournal, build the generatejournal tool )
	$(GHC) tools/generatejournal.hs

ghcid: $(call def-help,ghcid, start ghcid autobuilder on hledger-lib + hledger)
	ghcid -c 'make ghci'

ghcid-ui: $(call def-help,ghcid-ui, start ghcid autobuilder on hledger-lib + hledger + hledger-ui)
	ghcid -c 'make ghci-ui'

ghcid-web: $(call def-help,ghcid-web, start ghcid autobuilder on hledger-lib + hledger + hledger-web)
	ghcid -c 'make ghci-web'

ghcid-api: $(call def-help,ghcid-api, start ghcid autobuilder on hledger-lib + hledger + hledger-api)
	ghcid -c 'make ghci-api'

ghcid-test: $(call def-help,ghcid-test, start ghcid autobuilding and running the test command)
	ghcid -c 'make ghci' --test ':main test'

ghcid-test-%: $(call def-help,ghcid-test-TESTPATTERN, start ghcid autobuilding and running the test command with the given TESTPATTERN)
	ghcid -c 'make ghci' --test ':main test $*'

ghcid-doctest: $(call def-help,ghcid-doctest, start ghcid autobuilding and running hledger-lib doctests)
	ghcid -c 'cd hledger-lib; $(STACK) ghci hledger-lib:test:doctests' --test ':main' --reload hledger-lib

ghcid-shake: $(call def-help,ghcid-shake, start ghcid autobuilder on Shake.hs)
	stack exec \
		--package base-prelude \
		--package directory \
		--package extra \
		--package safe \
		--package shake \
		--package time \
		-- ghcid Shake.hs
# same packages as in Shake.hs


# multi-package GHCI prompts
ghci: $(call def-help,ghci, start ghci REPL on hledger-lib + hledger)
	$(STACK) exec -- $(GHCI) $(BUILDFLAGS) hledger/Hledger/Cli/Main.hs

ghci-prof: $(call def-help,ghci-prof, start ghci REPL on hledger-lib + hledger with profiling information)
	stack build --profile hledger --only-dependencies
	$(STACK) exec -- $(GHCI) $(BUILDFLAGS) -fexternal-interpreter -prof -fprof-auto hledger/Hledger/Cli/Main.hs

ghci-dev: $(call def-help,ghci-dev, start ghci REPL on hledger-lib + hledger + dev.hs script)
	$(STACK) exec -- $(GHCI) $(BUILDFLAGS) -fno-warn-unused-imports -fno-warn-unused-binds dev.hs

ghci-ui: $(call def-help,ghci-ui, start ghci REPL on hledger-lib + hledger + hledger-ui)
	$(STACK) exec -- $(GHCI) $(BUILDFLAGS) hledger-ui/Hledger/UI/Main.hs

ghci-web: link-web-dirs $(call def-help,ghci-web, start ghci REPL on hledger-lib + hledger + hledger-web)
	$(STACK) exec -- $(GHCI) $(BUILDFLAGS) hledger-web/app/main.hs

ghci-api: $(call def-help,ghci-api, start ghci REPL on hledger-lib + hledger + hledger-api)
	$(STACK) exec -- $(GHCI) $(BUILDFLAGS) hledger-api/hledger-api.hs

# ghci-all: $(call def-help,ghci-all, start ghci REPL on all the hledger)
# 	$(STACK) exec -- $(GHCI) $(BUILDFLAGS) \
# 		hledger-ui/Hledger/UI/Main.hs \
# 		hledger-web/app/main.hs \
# 		hledger-api/hledger-api.hs \

ghci-doctest: $(call def-help,ghci-doctest, start ghci REPL on hledger-lib doctests)
	cd hledger-lib; $(STACK) ghci hledger-lib:test:doctests


###############################################################################
$(call def-help-subheading,TESTING:)

test: pkgtest functest \
	$(call def-help,test, run default tests: package tests plus functional tests)

# For quieter tests add --silent. It may hide troubleshooting info.
# For very verbose tests add --verbosity=debug. It seems hard to get something in between.
STACKTEST=$(STACK) test

buildplantest: $(call def-help,buildplantest, stack build --dry-run all hledger packages ensuring an install plan with default snapshot) \
	buildplantest-stack.yaml

buildplantest-all: $(call def-help,buildplantest-all, stack build --dry-run all hledger packages ensuring an install plan with each ghc version/stackage snapshot )
	for F in stack-*.yaml stack.yaml; do make --no-print-directory buildplantest-$$F; done

buildplantest-%: $(call def-help,buildplantest-STACKFILE, stack build --dry-run all hledger packages ensuring an install plan with the given stack yaml file; eg make buildplantest-stack-ghc8.2.yaml )
	$(STACK) build --dry-run --test --bench --stack-yaml=$*

buildtest: $(call def-help,buildtest, force-rebuild all hledger packages/modules quickly ensuring no warnings with default snapshot) \
	buildtest-stack.yaml

buildtest-all: $(call def-help,buildtest-all, force-rebuild all hledger packages/modules quickly ensuring no warnings with each ghc version/stackage snapshot )
	for F in stack-*.yaml stack.yaml; do make --no-print-directory buildtest-$$F; done

buildtest-%: $(call def-help,buildtest-STACKFILE, force-rebuild all hledger packages/modules quickly ensuring no warnings with the given stack yaml file; eg make buildtest-stack-ghc8.2.yaml )
	$(STACK) build --test --bench --fast --force-dirty --ghc-options=-fforce-recomp --ghc-options=-Werror --stack-yaml=$*

incr-buildtest: $(call def-help,incr-buildtest, build any outdated hledger packages/modules quickly ensuring no warnings with default snapshot. Wont detect warnings in up-to-date modules.) \
	incr-buildtest-stack.yaml

incr-buildtest-all: $(call def-help,incr-buildtest-all, build any outdated hledger packages/modules quickly ensuring no warnings with each ghc version/stackage snapshot. Wont detect warnings in up-to-date modules. )
	for F in stack-*.yaml stack.yaml; do make --no-print-directory incr-buildtest-$$F; done

incr-buildtest-%: $(call def-help,incr-buildtest-STACKFILE, build any outdated hledger packages/modules quickly ensuring no warnings with the stack yaml file; eg make buildtest-stack-ghc8.2.yaml. Wont detect warnings in up-to-date modules. )
	$(STACK) build --test --bench --fast --ghc-options=-Werror --stack-yaml=$*

pkgtest: $(call def-help,pkgtest, run the test suites in each package )
	@($(STACKTEST) && echo $@ PASSED) || (echo $@ FAILED; false)

# doctest with ghc 8.4 on mac requires a workaround, see hledger-lib/package.yaml.
# Or, could run it with ghc 8.2: 
#	@($(STACKTEST) --stack-yaml stack-ghc8.2.yaml hledger-lib:test:doctests && echo $@ PASSED) || (echo $@ FAILED; false)
doctest: $(call def-help,doctest, run the doctests in hledger-lib module/function docs )
	@($(STACKTEST) hledger-lib:test:doctests && echo $@ PASSED) || (echo $@ FAILED; false)

easytest: $(call def-help,easytest, run the easytest unit tests in hledger-lib )
	@($(STACKTEST) hledger-lib:test:easytests && echo $@ PASSED) || (echo $@ FAILED; false)

# assumes an up to date hledger executable is built.
# I think we don't do it automatically to minimise unnecessary rebuilding.
builtintest: $(call def-help,builtintest, run hledgers built in test command)
	@($(STACK) exec hledger test && echo $@ PASSED) || (echo $@ FAILED; false)

#functest: addons tests/addons/hledger-addon 
functest: tests/addons/hledger-addon \
	$(call def-help,functest, build hledger quickly and run the functional tests (and some unit tests) )
	@stack build --fast hledger
	@($(SHELLTESTSTK) -w `stack exec -- which hledger` tests \
		&& echo $@ PASSED) || (echo $@ FAILED; false)

ADDONEXTS=pl py rb sh hs lhs rkt exe com bat
tests/addons/hledger-addon: \
	$(call def-help,tests/addons/hledger-addon,\
	generate dummy add-ons for testing (hledger-addon the rest)\
	)
	rm -rf tests/addons/hledger-*
	printf '#!/bin/sh\necho add-on: $$0\necho args: $$*\n' >tests/addons/hledger-addon
	for E in '' $(ADDONEXTS); do \
		cp tests/addons/hledger-addon tests/addons/hledger-addon.$$E; done
	for F in addon. addon2 addon2.hs addon3.exe addon3.lhs addon4.exe add reg; do \
		cp tests/addons/hledger-addon tests/addons/hledger-$$F; done
	mkdir tests/addons/hledger-addondir
	chmod +x tests/addons/hledger-*

# hlinttest hlint: $(call def-help,hlinttest (or hlint),generate a hlint report)
# 	hlint --hint=hlint --report=hlint.html $(SOURCEFILES)

haddocktest: $(call def-help,haddocktest, run haddock to make sure it can generate docs without dying )
	@(make --quiet haddock && echo $@ PASSED) || (echo $@ FAILED; false)

cabalfiletest: $(call def-help,cabalfiletest, run cabal check to test cabal file syntax )
	@(make --no-print-directory cabalcheck && echo $@ PASSED) || (echo $@ FAILED; false)

allghcstest: $(call def-help,allghcstest, build/test/benchmark with all supported GHC versions/stackage snapshots and warning-free) \
	test-stack-ghc7.10.yaml \
	test-stack-ghc8.0.yaml \
	test-stack-ghc8.2.yaml \
	test-stack.yaml \

test-stack%yaml:
	$(STACK) --stack-yaml stack$*yaml clean
	$(STACK) --stack-yaml stack$*yaml build --ghc-options="$(WARNINGS) -Werror" --test --bench --haddock --no-haddock-deps

travistest: $(call def-help,travistest, run tests similar to our travis CI tests)
	$(STACK) clean
	$(STACK) build --ghc-options=-Werror --test --haddock --no-haddock-deps hledger-lib
	$(STACK) build --ghc-options=-Werror --test --haddock --no-haddock-deps hledger
	$(STACK) build --ghc-options=-Werror --test --haddock --no-haddock-deps hledger-ui
	$(STACK) build --ghc-options=-Werror --test --haddock --no-haddock-deps hledger-web
	$(STACK) build --ghc-options=-Werror --test --haddock --no-haddock-deps hledger-api
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
	examples/100x100x10.journal \
	examples/1000x1000x10.journal \
	examples/1000x10000x10.journal \
	examples/10000x1000x10.journal \
	examples/10000x10000x10.journal \
	examples/100000x1000x10.journal \
	examples/1000000x1000x10.journal \
	examples/ascii.journal \
	examples/chinese.journal \
	examples/mixed.journal \

examples/sample.journal:
	true # XXX should probably regenerate this

examples/100x100x10.journal: tools/generatejournal
	tools/generatejournal 100 100 10 >$@

examples/1000x1000x10.journal: tools/generatejournal
	tools/generatejournal 1000 1000 10 >$@

examples/1000x10000x10.journal: tools/generatejournal
	tools/generatejournal 1000 10000 10 >$@

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

BENCHEXES=hledger
# or, eg: BENCHEXES=ledger,hledger-1.4,hledger  

bench: samplejournals bench.sh $(call def-help,bench, benchmark commands in bench.sh with quickbench and $(BENCHEXES))
	quickbench -v -w $(BENCHEXES)

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
		$(call def-help,quickprof-"CMD", run some command against a sample journal and display the execution profile )
	$(STACK) exec -- hledgerprof +RTS $(PROFRTSFLAGS) -RTS $* -f examples/10000x1000x10.journal >/dev/null
	profiteur hledgerprof.prof
	@echo
	@head -20 hledgerprof.prof
	@echo ...
	@echo
	@echo see hledgerprof.prof or hledgerprof.prof.html for the full report

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

# # XXX with a sandbox, use sandbox-repl-* instead
# repl-lib:\
# 	$(call def-help,repl-lib, start a cabal REPL and load the hledger-lib package)
# 	(cd hledger-lib; cabal repl)

# repl-cli:\
# 	$(call def-help,repl-cli, start a cabal REPL and load the hledger package)
# 	(cd hledger; cabal repl exe:hledger)

# repl-web:\
# 	$(call def-help,repl-web, start a cabal REPL and load the hledger-web package)
# 	(cd hledger-web; cabal repl exe:hledger-web)

###############################################################################
$(call def-help-subheading,DOCUMENTATION:)

# docs: site codedocs \
# 	$(call def-help,docs,\
# 	rebuild all docs\
# 	)

# cleandocs: site-clean \
# 	$(call def-help,cleandocs,\
# 	\
# 	)

hakyll-std site/hakyll-std/hakyll-std: \
	site/hakyll-std/hakyll-std.hs \
	site/hakyll-std/TableOfContents.hs \
		$(call def-help,hakyll-std, build a generic hakyll site builder script )
	cd site/hakyll-std; ./hakyll-std.hs

site-build: site/hakyll-std/hakyll-std site/manual.md \
	$(call def-help,site-build, generate the hledger.org website with hakyll-std )
	-cd site; hakyll-std/hakyll-std build

site/index.md: wiki/_Sidebar.md \
	$(call def-help,site/index.md, update home page with ./wiki/_Sidebar content )
	(sed -ne '1,/<!-- WIKICONTENT -->/ p'     site/index.md     ; \
	 sed -ne '/^#.*User/,$$ p'                 wiki/_Sidebar.md \
	 | perl -p \
			-e 's/\[\[([^\|]*)\|([^\]]*)\]\]/[\1](https:\/\/github.com\/simonmichael\/hledger\/wiki\/\2)/g;' \
		  -e 's/\[\[([^\]]*)\]\]/[\1](https:\/\/github.com\/simonmichael\/hledger\/wiki\/\1)/g;' \
		  -e 's/^# >/##/;' \
	 ; \
	 sed -ne '/<!-- ENDWIKICONTENT -->/,$$ p' site/index.md     ) \
	> site/_index.md.$$$$ && \
	mv site/_index.md.$$$$ site/index.md

site/index.md-push: \
	$(call def-help,site/index.md-push, update home page with ./wiki/_Sidebar content and commit and do a git push if changed )
	git diff --quiet site/index.md && \
	make -s site/index.md && \
	( git diff --quiet site/index.md || (git commit -q -m 'site: home: update from wiki' -m '[ci skip]' site/index.md && git push) )

site-clean: site/hakyll-std/hakyll-std \
	$(call def-help,site-clean, remove hakyll-generated files (& take down the website) ) #cleanolddocs
	-cd site; hakyll-std/hakyll-std clean
#	rm -rf site/_site/*

# regenerate html whenever markdown files change (and serve it on port 8000)
# XXX hakyll preview/watch often fail to notice changes in large files
# XXX can get confused when docs are generated concurrently by a Shake command
# XXX individual file updates loses the stylesheets for some reason
#     more robust: "ls site/*.md | entr ./Shake website" and reload file:///Users/simon/src/hledger/site/_site/docs.html
site-preview: site/hakyll-std/hakyll-std \
	$(call def-help,site-preview, run a hakyll server to preview the website  ) #site/site
	-cd site; hakyll-std/hakyll-std watch # -h hledger.org

# open a browser on the latest site html and reload the page whenever it changes
site-reload:
	(sleep 1; open http://localhost:8001) &
	livereloadx -p 8001 --static site/_site

# site-view: site \
# 	$(call def-help,site-view,\
# 	\
# 	)
# 	$(VIEWHTML) site/_site/index.html

# # site-auto:
# # 	cd site; $(AUTOBUILD) site.hs -o site $(PREFERMACUSRLIBFLAGS) --run preview

# # ensure some old doc versions are in place:

# #olddocs: doc/0.22 doc/0.21

# #doc/0.23:
# #	(cd doc; git archive --prefix doc/0.23/ tags/0.23 'doc/MANUAL.md') | tar xf -

# # doc/0.22:
# # 	git archive --prefix doc/0.22/ tags/0.22 'MANUAL.md' | tar xf -

# # doc/0.21:
# # 	git archive --prefix doc/0.21/ tags/0.21.3 'MANUAL.md' | tar xf -

# # cleanolddocs:
# # 	cd doc; rm -rf 0.22 0.21


# pdf: codepdf \
# 	$(call def-help,pdf,\
# 	\
# 	) #docspdf

# # generate pdf versions of main docs
# # docspdf:
# # 	-for d in $(DOCFILES); do (cd site && ln -sf ../$$d && pandoc $$d -w pdf && rm -f $$d); done

# # # format all code as a pdf for offline reading
# # ENSCRIPT=enscript -q --header='$$n|$$D{%+}|Page $$% of $$=' --highlight=haskell --line-numbers --font=Courier6 --color -o-
# # codepdf:
# # 	$(ENSCRIPT) --pretty-print=makefile hledger.cabal >cabal.ps
# # 	$(ENSCRIPT) --pretty-print=makefile Makefile >make.ps
# # 	$(ENSCRIPT) --pretty-print=haskell $(SOURCEFILES) >haskell.ps
# # 	cat cabal.ps make.ps haskell.ps | ps2pdf - >code.pdf

# # # view all docs and code as pdf
# # PDFS=site/{README,README2,MANUAL,CHANGES,CONTRIBUTORS}.pdf code.pdf
# # viewall: pdf
# # 	$(VIEWPDF) $(PDFS)

# # # print all docs and code for offline reading
# # printall: pdf
# # 	$(PRINT) $(PDFS)

# pushdocs: push \
# 	$(call def-help,pushdocs,\
# 	push latest docs etc. and update the hledger.org site\
# 	)
# 	ssh simon@joyful.com 'make -C/repos/hledger docs'

# EXES=hledger hledger-vty hledger-web
# savehelp: \
# 	$(call def-help,savehelp,\
# 	dump all executables command line help into files for review\
# 	)
# 	for e in $(EXES); do $$e --help >.HELP_$$e; done

# codedocs: haddock hscolour coverage \
# 	$(call def-help,codedocs,\
# 	generate api & other code docs\
# 	) #sourcegraph #hoogle

# cf http://www.haskell.org/haddock/doc/html/invoking.html
# --ghc-options=-optP-P is a workaround for http://trac.haskell.org/haddock/ticket/284
HADDOCKFLAGS= \
	--haddock-options='--no-warnings' \
	--ghc-options='-optP-P' \

haddock: \
	$(call def-help,haddock, generate haddock docs for the hledger packages )
	$(STACK) haddock --no-haddock-deps --no-keep-going # && echo OK
#	$(STACK) -v haddock --no-haddock-deps --no-keep-going # && echo OK

# view-haddock: \
# 	$(call def-help,view-haddock-cli,\
# 	view the haddock generated for the hledger package\
# 	)
# 	$(VIEWHTML) hledger/dist/doc/html/hledger/index.html

# # http://www.cs.york.ac.uk/fp/darcs/hscolour/
# HSCOLOUR=HsColour -css
# hscolour: site/api/src site/api/src/hscolour.css \
# 	$(call def-help,hscolour,\
# 	\
# 	)
# 	for f in $(HADDOCKSOURCEFILES); do \
# 		$(HSCOLOUR) -anchor $$f -osite/api/src/`echo $$f | sed -e's%[^/]*/%%' | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
# 	done

# site/api/src/hscolour.css: site/api/src \
# 	$(call def-help,site/api/src/hscolour.css,\
# 	\
# 	)
# 	$(HSCOLOUR) -print-css >site/api/src/hscolour.css

# site/api/src: \
# 	$(call def-help,site/api/src,\
# 	\
# 	)
# 	mkdir -p site/api/src

# sourcegraph: \
# 	$(call def-help,sourcegraph,\
# 	\
# 	)
# 	for p in $(PACKAGES); do (cd $$p; SourceGraph $$p.cabal); done

# # # generate external api docs for each package
# # allhaddock: cabalhaddock\ --hyperlink-source\ --executables

# # # generate internal code docs for each package
# # allhaddockinternal: cabalhaddock\ --hyperlink-source\ --executables\ --internal

# # # generate hoogle indices for each package
# # allhoogle: cabalhaddock\ --hoogle\ --executables

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

#

# in subsequent rules, allow automatic variables to be used in prerequisites (use $$)
.SECONDEXPANSION:

########################
# changelogs

LASTTAG=$(shell git describe --tags --abbrev=0)

# pre 2017:
## The last git revision referenced in the change notes. 
## (Or, if there are no change notes, the last tag.
## Tries hard to be warning free and run shell commands only when needed.)
#CHANGENOTESLASTREV=$(subst %,,$(subst %%,$(LASTTAG),%$(shell [ -f $(CHANGENOTES) ] && (grep -E '^[a-f0-9]{8}$$' $(CHANGENOTES) | tail -1) )%))
#
## create change notes file if it doesn't exist.. shouldn't happen much.
#$(CHANGENOTES):
#	@make changenotes-start
#
#changenotes-template:
#	@(\
#		echo "* change notes for hledger-$(VERSION)" ;\
#		echo "** hledger-lib" ;\
#		echo "** hledger" ;\
#		echo "** hledger-ui" ;\
#		echo "** hledger-web" ;\
#		echo "** hledger-api" ;\
#		echo "** hledger-install" ;\
#		echo "** project" ;\
#		echo "** unsorted" ;\
#		)
#
#changenotes-start: \
#		$(call def-help,changenotes-start, add a new outline named after $(VERSIONFILE) in $(CHANGENOTES). Run after bumping $(VERSIONFILE). )
#	@make changenotes-template >>$(CHANGENOTES)
#
## TODO problem: this only checks the last (bottom-most) git rev in the
## change notes file, so after manual editing has begun this may re-add
## commits which are already in the file.
#changenotes-update: $(CHANGENOTES) \
#		$(call def-help,changenotes-update, add any not-yet-added(*) commits to $(CHANGENOTES). Run periodically (* it may get this wrong once manual editing has begun). )
#	@make changenotes-show-from-$(CHANGENOTESLASTREV) >>$(CHANGENOTES)
#	@echo "Latest items in $(CHANGENOTES):"
#	@make changenotes-show | tail -10
#	@make changenotes-show-last
#
#changenotes-update-from-%: \
##		$(call def-help,changenotes-update-from-REV, add commits from this git revision onward to $(CHANGENOTES) )
#	@make changenotes-show-from-$* >>$(CHANGENOTES)
#
#changenotes-show-from-%: \
##		$(call def-help,changenotes-show-from-REV, show commits from this git revision onward as org nodes )
#	@git log --abbrev-commit --reverse --pretty=format:'ORGNODE %s (%an)%n%b%h' $*.. \
#		| sed -e 's/^\*/-/' -e 's/^ORGNODE/***/' \
#		| sed -e 's/ (Simon Michael)//'
#
#changenotes-show: $(CHANGENOTES) \
#		$(call def-help,changenotes-show, show all the org headlines recorded in $(CHANGENOTES) )
#	@cat $(CHANGENOTES) | grep '*'
#
## git l is a local alias with output like "2017-08-16 0a0e6d18 tools: make help-SECTION (HEAD -> master)"
#changenotes-show-last: $(CHANGENOTES) \
#		$(call def-help,changenotes-show-last, show the last commit recorded in $(CHANGENOTES) )
#	@git l -1 $(CHANGENOTESLASTREV)


# 2017-2018:
# At release time, in each package dir, dump commit log into CHANGES.org, edit and move to CHANGES. Eg:
#  export FROM=hledger-1.11; make changes-show-$FROM >CHANGES.org; for p in  hledger-lib hledger hledger-ui hledger-web hledger-api; do (cd $p; make -f../Makefile changes-show-$FROM >CHANGES.org); done
# where FROM could be a branch name (1.11) or a specific release tag (hledger-1.11.1, more precise)

#only works in top dir, use changes-show-TAG instead
# changes-show: $(call def-help,changes-show, show commits affecting the current directory excluding any hledger package subdirs from the last tag as org nodes newest first )
# 	@make changes-show-from-$(LASTTAG)

# --abbrev-commit shortens commit hashes. --pretty sets org-like output format.
# ORGNODE stands in for * until any * list bullets in commit messages have been rewritten.
# %s summary, %an author name, %n newline if needed?, %b long description, %h hash
# :! args are exclude pathspecs, to exclude package dirs when running in top dir.
#  https://git-scm.com/docs/gitglossary.html#gitglossary-aiddefpathspecapathspec
# changes-show-%: #$(call def-help,changes-show-from-REV, show commits affecting the current directory excluding any hledger package subdirs from this git revision onward as org nodes newest first )
# 	@git log \
# 		--abbrev-commit --pretty=format:'ORGNODE %s (%an)%n%b' $*.. \
# 		--stat \
# 		-- . ':!hledger-lib' ':!hledger' ':!hledger-ui' ':!hledger-web' ':!hledger-api' \
# 	| sed \
# 		-e 's/^\*/-/' \
# 		-e 's/^ORGNODE/*/' \
# 		-e 's/ (Simon Michael)//' \
# 		-e 's/\[ci skip\]//' \


# 2018-2019:
# goal: periodically, update all changelogs in place and save last seen commit

#changes-update:

changes:
	make changes-top >CHANGES.org
	make changes-lib >hledger-lib/CHANGES.org
	make changes-cli >hledger/CHANGES.org
	make changes-ui  >hledger-ui/CHANGES.org
	make changes-web >hledger-web/CHANGES.org
	make changes-api >hledger-api/CHANGES.org

# :! args are exclude pathspecs, https://git-scm.com/docs/gitglossary.html#gitglossary-aiddefpathspecapathspec
EXCLUDEPKGDIRS=\
	':!hledger-lib' \
	':!hledger' \
	':!hledger-ui' \
	':!hledger-web' \
	':!hledger-api' \

# -E for extended regular expressions
# ensure bullet lists in descriptions use hyphens not stars
# convert ORGNODE placeholders to stars
# strip most PKG: prefixes
# strip maintainer's author name
# strip [ci skip] lines
# replace consecutive newlines with one
# indent long descriptions
# TODO: can't edit this with IDEA right now, it rewrites the ^M
CLEANUPCHANGES=sed -E \
		-e 's/^( )*\*/\1-/' \
		-e 's/^ORGNODE/*/' \
		-e 's/^\* $(PKGPREFIX): /* /' \
		-e 's/ \(Simon Michael\)//' \
		-e 's///' \
		-e 's/\[ci skip\]//' \
		-e '/./,/^$$/!d' \
		-e 's/^([^\*])/  \1/' \

# --abbrev-commit shortens commit hashes
GITLOG=git log --abbrev-commit

# verbose org-like changelog format, including hashes and --stat info for troubleshooting.
# ORGNODE stands in for * until any * list bullets in commit messages have been rewritten.
# %s=summary, %an=author name, %n=newline if needed, %b=long description, %h=hash
VERBOSEFMT=--pretty=format:'ORGNODE %s (%an)  %n%b%h' --stat

changes-%-verbose: $(call def-help,changes-PKGID-verbose, show commits since the rev in PKGDIR/.CHANGES.seen in PKGDIR as verbose org nodes )
	$(eval PKGID=$*)
	$(eval PKGDIR=$(subst -cli,,hledger-$(PKGID)))
	$(eval PKGPREFIX=$(shell echo $(PKGDIR) | sed -e s/hledger-// -e s/^hledger$$/cli/))
	$(eval REV=$(shell cat $(PKGDIR)/.CHANGES.seen))
	@$(GITLOG) $(VERBOSEFMT) $(REV).. -- $(PKGDIR) | $(CLEANUPCHANGES)

changes-top-verbose: $(call def-help,changes-top-verbose, show commits since the rev in .CHANGES.seen excluding hledger package subdirs as verbose org nodes )
	$(eval REV=$(shell cat .CHANGES.seen))
	@$(GITLOG) $(VERBOSEFMT) $(REV).. -- . $(EXCLUDEPKGDIRS) | $(CLEANUPCHANGES)

# org-like changelog format suitable for changelogs/release notes
CHANGELOGFMT=--pretty=format:'ORGNODE %s (%an)  %n%b'

changes-%: $(call def-help,changes-PKGDIR, show commits since the rev in PKGDIR/.CHANGES.seen in PKGDIR as changelog-ready org nodes )
	$(eval PKGID=$*)
	$(eval PKGDIR=$(subst -cli,,hledger-$(PKGID)))
	$(eval PKGPREFIX=$(shell echo $(PKGDIR) | sed -e s/hledger-// -e s/^hledger$$/cli/))
	$(eval REV=$(shell cat $(PKGDIR)/.CHANGES.seen))
	@$(GITLOG) $(CHANGELOGFMT) $(REV).. -- $(PKGDIR) | $(CLEANUPCHANGES)

changes-top: $(call def-help,changes-top, show commits since the rev in .CHANGES.seen excluding hledger package subdirs as changelog-ready org nodes )
	$(eval REV=$(shell cat .CHANGES.seen))
	@$(GITLOG) $(CHANGELOGFMT) $(REV).. -- . $(EXCLUDEPKGDIRS) | $(CLEANUPCHANGES)


###############################################################################
$(call def-help-subheading,RELEASING:)
#$(call def-help-subheading,see also developer guide -> how to -> do a release)

# TODO update this:

# # Version numbering. See also .version and Version.hs.
# #
# # hledger's version number appears in:
# #  hledger --version
# #  hledger's cabal file
# #  darcs tags
# #  hackage tarball filenames
# #  hackage pages
# #
# # Some old version numbering goals:
# # 1 automation, robustness, simplicity, platform independence
# # 2 cabal versions must be all-numeric
# # 3 release versions can be concise (without extra .0's)
# # 4 releases should have a corresponding darcs tag
# # 5 development builds should have a precise version appearing in --version
# # 6 development builds should generate cabal packages with non-confusing versions
# # 7 there should be a way to mark builds/releases as alpha or beta
# # 8 it should be easy to darcs get the .0 release even after bugfix releases
# # 9 avoid unnecessary compiling and linking
# # 10 minimise rcs noise and syncing issues (commits, unrecorded changes)
# #
# # Current policy:
# #
# # - We follow http://haskell.org/haskellwiki/Package_versioning_policy
# #
# # - The full release version is ma.jor.minor, where minor is 0 for a
# #   normal release or 1..n for bugfix releases.
# #
# # - The elided release version is ma.jor when minor is 0. We use it for
# #   hackage releases when possible, trusting it doesn't cause trouble..
# #
# # - The build version is ma.jor.minor+patches, where patches is the number
# #   of patches applied in the current repo since the last release tag.
# #
# # - The release tag in the repo is the full release version.
# #
# # - hledger --version shows the release version or build version as
# #   appropriate.
# #
# # - The .version file must be updated manually before a release.
# #
# # - "make" updates the version in most other places, and defines PATCHES.
# #   Note "cabal build" should also do this but doesn't yet.
# #
# # - "make release" additionally records the main version number-affected
# #   files, and tags the repo with the release tag.

# TODO revive these:

# old examples: 
# a normal release: echo 0.7   >.version; make release
# a bugfix release: echo 0.7.1 >.version; make release
#release: releasetest bumpversion tagrelease $(call def-help,release, prepare a release and tag the repo )

#publish: hackageupload pushdocs $(call def-help,upload, publish latest hackage packages and docs )

#releaseandpublish: release upload $(call def-help,releaseandupload, prepare a release and publish )

ISCLEAN=git diff-index --quiet HEAD --

# stop if the working directory has uncommitted changes
iscleanwd:
	@$(ISCLEAN) || (echo "please clean the working directory first"; false)

# stop if the given file(s) have uncommitted changes
isclean-%:
	@$(ISCLEAN) $* || (echo "please clean these files first: $*"; false)

#	@$(ISCLEAN) doc/lib.m4 || (echo "please clean doc/lib.m4 first"; false)
setdate: $(call def-help,setdate, set date in manuals to current month and year )
	perl -pe "s/^(m4_define\({{_monthyear_}}, *{{)[^}]*(}}\)m4_dnl *)$$/\$${1}$(MONTHYEAR)\$${2}/" -i doc/lib.m4

updatedate: setdate $(call def-help,updatedate, set date in manuals to current month and year and commit )
	git commit -m "bump manual date to $(MONTHYEAR)" doc/lib.m4

# update a package yaml file's version, -DVERSION, and hledger lower bounds (upper bounds must be changed manually)
%/package.yaml: $(VERSIONFILE)
	perl -pe "s/(^version *: *).*/\$${1}'$(VERSION)'/" -i $@                                                       # version: 'A'
	perl -pe "s/(-DVERSION=\")[^\"]+/\$${1}$(VERSION)/" -i $@                                                      # -DVERSION="A"
	perl -pe "s/(hledger(-\w+)?) *== *((\d+\.)*\d+) *$$/\$$1 ==$(VERSION)/" -i $@                                  # hledgerX == A
	perl -pe "s/(hledger(-\w+)?) *>=? *((\d+\.)*\d+) *$$/\$$1 >=$(VERSION)/" -i $@                                 # hledgerX >= A
	perl -pe "s/(hledger(-\w+)?) *>=? *((\d+\.)*\d+) *&& *< *((\d+\.)*\d+) *$$/\$$1 >=$(VERSION) && <\$$5/" -i $@  # hledgerX >= A && < B

# update hledger-api's version strings
hledger-api/hledger-api.hs: $(VERSIONFILE)
	perl -pe "s/(hledgerApiVersion=)\"((\d+\.)*\d+)\" *$$/\$$1\"$(VERSION)\"/" -i $@
	perl -pe "s/(.*?hledger-api +)((\d+\.)*\d+)(.*)$$/\$${1}$(VERSION)\$$4/" -i $@

# update version string used in generated docs
doc/lib.m4: $(VERSIONFILE)
	perl -pe "s/^(m4_define\({{_version_}}, *{{)((\d+\.)*\d+)(}}\)m4_dnl *)$$/\$${1}$(VERSION)\$${4}/" -i $@
	@echo "please manually check/update _docversionlinks_ in doc/lib.m4"

# XXX obsolete ?
# update version string in manual
#site/manual-start.md: $(VERSIONFILE)
#	perl -p -e "s/(this version documents hledger and hledger-web) +[0-9.]+/\1 $(VERSION)/" -i $@

# XXX start with early targets isclean-$(VERSIONSENSITIVEFILES) (fails due to glob) and isdirty-$(VERSIONFILE) ?
setversion: $(VERSIONSENSITIVEFILES) #$(call def-help,setversion, update version strings & bounds from $(VERSIONFILE) (might need -B) )
	@echo "if this is a new major version, please manually update upper bounds in */package.yaml before generating cabal files"

# TODO: combine updateversion/updatecabal
# minor version bump: make setdate setversion gencabal
# major version bump: make setdate setversion, fix upper bounds in package.yamls, make gencabal

# updateversion: setdate setversion $(call def-help,updateversion, update manual date and update version strings & (lower) bounds from $(VERSIONFILE) and commit )
# 	@read -p "please review changes then press enter to commit $(VERSIONFILE) $(VERSIONSENSITIVEFILES)"
# 	git commit -m "bump version strings & bounds to $(VERSION)" $(VERSIONFILE) $(VERSIONSENSITIVEFILES)

# (re)generate a cabal file from its package.yaml definition 
# XXX to avoid warnings, this hpack should be the same version as stack's built-in hpack
#%.cabal: $$(dir $$@)package.yaml
#	hpack --silent $(dir $*) 
#
gencabal: $(call def-help,gencabal, regenerate cabal files from package.yaml files with stack )
	$(STACK) build --dry-run --silent --stack-yaml stack-ghc8.2.yaml

gencabal-with-hpack-%:
	$(STACK) build --with-hpack hpack-$* --dry-run --silent --stack-yaml stack-ghc8.2.yaml

# updatecabal: gencabal $(call def-help,updatecabal, regenerate cabal files and commit )
# 	@read -p "please review changes then press enter to commit $(shell ls */*.cabal)"
# 	git commit -m "update cabal files" $(shell ls */*.cabal)

# we call in shake for this job; so dependencies aren't checked here
genmanuals: Shake #$(call def-help,genmanuals, regenerate embedded manuals (might need -B) )
	./Shake manuals

updatemanuals: genmanuals $(call def-help,updatemanuals, regenerate embedded manuals and commit (might need -B) )
	@read -p "please review changes then press enter to commit $(shell ls hledger*/hledger*.{1,5,info,txt})"
	git commit -m "update embedded manuals" hledger*/hledger*.{1,5,info,txt}


tagrelease: \
	$(call def-help,tagrelease, commit a release tag based on $(VERSIONFILE) for each package )
	for p in $(PACKAGES); do git tag -f $$p-$(VERSION); done

# hackageupload-dry: \
# 	$(call def-help,hackageupload-dry,\
# 	upload all packages to hackage; dry run\
# 	)
# 	for p in $(PACKAGES); do cabal upload $$p/dist/$$p-$(VERSION).tar.gz -v2 --check; done

hackageupload: \
	$(call def-help,hackageupload, upload all packages to hackage	)
	for p in $(PACKAGES); do stack upload $$p; done

# RSYNC=rsync

# pushprofs: \
# 	$(call def-help,pushprofs,\
# 	push any new profiles and benchmark results to the public site\
# 	beware, results will vary depending on which machine generated them\
# 	)
# 	$(RSYNC) -azP doc/profs/ simon@joyful.com:/repos/hledger/doc/profs/

# pullprofs: \
# 	$(call def-help,pullprofs,\
# 	fetch any new profiles and benchmark results from the public site\
# 	)
# 	$(RSYNC) -azP simon@joyful.com:/repos/hledger/doc/profs/ doc/profs/

# compressbinary: \
# 	$(call def-help,compressbinary,\
# 	compress the just-built platform binary. make hledgerPLATFORM first. Use\
# 	the win variant on windows.\
# 	)
# 	gzip -9 bin/$(BINARYFILENAME)

# compressbinarywin: \
# 	$(call def-help,compressbinarywin,\
# 	\
# 	)
# 	cd bin; zip -9 $(BINARYFILENAME).zip $(BINARYFILENAME)

# # push the last-updated platform binary to the public download directory
# # pushlatestbinary:
# # 	cd bin; $(RSYNC) -aP `ls -t | head -2` simon@joyful.com:/repos/hledger/site/download/


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

###############################################################################
$(call def-help-subheading,MISCELLANEOUS:)

watch-%: $(call def-help,watch-RULE, run make RULE repeatedly when any committed file changes)
	 @git ls-files | entr -r make $*

Shake: Shake.hs $(call def-help,Shake, ensure the Shake script is compiled )
	./Shake.hs

cabal%: \
	$(call def-help,cabalCMD, run cabal CMD inside each hledger package directory )
	for p in $(PACKAGES); do (cd $$p; cabal $*); done

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

tag: emacstags-ctags \
	$(call def-help,tag, generate tag files for source code navigation (for emacs) )

# Tag haskell files with hasktags and just list the other main source files
# so they will be traversed by tags-search/tags-query-replace.
emacstags:
	rm -f TAGS
	hasktags -e $(SOURCEFILES)
	for f in Makefile $(WEBCODEFILES) $(HPACKFILES) $(CABALFILES) $(DOCSOURCEFILES); do \
		printf "\n$$f,1\n" >> TAGS; \
	done

# Tag haskell files with hasktags and everything else not excluded by .ctags
# with (exuberant) ctags.
emacstags-ctags:
	hasktags -e $(SOURCEFILES)
	ctags -a -e -R  

cleantags: \
	$(call def-help-hide,cleantags, remove tag files )
	rm -f TAGS tags

stackclean: \
	$(call def-help-hide,stackclean, remove .stack-work/* in packages (but not in project) )
	$(STACK) clean

Stackclean: \
	$(call def-help-hide,Stackclean, remove all stack working dirs )
	$(STACK) clean

cleanghco: \
	$(call def-help-hide,cleanghc, remove ghc build leftovers )
	rm -rf `find . -name "*.o" -o -name "*.hi" -o -name "*.dyn_o" -o -name "*.dyn_hi" -o -name "*~" | grep -vE '\.(stack-work|cabal-sandbox|virthualenv)'`

cleanghc: cleanghco \
	$(call def-help-hide,cleanghc, remove ghc builds )
	rm -f bin/hledgerdev bin/hledgerdev.ghc*

clean: cleanghco \
	$(call def-help,clean, default cleanup (ghc build leftovers) )

Clean: stackclean cabalclean cleanghc cleantags clean-manpages \
	$(call def-help,Clean, thorough cleanup (stack/cabal/ghc builds and tags) )

# reverse = $(if $(wordlist 2,2,$(1)),$(call reverse,$(wordlist 2,$(words $(1)),$(1))) $(firstword $(1)),$(1))

###############################################################################
# LOCAL UNTRACKED CUSTOMISATIONS

-include local.mk

#$(call def-help-heading,------------------)
