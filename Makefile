# hledger project makefile

# GHC-compiled executables require a locale (and not just C) or they
# will die on encountering non-ascii data.
# Set LANG only if not already set
export LANG?=en_US.UTF-8

# command to run during "make prof" and "make heap"
PROFCMD=bin/hledger-prof balance -f data/1000x1000x10.journal >/dev/null

#PROFRTSFLAGS=-p
PROFRTSFLAGS=-P

# command to run during "make coverage"
COVCMD=test
COVCMD=-f test-wf.csv print

# executables to run during "make simplebench"
BENCHEXES=hledger-0.23.3 hledger

# misc. tools
BROWSE=open
VIEWHTML=$(BROWSE)
VIEWPS=$(BROWSE)
VIEWPDF=$(BROWSE)
PRINT=lpr

GHC=ghc
GHCI=ghci
GHCPKG=ghc-pkg
HADDOCK=haddock
CABAL=cabal
CABALINSTALL=cabal install -w $(GHC)
SHELLTEST=shelltest -j16 --hide-successes
# used for make auto, http://joyful.com/repos/searchpath
SP=sp

PACKAGES=\
	hledger-lib \
	hledger \
	hledger-web

INCLUDEPATHS=\
	-ihledger-lib \
	-ihledger \
	-ihledger-web \
	-ihledger-web/app

MAIN=hledger/app/hledger-cli.hs

# all source files in the project (plus a few strays like Setup.hs & hlint.hs)
SOURCEFILES:= \
	hledger/*hs \
	hledger/Hledger/*hs \
	hledger/Hledger/*/*hs \
	hledger-*/*hs \
	hledger-*/Hledger/*hs \
	hledger-*/Hledger/*/*hs \
	hledger-web/app/*.hs \
	hledger-web/Handler/*.hs \
	hledger-web/Hledger/*.hs \
	hledger-web/Settings/*.hs

# # a more careful list suitable for for haddock-all
# HADDOCKSOURCEFILES:= \
# 	hledger-lib/Hledger.hs \
# 	hledger-lib/Hledger/*hs \
# 	hledger-lib/Hledger/*/*hs \
# 	hledger/Hledger/*hs \
# 	hledger/Hledger/*/*hs \
# 	hledger-web/Application.hs \
# 	hledger-web/Foundation.hs \
# 	hledger-web/Hledger/*hs \
# 	hledger-web/Hledger/*/*hs \
# 	hledger-web/Import.hs \
# 	hledger-web/Settings.hs \
# 	hledger-web/Settings/*hs \
# 	hledger-web/app/*hs \

CABALFILES:= \
	hledger/hledger.cabal \
	hledger-*/*.cabal

WEBFILES:= \
	hledger-web/templates/* \
	hledger-web/static/*.js \
	hledger-web/static/*.css

DOCFILES:= \
	doc/*.md

# files which should be updated when the version changes
VERSIONSENSITIVEFILES=\
	$(CABALFILES) \
	doc/manual.md \

# file(s) which require recompilation for a build to have an up-to-date version string
VERSIONSOURCEFILE=hledger/Hledger/Cli/Version.hs

# master file defining the current release/build version
VERSIONFILE=.version

# two or three-part version string, whatever's in VERSION
VERSION:=$(shell cat $(VERSIONFILE))

# the number of commits since the last tag
PATCHLEVEL:=$(shell git describe --long | awk -F - '{print $$2}')
# the number of commits since the last_release tag
#PATCHLEVEL:=$(shell git rev-list last_release..HEAD | wc -l)

# build flags
# comment the below to see more warnings
WARNINGS:=\
	-Wall \
	-fno-warn-unused-do-bind \
	-fno-warn-name-shadowing \
	-fno-warn-missing-signatures \
	-fno-warn-orphans \
	-fno-warn-type-defaults \

# For ghc-only dev builds of hledger-web: enable the language
# extensions specified in hledger-web.cabal, except for some which are
# not compatible with hledger-lib and hledger, or little-used; those
# are enabled with source file pragmas instead. Note: compilation
# warnings and errors might differ between ghc-only and cabal builds.
WEBLANGEXTS:=\
	-XCPP \
	-XMultiParamTypeClasses \
	-XOverloadedStrings \
	-XQuasiQuotes \
	-XRecordWildCards \
	-XTemplateHaskell \
#	-XNoImplicitPrelude \
#	-XTypeFamilies \
#	-XGADTs \
#	-XGeneralizedNewtypeDeriving \
#	-XFlexibleContexts \
#	-XEmptyDataDecls \
#	-XNoMonomorphismRestriction

PREFERMACUSRLIBFLAGS=-L/usr/lib
GHCMEMFLAGS= #+RTS -M200m -RTS
CABALMACROSFLAGS=-optP-include -optPhledger/dist/build/autogen/cabal_macros.h
BUILDFLAGS1:=-rtsopts $(WARNINGS) $(INCLUDEPATHS) $(PREFERMACUSRLIBFLAGS) $(GHCMEMFLAGS) $(CABALMACROSFLAGS) -DPATCHLEVEL=$(PATCHLEVEL) -DDEVELOPMENT
BUILDFLAGS:=$(BUILDFLAGS1) -DVERSION='"$(VERSION)dev"'
PROFBUILDFLAGS:=-prof -fprof-auto -osuf hs_p
# sp needs different quoting:
AUTOBUILDFLAGS:=$(BUILDFLAGS1) -DVERSION='\"$(VERSION)dev\"' # $(PROFBUILDFLAGS) 
LINUXRELEASEBUILDFLAGS:=-DMAKE $(WARNINGS) $(INCLUDEPATHS) -O2 -static -optl-static -optl-pthread
MACRELEASEBUILDFLAGS:=-DMAKE $(WARNINGS) $(INCLUDEPATHS) $(PREFERMACUSRLIBFLAGS) -O2 # -optl-L/usr/lib
#WINDOWSRELEASEBUILDFLAGS:=-DMAKE $(WARNINGS) $(INCLUDEPATHS)

AUTOBUILD=$(SP) --no-exts --no-default-map $(GHC)  -O0 $(GHCMEMFLAGS)

# get an accurate binary filename for the current source and platform, slow but reliable. Avoid := here.
BINARYFILENAME=$(shell touch $(VERSIONSOURCEFILE); runhaskell -ihledger -ihledger-lib $(MAIN) --binary-filename)

# some other thing for linux binary filenames
RELEASEBINARYSUFFIX=$(shell echo "-$(VERSION)-`uname`-`arch`" | tr '[:upper:]' '[:lower:]')

TIME:=$(shell date +"%Y%m%d%H%M")


# import the def-help function for documenting make rules. 
# Standard usage:
# TARGET: PREREQUISITES \
# 	$(call def-help,TARGET,\
# 	HELP\
# 	)
# 	ACTIONS
#
# Also:
# $(call def-help-section,SECTION,\
# HELP\
# )
#
# Arguments to def-help etc. may not contain , so use eg ; instead.
# They should not contain ' as it breaks emacs font-lock.
# HELP is one or more lines, or can be blank.
#
include help-system.mk

default: help \
	$(call def-help,help,\
	list all documented rules in this makefile\
	)


######################################################################
# SETUP
# work in progress
# Some rules use a sandbox, some don't, ideally we'll handle both cases.
# Initial dev setup involves:
#  initialising a sandbox, probably
#  cabal update, perhaps
#  cabal clean in hledger packages, perhaps
#  cabal install dependencies for hledger packages
#  cabal install hledger-lib and hledger, perhaps hledger-web
#  at least start cabal build in hledger packages, to make cabal include files (dist/build/{Paths_PKG.hs,cabal_macros.h}) (not working with a sandbox)
# When done we should be able to make install, repl-{lib,cli,web}, ghci[-web], check etc.

$(call def-help-section,SETUP)

check: \
	$(call def-help,check,\
	developer environment checks\
	)
	@echo sanity-check developer environment:
	@($(SHELLTESTV) checks \
		&& echo $@ PASSED) || echo $@ FAILED

include sandbox.mk

# sandbox: .cabal-sandbox sandbox-links \
# 	$(call def-help,sandbox,\
# 	set up a cabal sandbox and some symlinks\
# 	)

# .cabal-sandbox: \
# 	$(call def-help,.cabal-sandbox,\
# 	initialise ./.cabal sandbox and add hledger packages as sources \
# 	)
# 	cabal sandbox init
# 	cabal sandbox add-source ./hledger-lib ./hledger ./hledger-web

sandbox-links: \
	$(call def-help,sandbox-links,\
	symlink sandbox build dirs to */dist/build so that make ghci[-web] works\
	)
	-for p in hledger{-lib,,-web}; do (cd $$p/dist; ln -s dist-*/build); done

install: \
	$(call def-help,install,\
	cabal install the main hledger packages and all their dependencies\
	in the sandbox if any; otherwise in the users package db\
	)
	$(CABALINSTALL) $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS) --enable-tests

install-force: \
	$(call def-help,install-force,\
	cabal install the main hledger packages and all their dependencies more forcibly\
	(can break installed libs, requiring ghc-pkg-clean)\
	)
	$(CABALINSTALL) $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS) --enable-tests --allow-newer --force-reinstalls

installdeps: \
	$(call def-help,installdeps,\
	install all cabal dependencies for the main hledger packages\
	)
	$(CABALINSTALL) $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS) --enable-tests --only-dependencies

installdeps-force: \
	$(call def-help,installdeps-force,\
	install all cabal dependencies for the main hledger packages, more forcibly\
	(may break installed libs, requiring ghc-pkg-clean)\
	)
	$(CABALINSTALL) $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS) --enable-tests --only-dependencies --allow-newer --force-reinstalls

uninstall: \
	$(call def-help,uninstall,\
	unregister all packages, assuming they are defined lowest-dependency first\
	avoids some reinstall noise when repeatedly doing make install\
	)
	-for p in $(call reverse,$(PACKAGES)); do $(GHCPKG) unregister $$p; done

# utility function
reverse = $(if $(wordlist 2,2,$(1)),$(call reverse,$(wordlist 2,$(words $(1)),$(1))) $(firstword $(1)),$(1))

cabal%: \
	$(call def-help,cabal%,\
	run a cabal command in all hledger package dirs\
	)
	for p in $(PACKAGES); do (echo doing cabal $* in $$p; cd $$p; cabal $*; echo); done

# # run a command in all hledger package dirs
# all%:
# 	for p in $(PACKAGES); do (echo doing $* in $$p; cd $$p; $*); done

Reset: \
	$(call def-help,Reset,\
	\
	)
	cabal sandbox delete

######################################################################
# BUILDING

$(call def-help-section,BUILDING)

EXTRAINSTALLARGS=

auto: auto---version \
	$(call def-help,auto,\
	auto-recompile and run (something, eg --help or unit tests) whenever a module changes\
	)

auto-%: sp \
	$(call def-help,auto-%,\
	\
	)
	$(AUTOBUILD) $(MAIN) -o bin/hledgerdev $(AUTOBUILDFLAGS) --run $*

autoweb: sp link-web-dirs \
	$(call def-help,autoweb,\
	\
	)
	$(AUTOBUILD) hledger-web/app/main.hs -o bin/hledger-webdev $(AUTOBUILDFLAGS) $(WEBLANGEXTS) --run -B --port 5001 --base-url http://localhost:5001 -f webtest.j

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

sp: \
	$(call def-help,sp,\
	check for sp and explain how to get it if not found.\
	)
	@/usr/bin/env which sp >/dev/null || \
	  (echo '"sp" is required for auto-compilation. darcs get http://joyful.com/darcsden/simon/searchpath, make it (cabal install-ing any needed packages) and add it to your PATH'; exit 1)

# force a compile even if binary exists, since we don't specify dependencies for these
.PHONY: bin/hledgerdev bin/hledger-prof bin/hledgeropt bin/hledger-webdev

bin/hledgerdev: \
	$(call def-help,bin/hledgerdev,\
	build hledger developer binary, ie as quickly as possible\
	requires cabal macros, generated by doing a cabal build in hledger/\
	)
	$(GHC) $(MAIN) -o bin/hledgerdev $(BUILDFLAGS)

bin/hledgerdev.ghc-%: $(SOURCEFILES) \
	$(call def-help,bin/hledgerdev.ghc-%,\
	build a GHC-version-specific hledger binary without disturbing with other GHC version builds\
	)
	ghc-$*  $(MAIN) -o $@ $(BUILDFLAGS)  -outputdir .ghc-$*

bin/hledgerdev.ghcall: \
	bin/hledgerdev.ghc-7.6.1 \
	bin/hledgerdev.ghc-7.4.1 \
	bin/hledgerdev.ghc-7.2.2 \
	bin/hledgerdev.ghc-7.0.4 \
	$(call def-help,bin/hledgerdev.ghcall,\
	build hledger with the main supported GHC versions\
	)

bin/hledger: \
	$(call def-help,bin/hledger,\
	build the "production" optimised cabal build with profiling enabled. Assumes a cabal sandbox.\
	)
	rm -f bin/hledger
	cabal install --disable-library-profiling --disable-executable-profiling ./hledger-lib ./hledger \
		&& mv .cabal-sandbox/bin/hledger bin/hledger

bin/hledger-prof: \
	$(call def-help,bin/hledger-prof,\
	build the "production" cabal build with profiling enabled.\
	)
	rm -f bin/hledger-prof
	cabal install -p --enable-executable-profiling --ghc-options=-fprof-auto ./hledger-lib ./hledger \
		&& mv .cabal-sandbox/bin/hledger bin/hledger-prof

# build the dev build with profiling enabled.
# not working with cabal sandbox
# bin/hledgerdev-prof:
# 	$(GHC) $(BUILDFLAGS) $(PROFBUILDFLAGS) $(MAIN) -o $@

hledgerhpc: \
	$(call def-help,hledgerhpc,\
	build the heap profiling binary for coverage reports and heap profiles.\
	Keep these .o files separate from the regular ones.\
	)
	$(GHC) $(MAIN) -fhpc -o bin/hledgerhpc -outputdir .hledgerhpcobjs $(BUILDFLAGS)

# build other executables quickly

bin/hledger-webdev: link-web-dirs \
	$(call def-help,bin/hledger-webdev,\
	\
	)
	$(GHC) -o $@ $(BUILDFLAGS) $(WEBLANGEXTS) hledger-web/app/main.hs

bin/hledger-web-production: \
	$(call def-help,bin/hledger-web-production,\
	\
	)
	$(GHC) -o $@ $(BUILDFLAGS) $(WEBLANGEXTS) hledger-web/app/main.hs

linuxbinaries: 	linuxbinary-hledger \
		linuxbinary-hledger-web \
	$(call def-help,linuxbinaries,\
	build portable releaseable binaries for gnu/linux\
	)
	@echo 'Please check the binaries look portable, then make compressbinaries:'
	-file bin/*`arch`

linuxbinary-hledger: \
	$(call def-help,linuxbinary-hledger,\
	work around for inconsistently-named (why ?) hledger/app/hledger-cli.hs\
	)
	$(GHC) hledger/app/hledger-cli.hs -o bin/$*$(RELEASEBINARYSUFFIX) $(LINUXRELEASEBUILDFLAGS)

linuxbinary-%: \
	$(call def-help,linuxbinary-%,\
	\
	)
	$(GHC) $*/$*.hs -o bin/$*$(RELEASEBINARYSUFFIX) $(LINUXRELEASEBUILDFLAGS)

macbinaries:    macbinary-hledger \
		macbinary-hledger-web \
	$(call def-help,macbinaries,\
	\
	)
	@echo 'Please check the binaries are portable, then make compressbinaries'
	otool -L bin/*`arch`

macbinary-%: \
	$(call def-help,macbinary-%,\
	build a deployable mac binary for the specified hledger package, munging\
	the link command to use only standard osx libs.  Specifically we link\
	without the non-standard GMP framework, which causes no apparent harm.\
	Clunky, does the link twice.\
	)
	BINARY=`echo $(BINARYFILENAME) | sed -e 's/hledger/$*/'` ; \
	LINKCMD=`$(GHC) -v  $*/$*.hs $(MACRELEASEBUILDFLAGS) -o bin/$$BINARY 2>&1 | egrep "bin/gcc.*bin/$$BINARY"` ; \
	PORTABLELINKCMD=`echo $$LINKCMD | sed -e 's/ -framework GMP//'` ; \
	echo $$PORTABLELINKCMD; $$PORTABLELINKCMD

windowsbinaries: install \
	$(call def-help,windowsbinaries,\
	Run this on a windows machine or in a wine session, and probably in a\
	separate copy of the repo (hledger-win).\
	Builds and gather deployable binaries for windows, if cygwin tools are\
	present and all packages are buildable. Otherwise, cabal install each\
	package and gather the binaries by hand.\
	)
	cp ~/.cabal/bin/hledger.exe bin/`echo $(BINARYFILENAME) | dos2unix`
	-cp ~/.cabal/bin/hledger-web.exe bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-web/' | dos2unix`
	@echo 'Please check the binaries are portable, then make compressbinaries'
	ls -l bin/*`arch`

wine-cmd: \
	$(call def-help,wine-cmd,\
	various ways of getting a wine shell\
	command-line windows command prompt. Works eg in an emacs shell buffer.\
	)
	wine cmd

wine-cmd2: \
	$(call def-help,wine-cmd2,\
	as above but try to cd somewhere useful (doesnt work), also ctrl-d exits quickly\
	)
	(echo c:; echo cd \\mingw\\msys\\1.0; cat) | wine cmd

wine-cmd-window: \
	$(call def-help,wine-cmd-window,\
	windows command prompt in a new window\
	)
	wineconsole cmd &

wine-mintty: \
	$(call def-help,wine-mintty,\
	msys bash shell in a mintty window\
	)
	wine c:/mingw/msys/1.0/bin/mintty - &

compressbinaries: \
	$(call def-help,compressbinaries,\
	\
	)
	cd bin; for f in *-windows-*.exe ; do echo zipping $$f; rm -f $$f.zip; zip $$f.zip $$f; done
#	for f in bin/*-{linux,mac-}* ; do echo gzipping $$f; gzip -q $$f >$$f.gz; done
#	gzip bin/*`arch`

tools/unittest: tools/unittest.hs \
	$(call def-help,tools/unittest,\
	build the standalone unit test runner. Requires test-framework, which\
	may not work on windows.\
	)
	$(GHC) -threaded -O2 tools/unittest.hs

tools/doctest: tools/doctest.hs \
	$(call def-help,tools/doctest,\
	build the doctest runner\
	)
	$(GHC) tools/doctest.hs

tools/simplebench: tools/simplebench.hs \
	$(call def-help,tools/simplebench,\
	build the simple benchmark runner. Requires html and tabular.\
	)
	$(GHC) tools/simplebench.hs

tools/criterionbench: tools/criterionbench.hs \
	$(call def-help,tools/criterionbench,\
	build the criterion-based benchmark runner. Requires criterion.\
	)
	$(GHC) tools/criterionbench.hs

tools/progressionbench: tools/progressionbench.hs \
	$(call def-help,tools/progressionbench,\
	build the progression-based benchmark runner. Requires progression.\
	)
	$(GHC) tools/progressionbench.hs

tools/generatejournal: tools/generatejournal.hs \
	$(call def-help,tools/generatejournal,\
	build the generatejournal tool\
	)
	$(GHC) tools/generatejournal.hs

######################################################################
# TESTING

$(call def-help-section,TESTING)

packdeps: \
	$(call def-help,packdeps,\
	run packdeps on each package to check for disallowed newer dependencies\
	)
	for p in $(PACKAGES); do packdeps $$p/$$p.cabal; done

######################################################################
# DOCUMENTATION


######################################################################
# RELEASING

# set up this repo copy for previewing a release:
# ensure download links work
# set-up-rc-repo:
# 	cd site/_site; ln -s ../download

######################################################################
# MISCELLANEOUS



######################################################################
# OLD PRE PKG SPLIT
######################################################################

######################################################################
# TESTING

test: codetest \
	$(call def-help,test,\
	\
	)

codetest: unittest functest \
	$(call def-help,codetest,\
	quick code tests - run all the time\
	)

committest: hlinttest unittest doctest functest haddocktest warningstest quickcabaltest \
	$(call def-help,committest,\
	moderate pre-commit tests - run before record or before send/push, your choice\
	)

releasetest: Clean unittest functest fullcabaltest haddocktest #warningstest doctest \
	$(call def-help,releasetest,\
	thorough pre-release tests - run before release\
	consider hiding dev-build symlinks in Hledger/ first\
	)

hlinttest hlint: \
	$(call def-help,hlinttest hlint,\
	\
	)
	hlint --hint=hlint --report=hlint.html $(SOURCEFILES)

unittest: unittest-builtin \
	$(call def-help,unittest,\
	run unit tests\
	)

unittest-builtin: bin/hledgerdev \
	$(call def-help,unittest-builtin,\
	\
	)
	@echo unit tests:
	@(bin/hledgerdev test \
		&& echo $@ PASSED) || echo $@ FAILED

unittest-%: bin/hledgerdev \
	$(call def-help,unittest-%,\
	\
	)
	@echo unit tests:
	@(bin/hledgerdev test $* \
		&& echo $@ PASSED) || echo $@ FAILED

unittest-standalone: tools/unittest \
	$(call def-help,unittest-standalone,\
vv	XXX doesnt rebuild on hledger source changes\
	)
	@echo unit tests (standalone):
	@(tools/unittest \
		&& echo $@ PASSED) || echo $@ FAILED

unittest-interpreted: \
	$(call def-help,unittest-interpreted,\
	run unit tests without waiting for compilation\
	)
	@echo "unit tests (interpreted)":
	@(run$(GHC) $(MAIN) test \
		&& echo $@ PASSED) || echo $@ FAILED

functest: bin/hledgerdev tests/addons/hledger-addon \
	$(call def-help,functest,\
	run functional tests, requires shelltestrunner >= 0.9 from hackage\
	16 threads sometimes gives "commitAndReleaseBuffer: resource vanished (Broken pipe)" here but seems harmless\
	)
	@echo functional tests:
	@(COLUMNS=80 $(SHELLTEST) --execdir tests \
		&& echo $@ PASSED) || echo $@ FAILED

ADDONEXTS=pl py rb sh hs lhs rkt exe com bat
tests/addons/hledger-addon: \
	$(call def-help,tests/addons/hledger-addon,\
	generate dummy add-ons for testing (hledger-addon the rest)\
	)
	rm -rf tests/addons/hledger-*
	printf '#!/bin/sh\necho add-on: $$0\necho args: $$*\n' >tests/ADDONS/hledger-addon
	for E in '' $(ADDONEXTS); do \
		cp tests/ADDONS/hledger-addon tests/ADDONS/hledger-addon.$$E; done
	for F in addon. addon2 addon2.hs addon3.exe addon3.lhs addon4.exe add reg; do \
		cp tests/ADDONS/hledger-addon tests/ADDONS/hledger-$$F; done
	mkdir tests/ADDONS/hledger-addondir
	chmod +x tests/ADDONS/hledger-*

test-ghc-%: # bin/hledgerdev.ghc-$* \
	$(call def-help,test-ghc-%,\
	run unit and functional tests with a specific GHC version\
	some functional tests (add, include, read-csv..) have bin/hledgerdev hard coded - might need to symlink it\
	)
	@echo; echo testing hledger built with ghc-$*
	@(echo unit tests: \
	&& bin/hledgerdev.ghc-$* test \
	&& echo functional tests: \
	&& $(SHELLTEST) tests -w bin/hledgerdev.ghc-$* \
	&& echo $@ PASSED) || echo $@ FAILED

test-ghcall: bin/hledgerdev.ghcall \
	test-ghc-7.6.1 \
	test-ghc-7.4.1 \
	test-ghc-7.2.2 \
	test-ghc-7.0.4 \
	$(call def-help,test-ghcall,\
	run unit and functional tests with main supported GHC versions\
	)

DOCTESTFILES=\
	hledger/Hledger/Cli/Tests.hs
doctest: tools/doctest \
	$(call def-help,doctest,\
	run doc tests\
	)
	@for f in $(DOCTESTFILES); do \
		(tools/doctest $$f && echo $@ PASSED) || echo $@ FAILED ; done

haddocktest: \
	$(call def-help,haddocktest,\
	make sure we have no haddock errors\
	)
	@(make --quiet haddock \
		&& echo $@ PASSED) || echo $@ FAILED

warningstest: \
	$(call def-help,warningstest,\
	make sure the normal build has no warnings XXX needs updating\
	)
	@(make -s clean \
		&& make --no-print-directory -s hledgernowarnings \
		&& echo $@ PASSED) || echo $@ FAILED

quickcabaltest: \
	$(call def-help,quickcabaltest,\
	make sure cabal is reasonably happy\
	)
	@(make --no-print-directory cabalclean cabalcheck cabalconfigure \
		&& echo $@ PASSED) || echo $@ FAILED

fullcabaltest: \
	$(call def-help,fullcabaltest,\
	make sure cabal is happy in all possible ways\
	)
	(for p in $(PACKAGES); do ( \
		printf "\ntesting $$p package\n" \
		&& cd $$p \
		&& cabal clean \
		&& cabal check \
		&& $(CABALINSTALL) \
		&& cabal sdist \
		); done \
		&& echo $@ PASSED) || echo $@ FAILED
#		&& cabal upload dist/$$p-$(VERSION).tar.gz --check -v3 \

quickbench: samplejournals tests/bench.tests tools/simplebench \
	$(call def-help,quickbench,\
	run simple performance benchmarks without saving results\
	Requires some commands defined in tests/bench.tests and some BENCHEXES defined above.\
	)
	tools/simplebench -v -ftests/bench.tests $(BENCHEXES)
	@rm -f benchresults.*

bench: samplejournals tests/bench.tests tools/simplebench \
	$(call def-help,bench,\
	run simple performance benchmarks and archive results\
	Requires some commands defined in tests/bench.tests and some BENCHEXES defined above.\
	)
	tools/simplebench -v -ftests/bench.tests $(BENCHEXES) | tee doc/profs/$(TIME).bench
	@rm -f benchresults.*
	@(cd doc/profs; rm -f latest.bench; ln -s $(TIME).bench latest.bench)

criterionbench: samplejournals tools/criterionbench \
	$(call def-help,criterionbench,\
	run criterion benchmark tests and save graphical results\
	)
	tools/criterionbench -t png -k png

progressionbench: samplejournals tools/progressionbench \
	$(call def-help,progressionbench,\
	run progression benchmark tests and save graphical results\
	)
	tools/progressionbench -- -t png -k png

prof: samplejournals \
	$(call def-help,prof,\
	generate and archive an execution profile\
	) #bin/hledger-prof
	@echo "Profiling: $(PROFCMD)"
	-$(PROFCMD) +RTS $(PROFRTSFLAGS) -RTS
	mv hledger-prof.prof doc/profs/$(TIME).prof
	(cd doc/profs; rm -f latest*.prof; ln -s $(TIME).prof latest.prof)

viewprof: prof \
	$(call def-help,viewprof,\
	generate, archive, simplify and display an execution profile\
	)
	tools/simplifyprof.hs doc/profs/latest.prof

quickprof: samplejournals \
	$(call def-help,quickprof,\
	generate and display an execution profile, dont save or simplify\
	) #bin/hledger-prof
	@echo "Profiling: $(PROFCMD)"
	-$(PROFCMD) +RTS $(PROFRTSFLAGS) -RTS
	echo; cat hledger-prof.prof

heap: samplejournals \
	$(call def-help,heap,\
	generate and archive a graphical heap profile\
	) #bin/hledger-prof
	@echo "Profiling heap with: $(PROFCMD)"
	$(PROFCMD) +RTS -hc -RTS
	mv hledger-prof.hp doc/profs/$(TIME).hp
	(cd doc/profs; rm -f latest.hp; ln -s $(TIME).hp latest.hp; \
		hp2ps $(TIME).hp; rm -f latest.ps; ln -s $(TIME).ps latest.ps; rm -f *.aux)

viewheap: heap \
	$(call def-help,viewheap,\
	\
	)
	$(VIEWPS) doc/profs/latest.ps

quickheap: samplejournals \
	$(call def-help,quickheap,\
	generate and display a graphical heap profile, dont save\
	) #bin/hledger-prof
	@echo "Profiling heap with: $(PROFCMD)"
	$(PROFCMD) +RTS -hc -RTS
	hp2ps hledger-prof.hp
	$(VIEWPS) hledger.ps

quickcoverage: hledgerhpc \
	$(call def-help,quickcoverage,\
	display a code coverage text report from running hledger COVCMD\
	)
	@echo "Generating code coverage text report for hledger command: $(COVCMD)"
	tools/runhledgerhpc "report" $(COVCMD)

coverage: samplejournals hledgerhpc \
	$(call def-help,coverage,\
	generate a code coverage html report from running hledger COVCMD\
	)
	@echo "Generating code coverage html report for hledger command: $(COVCMD)"
	tools/runhledgerhpc "markup --destdir=doc/profs/coverage" $(COVCMD)
	cd doc/profs/coverage; rm -f index.html; ln -s hpc_index.html index.html

viewcoverage: \
	$(call def-help,viewcoverage,\
	view the last html code coverage report\
	)
	$(VIEWHTML) doc/profs/coverage/index.html

# XXX with a sandbox, use sandbox-repl-* instead
repl-lib:\
	$(call def-help,repl-lib, start a cabal REPL and load the hledger-lib package)
	(cd hledger-lib; cabal repl)

repl-cli:\
	$(call def-help,repl-cli, start a cabal REPL and load the hledger package)
	(cd hledger; cabal repl exe:hledger)

repl-web:\
	$(call def-help,repl-web, start a cabal REPL and load the hledger-web package)
	(cd hledger-web; cabal repl exe:hledger-web)

ghci: \
	$(call def-help,ghci, start a sandbox-aware GHCI REPL and load the hledger-lib and hledger packages)
	cabal exec $(GHCI) -- $(BUILDFLAGS) hledger/Hledger/Cli/Main.hs

ghci-web: \
	$(call def-help,ghci-web, start a sandbox-aware GHCI REPL and load the hledger-lib, hledger and hledger-web packages)
	cabal exec $(GHCI) -- $(BUILDFLAGS) hledger-web/app/main.hs

samplejournals: data/sample.journal data/100x100x10.journal data/1000x1000x10.journal data/1000x10000x10.journal data/10000x1000x10.journal data/10000x10000x10.journal data/100000x1000x10.journal \
	$(call def-help,samplejournals,\
	generate standard sample journals\
	)

data/sample.journal: \
	$(call def-help,data/sample.journal,\
	\
	)
	true # XXX should probably regenerate this

data/100x100x10.journal: tools/generatejournal \
	$(call def-help,data/100x100x10.journal,\
	\
	)
	tools/generatejournal 100 100 10 >$@

data/1000x1000x10.journal: tools/generatejournal \
	$(call def-help,data/1000x1000x10.journal,\
	\
	)
	tools/generatejournal 1000 1000 10 >$@

data/1000x10000x10.journal: tools/generatejournal \
	$(call def-help,data/1000x10000x10.journal,\
	\
	)
	tools/generatejournal 1000 10000 10 >$@

data/10000x1000x10.journal: tools/generatejournal \
	$(call def-help,data/10000x1000x10.journal,\
	\
	)
	tools/generatejournal 10000 1000 10 >$@

data/10000x10000x10.journal: tools/generatejournal \
	$(call def-help,data/10000x10000x10.journal,\
	\
	)
	tools/generatejournal 10000 10000 10 >$@

data/100000x1000x10.journal: tools/generatejournal \
	$(call def-help,data/100000x1000x10.journal,\
	\
	)
	tools/generatejournal 100000 1000 10 >$@

######################################################################
# DOCUMENTATION

$(call def-help-section,DOCUMENTATION)

docs: site codedocs \
	$(call def-help,docs,\
	rebuild all docs\
	)

cleandocs: site-clean \
	$(call def-help,cleandocs,\
	\
	)

site-build site: \
	$(call def-help,site-build site,\
	build some additional static bits of the hledger.org website\
	Requires hakyll-std, a generic hakyll site builder\
	) #olddocs
	-cd doc/site; hakyll build

site-clean: \
	$(call def-help,site-clean,\
	\
	) #cleanolddocs
	-cd doc/site; hakyll clean
#	rm -rf doc/site/_site/*

site-preview: \
	$(call def-help,site-preview,\
	\
	) #doc/site/site
	cd doc/site; hakyll preview

site-view: site \
	$(call def-help,site-view,\
	\
	)
	$(VIEWHTML) doc/site/_site/index.html

# site-auto:
# 	cd doc/site; $(AUTOBUILD) site.hs -o site $(PREFERMACUSRLIBFLAGS) --run preview

# ensure some old doc versions are in place:

#olddocs: doc/0.22 doc/0.21

#doc/0.23:
#	(cd doc; git archive --prefix doc/0.23/ tags/0.23 'doc/MANUAL.md') | tar xf -

# doc/0.22:
# 	git archive --prefix doc/0.22/ tags/0.22 'MANUAL.md' | tar xf -

# doc/0.21:
# 	git archive --prefix doc/0.21/ tags/0.21.3 'MANUAL.md' | tar xf -

# cleanolddocs:
# 	cd doc; rm -rf 0.22 0.21


pdf: codepdf \
	$(call def-help,pdf,\
	\
	) #docspdf

# generate pdf versions of main docs
# docspdf:
# 	-for d in $(DOCFILES); do (cd site && ln -sf ../$$d && pandoc $$d -w pdf && rm -f $$d); done

# # format all code as a pdf for offline reading
# ENSCRIPT=enscript -q --header='$$n|$$D{%+}|Page $$% of $$=' --highlight=haskell --line-numbers --font=Courier6 --color -o-
# codepdf:
# 	$(ENSCRIPT) --pretty-print=makefile hledger.cabal >cabal.ps
# 	$(ENSCRIPT) --pretty-print=makefile Makefile >make.ps
# 	$(ENSCRIPT) --pretty-print=haskell $(SOURCEFILES) >haskell.ps
# 	cat cabal.ps make.ps haskell.ps | ps2pdf - >code.pdf

# # view all docs and code as pdf
# PDFS=site/{README,README2,MANUAL,CHANGES,CONTRIBUTORS}.pdf code.pdf
# viewall: pdf
# 	$(VIEWPDF) $(PDFS)

# # print all docs and code for offline reading
# printall: pdf
# 	$(PRINT) $(PDFS)

pushdocs: push \
	$(call def-help,pushdocs,\
	push latest docs etc. and update the hledger.org site\
	)
	ssh simon@joyful.com 'make -C/repos/hledger docs'

EXES=hledger hledger-vty hledger-web
savehelp: \
	$(call def-help,savehelp,\
	dump all executables command line help into files for review\
	)
	for e in $(EXES); do $$e --help >.HELP_$$e; done

codedocs: haddock hscolour coverage \
	$(call def-help,codedocs,\
	generate api & other code docs\
	) #sourcegraph #hoogle

# cf http://www.haskell.org/haddock/doc/html/invoking.html
# --ghc-options=-optP-P is a workaround for http://trac.haskell.org/haddock/ticket/284
HADDOCKFLAGS= \
	--haddock-options='--no-warnings' \
	--ghc-options='-optP-P' \

haddock: haddock-lib haddock-cli haddock-web \
	$(call def-help,haddock,\
	build per-package haddocks using cabal\
	)

haddock-lib: \
	$(call def-help,haddock-lib,\
	\
	)
	(cd hledger-lib; cabal haddock $(HADDOCKFLAGS))

haddock-cli: \
	$(call def-help,haddock-cli,\
	\
	)
	(cd hledger; cabal haddock $(HADDOCKFLAGS))

haddock-web: \
	$(call def-help,haddock-web,\
	\
	)
	(cd hledger-web; cabal haddock $(HADDOCKFLAGS))

view-haddock-cli: \
	$(call def-help,view-haddock-cli,\
	view-haddock-cli\
	)
	$(VIEWHTML) hledger/dist/doc/html/hledger/index.html

view-haddock-%: \
	$(call def-help,view-haddock-%,\
	view-haddock-lib, view-haddock-web\
	)
	$(VIEWHTML) hledger-$*/dist/doc/html/hledger-$*/index.html

# HADDOCKALLFLAGS= \
#		--no-warnings \
# 	--prologue .haddockallprologue \
# 	--optghc='-optP-include' \
# 	--optghc='-optPhledger/dist/build/autogen/cabal_macros.h'
# #	 --optghc='-XCPP'
# #	--optghc="$(WEBLANGEXTS)"
# #	--optghc='-hide-package monads-tf' 
#$(subst -D,--optghc=-D,$(DEFINEFLAGS))
#
# # .haddockallsynopsis: hledger/hledger.cabal
# # 	grep synopsis $< | sed -e 's/synopsis: *//' >$@
#
# .haddockallprologue: hledger/hledger.cabal
# 	cat $< | perl -ne 'print if (/^description:/../^$$/)' | sed -e 's/^description: *//' >$@
# 	printf "\nThis haddock covers all hledger-* packages, for individual package haddocks see hackage.\n" >>$@
#
# # build haddock docs for the whole project
# haddock-all: .haddockallprologue
# 	$(HADDOCK) \
# 		$(HADDOCKALLFLAGS) \
# 		--title "hledger-* API docs" \
# 		-o site/api \
# 		--html \
# 		--source-module=src/%{MODULE/./-}.html \
# 		--source-entity=src/%{MODULE/./-}.html#%N \
# 		$(HADDOCKSOURCEFILES)
#
# # browse the whole-project haddock
# view-haddock-all:
# 	$(VIEWHTML) site/api/frames.html

# http://www.cs.york.ac.uk/fp/darcs/hscolour/
HSCOLOUR=HsColour -css
hscolour: doc/site/api/src doc/site/api/src/hscolour.css \
	$(call def-help,hscolour,\
	\
	)
	for f in $(HADDOCKSOURCEFILES); do \
		$(HSCOLOUR) -anchor $$f -odoc/site/api/src/`echo $$f | sed -e's%[^/]*/%%' | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
	done

doc/site/api/src/hscolour.css: doc/site/api/src \
	$(call def-help,doc/site/api/src/hscolour.css,\
	\
	)
	$(HSCOLOUR) -print-css >doc/site/api/src/hscolour.css

doc/site/api/src: \
	$(call def-help,doc/site/api/src,\
	\
	)
	mkdir -p doc/site/api/src

sourcegraph: \
	$(call def-help,sourcegraph,\
	\
	)
	for p in $(PACKAGES); do (cd $$p; SourceGraph $$p.cabal); done

patchdeps: \
	$(call def-help,patchdeps,\
	\
	)
	darcs2dot > patchdeps.dot && dot -Tpng -O patchdeps.dot

# # generate external api docs for each package
# allhaddock: cabalhaddock\ --hyperlink-source\ --executables

# # generate internal code docs for each package
# allhaddockinternal: cabalhaddock\ --hyperlink-source\ --executables\ --internal

# # generate hoogle indices for each package
# allhoogle: cabalhaddock\ --hoogle\ --executables

#set up the hoogle web interface
## We munge haddock and hoogle into a rough but useful framed layout.
## For this to work the hoogle cgi must be built with base target "main".
## XXX move the framed index building into haddock: ?
# 	sed -i -e 's%^></HEAD%><base target="main"></HEAD%' site/api/modules-index.html ; \
# 	cp site/api-frames.html site/api/index.html ; \
# # 	cp site/hoogle-small.html site/api
#
#uses a hoogle source tree configured with --datadir=., patched to fix haddock urls/target frame
# HOOGLESRC=/usr/local/src/hoogle
# HOOGLE=$(HOOGLESRC)/dist/build/hoogle/hoogle
# HOOGLEVER=`$(HOOGLE) --version |tail -n 1 | sed -e 's/Version /hoogle-/'`
# hoogle: hoogleindex
# 	if test -f $(HOOGLE) ; then \
# 		cd site/api && \
# 		rm -f $(HOOGLEVER) && \
# 		ln -s . $(HOOGLEVER) && \
# 		cp -r $(HOOGLESRC)/src/res/ . && \
# 		cp -p $(HOOGLE) index.cgi && \
# 		touch log.txt && chmod 666 log.txt ; \
# 	else \
# 		echo "Could not find $(HOOGLE) in the hoogle source tree" ; \
# 	fi
#
#generate a hoogle index
# hoogleindex:
# 	$(HADDOCK) $(HADDOCKFLAGS) -o site/api --hoogle $(MAIN) && \
# 	cd site/api && \
# 	hoogle --convert=main.txt --output=default.hoo

######################################################################
# RELEASING

$(call def-help-section,RELEASING)

# Version numbering. See also .version and Version.hs.
#
# hledger's version number appears in:
#  hledger --version
#  hledger's cabal file
#  darcs tags
#  hackage tarball filenames
#  hackage pages
#
# Some old version numbering goals:
# 1 automation, robustness, simplicity, platform independence
# 2 cabal versions must be all-numeric
# 3 release versions can be concise (without extra .0's)
# 4 releases should have a corresponding darcs tag
# 5 development builds should have a precise version appearing in --version
# 6 development builds should generate cabal packages with non-confusing versions
# 7 there should be a way to mark builds/releases as alpha or beta
# 8 it should be easy to darcs get the .0 release even after bugfix releases
# 9 avoid unnecessary compiling and linking
# 10 minimise rcs noise and syncing issues (commits, unrecorded changes)
#
# Current policy:
#
# - We follow http://haskell.org/haskellwiki/Package_versioning_policy
#
# - The full release version is ma.jor.minor, where minor is 0 for a
#   normal release or 1..n for bugfix releases.
#
# - The elided release version is ma.jor when minor is 0. We use it for
#   hackage releases when possible, trusting it doesn't cause trouble..
#
# - The build version is ma.jor.minor+patches, where patches is the number
#   of patches applied in the current repo since the last release tag.
#
# - The release tag in the repo is the full release version.
#
# - hledger --version shows the release version or build version as
#   appropriate.
#
# - The .version file must be updated manually before a release.
#
# - "make simplebench" depends on version numbers in BENCHEXES, these also
#   must be updated manually.
#
# - "make" updates the version in most other places, and defines PATCHES.
#   Note "cabal build" should also do this but doesn't yet.
#
# - "make release" additionally records the main version number-affected
#   files, and tags the repo with the release tag.

release: releasetest setandrecordversion tagrelease \
	$(call def-help,release,\
	Build a release, tag the repo, prepare a cabal package\
	First update .version. Eg:\
	a normal release: echo 0.7   >.version; make release\
	a bugfix release: echo 0.7.1 >.version; make release\
	)

upload: cabalsdist hackageupload pushdocs \
	$(call def-help,upload,\
	Upload the latest cabal package and update hledger.org\
	)

releaseandupload: release upload \
	$(call def-help,releaseandupload,\
	\
	)


setandrecordversion: setversion \
	$(call def-help,setandrecordversion,\
	update the version number in local files, and prompt to record changes\
	in these files. Triggered by "make release".\
	)
	darcs record -m "bump version" $(VERSIONFILE) $(VERSIONSENSITIVEFILES)

setversion: $(VERSIONSENSITIVEFILES) \
	$(call def-help,setversion,\
	update the version string in local files. This should be run immediately\
	after editing the VERSION file.\
	)

Setversion: \
	$(call def-help,Setversion,\
	re-update version string even if it seems unchanged\
	)
	touch $(VERSIONFILE); make setversion

hledger-lib/hledger-lib.cabal: $(VERSIONFILE) \
	$(call def-help,hledger-lib/hledger-lib.cabal,\
	\
	)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@

hledger/hledger.cabal: $(VERSIONFILE) \
	$(call def-help,hledger/hledger.cabal,\
	\
	)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger(-lib)? *[>=]=) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(-DVERSION=\")[^\"]+/\$${1}$(VERSION)/" -i $@

hledger-web/hledger-web.cabal: $(VERSIONFILE) \
	$(call def-help,hledger-web/hledger-web.cabal,\
	\
	)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger(-lib|-web)? *[>=]=) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(-DVERSION=\")[^\"]+/\$${1}$(VERSION)/" -i $@

doc/MANUAL.md: $(VERSIONFILE) \
	$(call def-help,doc/MANUAL.md,\
	\
	)
	perl -p -e "s/(^Version:) +[0-9.]+/\1 $(VERSION)/" -i $@

tagrelease: \
	$(call def-help,tagrelease,\
	\
	)
	for p in $(PACKAGES); do git tag $$p-$(VERSION); done

hackageupload-dry: \
	$(call def-help,hackageupload-dry,\
	\
	)
	for p in $(PACKAGES); do cabal upload $$p/dist/$$p-$(VERSION).tar.gz -v2 --check; done

hackageupload: \
	$(call def-help,hackageupload,\
	\
	)
	for p in $(PACKAGES); do cabal upload $$p/dist/$$p-$(VERSION).tar.gz -v2; done

send: \
	$(call def-help,send,\
	send unpushed patches to the mail list\
	)
	darcs send http://joyful.com/repos/hledger --to=hledger@googlegroups.com --edit-description  

push: pushprofs \
	$(call def-help,push,\
	push patches and anything else pending to the public server\
	) # pushlatestbinary
	darcs push simon@joyful.com:/repos/hledger

pull: pullprofs \
	$(call def-help,pull,\
	pull anything pending from the public server\
	)
	darcs pull -a simon@joyful.com:/repos/hledger

RSYNC=rsync

pushprofs: \
	$(call def-help,pushprofs,\
	push any new profiles and benchmark results to the public site\
	beware, results will vary depending on which machine generated them\
	)
	$(RSYNC) -azP doc/profs/ simon@joyful.com:/repos/hledger/doc/profs/

pullprofs: \
	$(call def-help,pullprofs,\
	fetch any new profiles and benchmark results from the public site\
	)
	$(RSYNC) -azP simon@joyful.com:/repos/hledger/doc/profs/ doc/profs/

compressbinary: \
	$(call def-help,compressbinary,\
	compress the just-built platform binary. make hledgerPLATFORM first. Use\
	the win variant on windows.\
	)
	gzip -9 bin/$(BINARYFILENAME)
compressbinarywin: \
	$(call def-help,compressbinarywin,\
	\
	)
	cd bin; zip -9 $(BINARYFILENAME).zip $(BINARYFILENAME)

# push the last-updated platform binary to the public download directory
# pushlatestbinary:
# 	cd bin; $(RSYNC) -aP `ls -t | head -2` simon@joyful.com:/repos/hledger/site/download/


showreleasestats stats: \
	showreleasedays \
	showunreleasedchangecount \
	showloc \
	showtestcount \
	showunittestcoverage \
	showreleaseauthors \
	showunreleasedcodechanges \
	showunpushedchanges \
	$(call def-help,showreleasestats stats,\
	show project stats useful for release notes\
	)
#	simplebench
#	showerrors

FROMTAG=.

showreleasedays: \
	$(call def-help,showreleasedays,\
	\
	)
	@echo Days since last release:
	@tools/dayssincetag.hs $(FROMTAG) | head -1 | cut -d' ' -f-1
	@echo

showunreleasedchangecount: \
	$(call def-help,showunreleasedchangecount,\
	\
	)
	@echo Commits since last release:
	@darcs changes --from-tag $(FROMTAG) --count
	@echo

showreleaseauthors: \
	$(call def-help,showreleaseauthors,\
	\
	)
	@echo Patch authors since last release:
	@darcs changes --from-tag $(FROMTAG) |grep '^\w' |cut -c 31- |sort |uniq
	@echo

showloc: \
	$(call def-help,showloc,\
	\
	)
	@echo Current lines of code including tests:
	@sloccount `ls $(SOURCEFILES)` | grep haskell:
	@echo

sloc: \
	$(call def-help,sloc,\
	\
	)
	@sloccount hledger-lib hledger hledger-web

cloc: \
	$(call def-help,cloc,\
	\
	)
	@echo
	@echo "Lines of code as of `date`:"
	@echo
	@echo "hledger-lib, hledger"
	@cloc -q hledger-lib hledger             2>&1 | grep -v 'defined('
	@echo
	@echo "hledger-web"
	@cloc -q hledger-web                     2>&1 | grep -v 'defined('
	@echo
	@echo "hledger-lib, hledger, hledger-web"
	@cloc -q hledger-lib hledger hledger-web 2>&1 | grep -v 'defined('

showtestcount: \
	$(call def-help,showtestcount,\
	\
	)
	@echo "Unit tests:"
	@hledger test 2>&1 | cut -d' ' -f2
	@echo "Functional tests:"
	@make --no-print functest | egrep '^ Total' | awk '{print $$2}'
	@echo

showunittestcoverage: \
	$(call def-help,showunittestcoverage,\
	\
	)
	@echo Unit test coverage:
	@make --no-print quickcoverage | grep 'expressions'
	@echo

# showerrors:
# 	@echo Known errors:
# 	@awk '/^** errors/, /^** / && !/^** errors/' NOTES.org | grep '^\*\*\* ' | tail +1
# 	@echo

showunpushedchanges showunpushed: \
	$(call def-help,showunpushedchanges showunpushed,\
	\
	)
	@echo "Changes not yet pushed upstream (to `darcs show repo | grep 'Default Remote' | cut -c 17-`):"
	@-darcs push simon@joyful.com:/repos/hledger --dry-run | grep '*' | tac
	@echo

showunreleasedcodechanges showunreleased showchanges: \
	$(call def-help,showunreleasedcodechanges showunreleased showchanges,\
	\
	)
	@echo "hledger code changes since last release:"
	@darcs changes --from-tag $(FROMTAG) --matches "not (name docs: or name doc: or name site: or name tools:)" | grep '*'
	@echo

showcodechanges: \
	$(call def-help,showcodechanges,\
	\
	)
	@echo "hledger code changes:"
	@darcs changes --matches "not (name docs: or name site: or name tools:)" | egrep '^ +(\*|tagged)'
	@echo

######################################################################
# MISCELLANEOUS

$(call def-help-section,MISCELLANEOUS)

fixperms: \
	$(call def-help,fixperms,\
	fix permissions (eg after darcs get)\
	)
	chmod +x tools/* $(MAIN) hledger-*/Main.hs

tag: emacstags \
	$(call def-help,tag,\
	\
	)

emacstags: \
	$(call def-help,emacstags,\
	\
	)
	-@rm -f TAGS; hasktags -e $(SOURCEFILES) $(WEBFILES) $(CABALFILES) $(DOCFILES) Makefile

cleanghc: \
	$(call def-help,cleanghc,\
	\
	)
	rm -rf `find . -name "*.o" -o -name "*.hi" -o -name "*~" | grep -vE '(virthualenv|cabal-sandbox)'`

cleancabal: cabalclean \
	$(call def-help,cleancabal,\
	\
	)

cleanbin: \
	$(call def-help,cleanbin,\
	\
	)
	rm -f bin/hledgerdev bin/hledgerdev.ghc*

cleantags: \
	$(call def-help,cleantags,\
	\
	)
	rm -f TAGS tags

clean: cleanghc \
	$(call def-help,clean,\
	\
	)

Clean: cleanghc cleancabal cleanbin cleantags \
	$(call def-help,Clean,\
	\
	)

######################################################################
# LOCAL NON-COMMITTED CUSTOMISATIONS, IF ANY

-include Makefile.local
