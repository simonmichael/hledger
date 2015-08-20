###############################################################################
# hledger project makefile
#
# This is a reboot of Makefile.old. The old rules are commented out below,
# to be removed or updated over the next while.
#
# Users of this makefile: hledger developers, contributors, probably not end-users.
#
# Every user-relevant rule in this makefile should use def-help to define
# a help string. Use "make help" to see the available rules.
#
# Supplementary tools used:
#
# - stack, installs dependencies and drives cabal & ghc
# - shelltestrunner (latest version from hackage or possibly git), runs functional tests
# - hasktags, generates tag files for code navigation
# - profiteur, renders profiles as interactive html
# - hpack, generates cabal files from package.yaml files
# - hakyll-std, my generic site-building hakyll script
# - perl, currently used by a few rules (setversion)
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

# def-help* functions for documenting make rules. See the file for usage.
include help-system.mk

$(call def-help-section, hledger make rules )

help2: \
	$(call def-help,[help], list documented rules in this makefile. make -n RULE shows more detail. )

###############################################################################
# VARS

# GHC-compiled executables require a locale (and not just C) or they
# will die on encountering non-ascii data. Set LANG to something if not already set.
export LANG?=en_US.UTF-8

# command to run during profiling (time and heap)
#PROFCMD=stack exec -- hledgerprof balance -f data/1000x1000x10.journal >/dev/null

#PROFRTSFLAGS=-p
PROFRTSFLAGS=-P

# # command to run during "make coverage"
# COVCMD=test
# COVCMD=-f test-wf.csv print

# # executables to run during "make simplebench"
# BENCHEXES=hledger-0.23.3 hledger

# misc. system tools
BROWSE=open
# VIEWHTML=$(BROWSE)
VIEWPS=$(BROWSE)
# VIEWPDF=$(BROWSE)
# PRINT=lpr

GHC=ghc
GHCI=ghci
# GHCPKG=ghc-pkg
# HADDOCK=haddock
# CABAL=cabal
# CABALINSTALL=cabal install -w $(GHC)

# -j16 sometimes gives "commitAndReleaseBuffer: resource vanished (Broken pipe)" but seems harmless
SHELLTESTOPTS=--execdir -- -j16 --hide-successes
# run shell tests using the executable specified in tests
SHELLTEST=shelltest $(SHELLTESTOPTS)
# run shell tests using the stack build
SHELLTESTSTK=stack exec -- shelltest $(SHELLTESTOPTS)
#SHELLTESTSTK=shelltest -w `stack exec which hledger` $(SHELLTESTOPTS)

# # used for make auto, http://joyful.com/repos/searchpath
# SP=sp

PACKAGES=\
	hledger-lib \
	hledger \
	hledger-ui \
	hledger-web

INCLUDEPATHS=\
	-ihledger-lib \
	-ihledger \
	-ihledger-ui \
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

CABALFILES:= \
	hledger/hledger.cabal \
	hledger-*/*.cabal \
	doc/site/hakyll-std.cabal

HPACKFILES:= \
	hledger/package.yaml \
	hledger-*/package.yaml \
	doc/site/package.yaml

WEBFILES:= \
	hledger-web/templates/* \
	hledger-web/static/*.js \
	hledger-web/static/*.css

DOCFILES:= \
	doc/*.md

# files which should be updated when the version changes
VERSIONSENSITIVEFILES=\
	$(HPACKFILES) \
	doc/manual.md \

# # file(s) which require recompilation for a build to have an up-to-date version string
# VERSIONSOURCEFILE=hledger/Hledger/Cli/Version.hs

# master file defining the current release/build version
VERSIONFILE=.version

# two or three-part version string, whatever's in VERSION
VERSION:=$(shell cat $(VERSIONFILE))

# the number of commits since the last tag
PATCHLEVEL:=$(shell git describe --long | awk -F - '{print $$2}')
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
# from cabal's dist or dist-sandbox, hopefully there's just one
#CABALMACROSFLAGS=-optP-include -optP hledger/dist*/build/autogen/cabal_macros.h
# from stack's dist, hopefully there's just one
CABALMACROSFLAGS=-optP-include -optP hledger/.stack-work/dist/*/*/build/autogen/cabal_macros.h

BUILDFLAGS1:=-rtsopts $(WARNINGS) $(INCLUDEPATHS) $(PREFERMACUSRLIBFLAGS) $(GHCMEMFLAGS) $(CABALMACROSFLAGS) -DPATCHLEVEL=$(PATCHLEVEL) -DDEVELOPMENT
BUILDFLAGS:=$(BUILDFLAGS1) -DVERSION='"$(VERSION)dev"'
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

# TIME:=$(shell date +"%Y%m%d%H%M")

###############################################################################
$(call def-help-subsection,INSTALLING:)

install: \
	$(call def-help,install, download dependencies and install hledger executables to ~/.local/bin or equivalent (with stack))
	stack install

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
$(call def-help-subsection,BUILDING:)

# EXTRAINSTALLARGS=

build: \
	$(call def-help,build, download dependencies and build hledger executables (with stack))
	stack build

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

# link-web-dirs: config messages static templates \
# 	$(call def-help,link-web-dirs,\
# 	\
# 	)

# config: \
# 	$(call def-help,config,\
# 	\
# 	)
# 	ln -sf hledger-web/$@

# messages: \
# 	$(call def-help,messages,\
# 	\
# 	)
# 	ln -sf hledger-web/$@

# static: \
# 	$(call def-help,static,\
# 	\
# 	)
# 	ln -sf hledger-web/$@

# templates: \
# 	$(call def-help,templates,\
# 	\
# 	)
# 	ln -sf hledger-web/$@

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
	stack build hledger-lib hledger --library-profiling --executable-profiling --ghc-options=-fprof-auto
	cp `stack exec which hledger`{,prof}
	@echo to profile, use stack exec -- hledgerprof ...

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

#	hledger-lib/Hledger/Read/TimelogReaderPP.hs
dev: dev.hs $(SOURCEFILES) \
	$(call def-help,dev, build the dev.hs script for quick experiments (with ghc) )
	stack ghc -- $(CABALMACROSFLAGS) -ihledger-lib dev.hs \

dev-profile: $(SOURCEFILES) \
	$(call def-help,dev-profile, profile the dev.hs script )
	stack ghc -- $(CABALMACROSFLAGS) -ihledger-lib dev.hs -prof -fprof-auto -osuf p_o \
	&& time ./dev +RTS -P \
	&& cp dev.prof dev.prof.$(TIME) \
	&& profiteur dev.prof

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

tools/simplebench: tools/simplebench.hs \
		$(call def-help,tools/simplebench, build the standalone generic benchmark runner. Requires libs installed by stack build --bench. )
	stack exec -- $(GHC) tools/simplebench.hs

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

###############################################################################
$(call def-help-subsection,TESTING:)

# packdeps: \
# 	$(call def-help,packdeps,\
# 	run packdeps on each package to check for disallowed newer dependencies\
# 	)
# 	for p in $(PACKAGES); do packdeps $$p/$$p.cabal; done

test: pkgtest builtintest functest \
	$(call def-help,test, run default tests )

# test-ghc-%: # bin/hledgerdev.ghc-$* \
# 	$(call def-help,test-ghc-%,\
# 	run default tests with a specific GHC version\
# 	some functional tests (add, include, read-csv..) have bin/hledgerdev hard coded - might need to symlink it\
# 	)
# 	@echo; echo testing hledger built with ghc-$*
# 	@(echo unit tests: \
# 	&& bin/hledgerdev.ghc-$* test \
# 	&& echo functional tests: \
# 	&& $(SHELLTEST) tests -w bin/hledgerdev.ghc-$* \
# 	&& echo $@ PASSED) || echo $@ FAILED

# test-ghcall: bin/hledgerdev.ghcall \
# 	test-ghc-7.6.1 \
# 	test-ghc-7.4.1 \
# 	test-ghc-7.2.2 \
# 	test-ghc-7.0.4 \
# 	$(call def-help,test-ghcall,\
# 	run default tests with all supported GHC versions\
# 	)

# codetest: unittest functest \
# 	$(call def-help,codetest,\
# 	quick code tests, to be run frequently\
# 	)

# committest: hlinttest unittest doctest functest haddocktest warningstest quickcabaltest \
# 	$(call def-help,committest,\
# 	more thorough pre-commit/pre-push tests\
# 	)

# # releasetest: Clean unittest functest fullcabaltest haddocktest #warningstest doctest \
# # 	$(call def-help,releasetest,\
# # 	pre-release tests\
# # 	)

# hlinttest hlint: \
# 	$(call def-help,hlinttest (or hlint),\
# 	generate a hlint report\
# 	)
# 	hlint --hint=hlint --report=hlint.html $(SOURCEFILES)

#@echo package tests:
pkgtest: \
	$(call def-help,pkgtest, run the test suites for each package )
	@(stack test \
		&& echo $@ PASSED) || echo $@ FAILED

# NB ensure hledger executable is current (eg do pkgtest first)
#@echo "built-in tests (hledger cli unit tests)":
builtintest: \
	$(call def-help,builtintest, run tests built in to the hledger executable (subset of pkg tests) )
	@(stack exec hledger test \
		&& echo $@ PASSED) || echo $@ FAILED

# builtintestghc: bin/hledgerdev \
# 	$(call def-help,builtintest, run built-in unit tests with ghc build )
# 	@(bin/hledgerdev test \
# 		&& echo $@ PASSED) || echo $@ FAILED

# builtintestghc-%: bin/hledgerdev \
# 	$(call def-help,builtintest-PAT, run built-in unit tests whose name contains PAT )
# 	@(bin/hledgerdev test $* \
# 		&& echo $@ PASSED) || echo $@ FAILED

# builtintestghc-interpreted: \
# 	$(call def-help,builtintest-interpreted,\
# 	run built-in unit tests without waiting for compilation\
# 	)
# 	@echo "builtin tests (interpreted)":
# 	@(run$(GHC) $(MAIN) test \
# 		&& echo $@ PASSED) || echo $@ FAILED

# NB ensure hledger executable is current (eg do pkgtest first)
functest: tests/addons/hledger-addon \
	$(call def-help,functest, run hledger functional tests with the stack build )
	@(COLUMNS=80 PATH=`pwd`/bin:$(PATH) $(SHELLTESTSTK) tests \
		&& echo $@ PASSED) || echo $@ FAILED

#@echo functional tests:
functestdef: bin/hledgerdev tests/addons/hledger-addon \
	$(call def-help-hide,functestdef, run hledger functional tests with the hledger in PATH )
	@(COLUMNS=80 PATH=`pwd`/bin:$(PATH) $(SHELLTEST) tests \
		&& echo $@ PASSED) || echo $@ FAILED

# ADDONEXTS=pl py rb sh hs lhs rkt exe com bat
# tests/addons/hledger-addon: \
# 	$(call def-help,tests/addons/hledger-addon,\
# 	generate dummy add-ons for testing (hledger-addon the rest)\
# 	)
# 	rm -rf tests/addons/hledger-*
# 	printf '#!/bin/sh\necho add-on: $$0\necho args: $$*\n' >tests/addons/hledger-addon
# 	for E in '' $(ADDONEXTS); do \
# 		cp tests/addons/hledger-addon tests/addons/hledger-addon.$$E; done
# 	for F in addon. addon2 addon2.hs addon3.exe addon3.lhs addon4.exe add reg; do \
# 		cp tests/addons/hledger-addon tests/addons/hledger-$$F; done
# 	mkdir tests/addons/hledger-addondir
# 	chmod +x tests/addons/hledger-*

# DOCTESTFILES=\
# 	hledger/Hledger/Cli/Tests.hs
# doctest: tools/doctest \
# 	$(call def-help,doctest,\
# 	run doc tests\
# 	)
# 	@for f in $(DOCTESTFILES); do \
# 		(tools/doctest $$f && echo $@ PASSED) || echo $@ FAILED ; done

haddocktest: \
	$(call def-help,haddocktest, run haddock and make sure it succeeds )
	@(make --quiet haddock \
		&& echo $@ PASSED) || echo $@ FAILED

# warningstest: \
# 	$(call def-help,warningstest,\
# 	make sure the normal build has no warnings XXX needs updating\
# 	)
# 	@(make -s clean \
# 		&& make --no-print-directory -s hledgernowarnings \
# 		&& echo $@ PASSED) || echo $@ FAILED

cabalfiletest: \
	$(call def-help,cabalfiletest, run cabal check to test cabal file syntax )
	@(make --no-print-directory cabalcheck \
		&& echo $@ PASSED) || echo $@ FAILED

# quickcabaltest: \
# 	$(call def-help,quickcabaltest,\
# 	make sure cabal is reasonably happy\
# 	)
# 	@(make --no-print-directory cabalclean cabalcheck cabalconfigure \
# 		&& echo $@ PASSED) || echo $@ FAILED

# fullcabaltest: \
# 	$(call def-help,fullcabaltest,\
# 	make sure cabal is happy in all possible ways\
# 	)
# 	(for p in $(PACKAGES); do ( \
# 		printf "\ntesting $$p package\n" \
# 		&& cd $$p \
# 		&& cabal clean \
# 		&& cabal check \
# 		&& $(CABALINSTALL) \
# 		&& cabal sdist \
# 		); done \
# 		&& echo $@ PASSED) || echo $@ FAILED
# #		&& cabal upload dist/$$p-$(VERSION).tar.gz --check -v3 \

# quickbench: samplejournals tests/bench.tests tools/simplebench \
# 	$(call def-help,quickbench,\
# 	run simple performance benchmarks without saving results\
# 	Requires some commands defined in tests/bench.tests and some BENCHEXES defined above.\
# 	)
# 	tools/simplebench -v -ftests/bench.tests $(BENCHEXES)
# 	@rm -f benchresults.*

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
	stack exec -- hledgerprof +RTS $(PROFRTSFLAGS) -RTS $* -f data/10000x1000x10.journal >/dev/null
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
	stack exec -- hledgerprof +RTS -hc -RTS $* -f data/10000x1000x10.journal >/dev/null
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

ghci: \
	 	$(call def-help,ghci, start a GHCI REPL and load the hledger-lib and hledger packages)
	stack exec $(GHCI) -- $(BUILDFLAGS) hledger/Hledger/Cli/Main.hs

ghci-ui: \
		$(call def-help,ghci-ui, start a GHCI REPL and load the hledger-lib, hledger and hledger-ui packages)
	stack exec $(GHCI) -- $(BUILDFLAGS) hledger-ui/Hledger/UI/Main.hs

ghci-web: \
		$(call def-help,ghci-web, start a GHCI REPL and load the hledger-lib, hledger and hledger-web packages)
	stack exec $(GHCI) -- $(BUILDFLAGS) hledger-web/app/main.hs

samplejournals: data/sample.journal data/100x100x10.journal data/1000x1000x10.journal data/1000x10000x10.journal data/10000x1000x10.journal data/10000x10000x10.journal data/100000x1000x10.journal \
	$(call def-help,samplejournals, regenerate standard sample journals in data/ )

data/sample.journal:
	true # XXX should probably regenerate this

data/100x100x10.journal: tools/generatejournal
	tools/generatejournal 100 100 10 >$@

data/1000x1000x10.journal: tools/generatejournal
	tools/generatejournal 1000 1000 10 >$@

data/1000x10000x10.journal: tools/generatejournal
	tools/generatejournal 1000 10000 10 >$@

data/10000x1000x10.journal: tools/generatejournal
	tools/generatejournal 10000 1000 10 >$@

data/10000x10000x10.journal: tools/generatejournal
	tools/generatejournal 10000 10000 10 >$@

data/100000x1000x10.journal: tools/generatejournal
	tools/generatejournal 100000 1000 10 >$@

###############################################################################
$(call def-help-subsection,DOCUMENTATION:)

# docs: site codedocs \
# 	$(call def-help,docs,\
# 	rebuild all docs\
# 	)

# cleandocs: site-clean \
# 	$(call def-help,cleandocs,\
# 	\
# 	)

doc/site/hakyll-std hakyll-std: \
	doc/site/hakyll-std.hs \
	doc/site/TableOfContents.hs \
		$(call def-help,hakyll-std, build a generic hakyll site builder script )
	cd doc/site; stack ghc hakyll-std

site: doc/site/hakyll-std \
	$(call def-help,site, generate the hledger.org website with hakyll-std )
	-cd doc/site; ./hakyll-std build

site-clean: doc/site/hakyll-std \
	$(call def-help,site-clean, remove hakyll-generated files (& take down the website) ) #cleanolddocs
	-cd doc/site; ./hakyll-std clean
#	rm -rf doc/site/_site/*

# XXX hakyll watch & preview mostly don't live-update any more
site-preview: doc/site/hakyll-std \
	$(call def-help,site-preview, run a hakyll server to preview the website  ) #doc/site/site
	-cd doc/site; ./hakyll-std watch # -h hledger.org

# site-view: site \
# 	$(call def-help,site-view,\
# 	\
# 	)
# 	$(VIEWHTML) doc/site/_site/index.html

# # site-auto:
# # 	cd doc/site; $(AUTOBUILD) site.hs -o site $(PREFERMACUSRLIBFLAGS) --run preview

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
	stack haddock --no-haddock-deps --no-keep-going # && echo OK
#	stack -v haddock --no-haddock-deps --no-keep-going # && echo OK

# view-haddock: \
# 	$(call def-help,view-haddock-cli,\
# 	view the haddock generated for the hledger package\
# 	)
# 	$(VIEWHTML) hledger/dist/doc/html/hledger/index.html

# # http://www.cs.york.ac.uk/fp/darcs/hscolour/
# HSCOLOUR=HsColour -css
# hscolour: doc/site/api/src doc/site/api/src/hscolour.css \
# 	$(call def-help,hscolour,\
# 	\
# 	)
# 	for f in $(HADDOCKSOURCEFILES); do \
# 		$(HSCOLOUR) -anchor $$f -odoc/site/api/src/`echo $$f | sed -e's%[^/]*/%%' | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
# 	done

# doc/site/api/src/hscolour.css: doc/site/api/src \
# 	$(call def-help,doc/site/api/src/hscolour.css,\
# 	\
# 	)
# 	$(HSCOLOUR) -print-css >doc/site/api/src/hscolour.css

# doc/site/api/src: \
# 	$(call def-help,doc/site/api/src,\
# 	\
# 	)
# 	mkdir -p doc/site/api/src

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

###############################################################################
$(call def-help-subsection,RELEASING:)
#$(call def-help-subsection,see also developer guide -> how to -> do a release)

# # XXX UPDATE
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
# # - "make simplebench" depends on version numbers in BENCHEXES, these also
# #   must be updated manually.
# #
# # - "make" updates the version in most other places, and defines PATCHES.
# #   Note "cabal build" should also do this but doesn't yet.
# #
# # - "make release" additionally records the main version number-affected
# #   files, and tags the repo with the release tag.

# release: releasetest setandrecordversion tagrelease \
# 	$(call def-help,release,\
# 	Build a release, tag the repo, prepare a cabal package\
# 	First update .version. Eg:\
# 	a normal release: echo 0.7   >.version; make release\
# 	a bugfix release: echo 0.7.1 >.version; make release\
# 	)

# upload: cabalsdist hackageupload pushdocs \
# 	$(call def-help,upload,\
# 	Upload the latest cabal package and update hledger.org\
# 	)

# releaseandupload: release upload \
# 	$(call def-help,releaseandupload,\
# 	\
# 	)

# # setandrecordversion: setversion \
# # 	$(call def-help,setandrecordversion,\
# # 	update the version number in local files, and prompt to record changes\
# # 	in these files. Triggered by "make release".\
# # 	)
# # 	darcs record -m "bump version" $(VERSIONFILE) $(VERSIONSENSITIVEFILES)

setversion: $(VERSIONSENSITIVEFILES) \
	$(call def-help,setversion, update all version strings to match $(VERSIONFILE) )

setversionforce:\
	$(call def-help,setversionforce, update all version strings even if $(VERSIONFILE) seems unchanged)
	touch $(VERSIONFILE); make setversion

# XXX may need fixing:

hledger-lib/package.yaml: $(VERSIONFILE) \
	$(call def-help-hide,hledger-lib/package.yaml, update the version in this file )
	perl -p -e "s/(^version *: *).*/\$${1}'$(VERSION)'/" -i $@

hledger/package.yaml: $(VERSIONFILE) \
	$(call def-help-hide,hledger/package.yaml, update the version in this file )
	perl -p -e "s/(^version *: *).*/\$${1}'$(VERSION)'/" -i $@
	perl -p -e "s/(hledger(-lib)? *[>=]= *).*/\$${1}$(VERSION)/" -i $@
	perl -p -e "s/(-DVERSION=\")[^\"]+/\$${1}$(VERSION)/" -i $@

hledger-web/package.yaml: $(VERSIONFILE) \
	$(call def-help-hide,hledger-web/package.yaml, update the version in this file )
	perl -p -e "s/(^version *: *).*/\$${1}'$(VERSION)'/" -i $@
	perl -p -e "s/(hledger(-lib|-web)? *[>=]= *).*/\$${1}$(VERSION)/" -i $@
	perl -p -e "s/(-DVERSION=\")[^\"]+/\$${1}$(VERSION)/" -i $@

doc/manual.md: $(VERSIONFILE) \
	$(call def-help-hide,doc/MANUAL.md, update the version in this file )
	perl -p -e "s/(this version documents hledger and hledger-web) +[0-9.]+/\1 $(VERSION)/" -i $@

tagrelease: \
	$(call def-help,tagrelease, commit a release tag based on $(VERSIONFILE) for each package )
	for p in $(PACKAGES); do git tag $$p-$(VERSION); done

# hackageupload-dry: \
# 	$(call def-help,hackageupload-dry,\
# 	upload all packages to hackage; dry run\
# 	)
# 	for p in $(PACKAGES); do cabal upload $$p/dist/$$p-$(VERSION).tar.gz -v2 --check; done

# hackageupload: \
# 	$(call def-help,hackageupload,\
# 	upload all packages to hackage\
# 	)
# 	for p in $(PACKAGES); do cabal upload $$p/dist/$$p-$(VERSION).tar.gz -v2; done

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
# #	simplebench
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

# # XXX
# showreleaseauthors: \
# 	$(call def-help,showreleaseauthors,\
# 	\
# 	)
# 	@echo Patch authors since last release:
# 	@darcs changes --from-tag $(FROMTAG) |grep '^\w' |cut -c 31- |sort |uniq
# 	@echo

# showloc: \
# 	$(call def-help,showloc,\
# 	\
# 	)
# 	@echo Current lines of code including tests:
# 	@sloccount `ls $(SOURCEFILES)` | grep haskell:
# 	@echo

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
$(call def-help-subsection,MISCELLANEOUS:)

# allow automatic variables (using $$) in the prerequisites of subsequent rules
.SECONDEXPANSION:

# XXX enable for all cabal files when hpack is a little better
# gencabalfiles: $$(CABALFILES)
gencabalfiles: doc/site/hakyll-std.cabal \
		$(call def-help,gencabalfiles, regenerate cabal files from their package.yaml definitions )

%.cabal: $$(dir $$@)package.yaml \
		$(call def-help-hide,PATH/SOME.cabal, regenerate a cabal file from its package.yaml definition with hpack )
	cd $(dir $*) && \
	hpack && \
	touch $(notdir $@) && \
	cabal check

cabal%: \
	$(call def-help,cabalCMD, run cabal CMD inside each hledger package directory )
	for p in $(PACKAGES); do (cd $$p; cabal $*); done

all%: \
	$(call def-help,all"CMD", run CMD inside each hledger package directory )
	for p in $(PACKAGES); do (cd $$p; $*); done

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

tag: emacstags \
	$(call def-help,tag, generate tag files for source code navigation (for emacs) )

emacstags:
	-@rm -f TAGS; hasktags -e $(SOURCEFILES) $(WEBFILES) $(CABALFILES) $(DOCFILES) Makefile

cleantags: \
	$(call def-help-hide,cleantags, remove tag files )
	rm -f TAGS tags

stackclean: \
	$(call def-help-hide,stackclean, remove .stack-work/* in packages (but not in project) )
	stack clean

Stackclean: \
	$(call def-help-hide,Stackclean, remove all stack working dirs )
	stack clean

cleanghco: \
	$(call def-help-hide,cleanghc, remove ghc build leftovers )
	rm -rf `find . -name "*.o" -o -name "*.hi" -o -name "*.dyn_o" -o -name "*.dyn_hi" -o -name "*~" | grep -vE '\.(stack-work|cabal-sandbox|virthualenv)'`

cleanghc: cleanghco \
	$(call def-help-hide,cleanghc, remove ghc builds )
	rm -f bin/hledgerdev bin/hledgerdev.ghc*

clean: cleanghco \
	$(call def-help,clean, default cleanup (ghc build leftovers) )

Clean: stackclean cabalclean cleanghc cleantags \
	$(call def-help,Clean, thorough cleanup (stack/cabal/ghc builds and tags) )

# reverse = $(if $(wordlist 2,2,$(1)),$(call reverse,$(wordlist 2,$(words $(1)),$(1))) $(firstword $(1)),$(1))

###############################################################################
# LOCAL UNTRACKED CUSTOMISATIONS

-include local.mk

$(call def-help-section,------------------)
