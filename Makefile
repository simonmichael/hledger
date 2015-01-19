# hledger project makefile

# GHC-compiled executables require a locale (and not just C) or they
# will die on encountering non-ascii data
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
SHELLTEST=shelltest
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



defaulttarget: bin/hledgerdev

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

sandbox: .cabal-sandbox

.cabal-sandbox:
	cabal sandbox init
	cabal sandbox add-source ./hledger-lib ./hledger ./hledger-web

# cabal install the main hledger packages and all their dependencies
# in the sandbox if any, otherwise in the user's package db
install:
	$(CABALINSTALL) $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS) --enable-tests

# cabal install the main hledger packages and all their dependencies more forcibly
# (can break installed libs, requiring ghc-pkg-clean)
install-force:
	$(CABALINSTALL) $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS) --enable-tests --allow-newer --force-reinstalls

# install all cabal dependencies for the main hledger packages
installdeps:
	$(CABALINSTALL) $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS) --enable-tests --only-dependencies

# install all cabal dependencies for the main hledger packages, more forcibly
# (may break installed libs, requiring ghc-pkg-clean)
installdeps-force:
	$(CABALINSTALL) $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS) --enable-tests --only-dependencies --allow-newer --force-reinstalls

# unregister all packages, assuming they are defined lowest-dependency first
# avoids some reinstall noise when repeatedly doing make install
uninstall:
	-for p in $(call reverse,$(PACKAGES)); do $(GHCPKG) unregister $$p; done

# utility function
reverse = $(if $(wordlist 2,2,$(1)),$(call reverse,$(wordlist 2,$(words $(1)),$(1))) $(firstword $(1)),$(1))

# run a cabal command in all hledger package dirs
cabal%:
	for p in $(PACKAGES); do (echo doing cabal $* in $$p; cd $$p; cabal $*; echo); done

# # run a command in all hledger package dirs
# all%:
# 	for p in $(PACKAGES); do (echo doing $* in $$p; cd $$p; $*); done

Reset:
	cabal sandbox delete

######################################################################
# BUILDING

EXTRAINSTALLARGS=

# auto-recompile and run (something, eg --help or unit tests) whenever a module changes

auto: auto---version

auto-%: sp
	$(AUTOBUILD) $(MAIN) -o bin/hledgerdev $(AUTOBUILDFLAGS) --run $*

autoweb: sp link-web-dirs
	$(AUTOBUILD) hledger-web/app/main.hs -o bin/hledger-webdev $(AUTOBUILDFLAGS) $(WEBLANGEXTS) --run -B --port 5001 --base-url http://localhost:5001 -f webtest.j

link-web-dirs: config messages static templates

config:
	ln -sf hledger-web/$@

messages:
	ln -sf hledger-web/$@

static:
	ln -sf hledger-web/$@

templates:
	ln -sf hledger-web/$@

# check for sp and explain how to get it if not found.
sp:
	@/usr/bin/env which sp >/dev/null || \
	  (echo '"sp" is required for auto-compilation. darcs get http://joyful.com/darcsden/simon/searchpath, make it (cabal install-ing any needed packages) and add it to your PATH'; exit 1)

# force a compile even if binary exists, since we don't specify dependencies for these
.PHONY: bin/hledgerdev bin/hledger-prof bin/hledgeropt bin/hledger-webdev

# build hledger developer binary, ie as quickly as possible
# requires cabal macros, generated by doing a cabal build in hledger/
bin/hledgerdev:
	$(GHC) $(MAIN) -o bin/hledgerdev $(BUILDFLAGS)

# build a GHC-version-specific hledger binary without disturbing with other GHC version builds
bin/hledgerdev.ghc-%: $(SOURCEFILES)
	ghc-$*  $(MAIN) -o $@ $(BUILDFLAGS)  -outputdir .ghc-$*

# build hledger with the main supported GHC versions
bin/hledgerdev.ghcall: \
	bin/hledgerdev.ghc-7.6.1 \
	bin/hledgerdev.ghc-7.4.1 \
	bin/hledgerdev.ghc-7.2.2 \
	bin/hledgerdev.ghc-7.0.4 \
#	bin/hledgerdev.ghc-6.12.3 \

# build the "production" optimised cabal build with profiling enabled. Assumes a cabal sandbox.
bin/hledger:
	rm -f bin/hledger
	cabal install --disable-library-profiling --disable-executable-profiling ./hledger-lib ./hledger \
		&& mv .cabal-sandbox/bin/hledger bin/hledger

# build the "production" cabal build with profiling enabled.
bin/hledger-prof:
	rm -f bin/hledger-prof
	cabal install -p --enable-executable-profiling --ghc-options=-fprof-auto ./hledger-lib ./hledger \
		&& mv .cabal-sandbox/bin/hledger bin/hledger-prof

# build the dev build with profiling enabled.
# not working with cabal sandbox
# bin/hledgerdev-prof:
# 	$(GHC) $(BUILDFLAGS) $(PROFBUILDFLAGS) $(MAIN) -o $@

# build the heap profiling binary for coverage reports and heap profiles.
# Keep these .o files separate from the regular ones.
hledgerhpc:
	$(GHC) $(MAIN) -fhpc -o bin/hledgerhpc -outputdir .hledgerhpcobjs $(BUILDFLAGS)

# build other executables quickly

bin/hledger-webdev: link-web-dirs
	$(GHC) -o $@ $(BUILDFLAGS) $(WEBLANGEXTS) hledger-web/app/main.hs

bin/hledger-web-production:
	$(GHC) -o $@ $(BUILDFLAGS) $(WEBLANGEXTS) hledger-web/app/main.hs

# build portable releaseable binaries for gnu/linux
linuxbinaries: 	linuxbinary-hledger \
		linuxbinary-hledger-web
	@echo 'Please check the binaries look portable, then make compressbinaries:'
	-file bin/*`arch`

# work around for inconsistently-named (why ?) hledger/app/hledger-cli.hs
linuxbinary-hledger:
	$(GHC) hledger/app/hledger-cli.hs -o bin/$*$(RELEASEBINARYSUFFIX) $(LINUXRELEASEBUILDFLAGS)

linuxbinary-%:
	$(GHC) $*/$*.hs -o bin/$*$(RELEASEBINARYSUFFIX) $(LINUXRELEASEBUILDFLAGS)

macbinaries:    macbinary-hledger \
		macbinary-hledger-web
	@echo 'Please check the binaries are portable, then make compressbinaries'
	otool -L bin/*`arch`

# build a deployable mac binary for the specified hledger package, munging
# the link command to use only standard osx libs.  Specifically we link
# without the non-standard GMP framework, which causes no apparent harm.
# Clunky, does the link twice.
macbinary-%:
	BINARY=`echo $(BINARYFILENAME) | sed -e 's/hledger/$*/'` ; \
	LINKCMD=`$(GHC) -v  $*/$*.hs $(MACRELEASEBUILDFLAGS) -o bin/$$BINARY 2>&1 | egrep "bin/gcc.*bin/$$BINARY"` ; \
	PORTABLELINKCMD=`echo $$LINKCMD | sed -e 's/ -framework GMP//'` ; \
	echo $$PORTABLELINKCMD; $$PORTABLELINKCMD

# Run this on a windows machine or in a wine session, and probably in a
# separate copy of the repo (hledger-win).
# Builds and gather deployable binaries for windows, if cygwin tools are
# present and all packages are buildable. Otherwise, cabal install each
# package and gather the binaries by hand.
windowsbinaries: install
	cp ~/.cabal/bin/hledger.exe bin/`echo $(BINARYFILENAME) | dos2unix`
	-cp ~/.cabal/bin/hledger-web.exe bin/`echo $(BINARYFILENAME) | sed -e 's/hledger/hledger-web/' | dos2unix`
	@echo 'Please check the binaries are portable, then make compressbinaries'
	ls -l bin/*`arch`

# various ways of getting a wine shell
# command-line windows command prompt. Works eg in an emacs shell buffer.
wine-cmd:
	wine cmd

# as above but try to cd somewhere useful (doesn't work), also ctrl-d exits quickly
wine-cmd2:
	(echo c:; echo cd \\mingw\\msys\\1.0; cat) | wine cmd

# windows command prompt in a new window
wine-cmd-window:
	wineconsole cmd &

# msys bash shell in a mintty window
wine-mintty:
	wine c:/mingw/msys/1.0/bin/mintty - &

compressbinaries:
	cd bin; for f in *-windows-*.exe ; do echo zipping $$f; rm -f $$f.zip; zip $$f.zip $$f; done
#	for f in bin/*-{linux,mac-}* ; do echo gzipping $$f; gzip -q $$f >$$f.gz; done
#	gzip bin/*`arch`

# build the standalone unit test runner. Requires test-framework, which
# may not work on windows.
tools/unittest: tools/unittest.hs
	$(GHC) -threaded -O2 tools/unittest.hs

# build the doctest runner
tools/doctest: tools/doctest.hs
	$(GHC) tools/doctest.hs

# build the simple benchmark runner. Requires html and tabular.
tools/simplebench: tools/simplebench.hs
	$(GHC) tools/simplebench.hs

# build the criterion-based benchmark runner. Requires criterion.
tools/criterionbench: tools/criterionbench.hs
	$(GHC) tools/criterionbench.hs

# build the progression-based benchmark runner. Requires progression.
tools/progressionbench: tools/progressionbench.hs
	$(GHC) tools/progressionbench.hs

# build the generatejournal tool
tools/generatejournal: tools/generatejournal.hs
	$(GHC) tools/generatejournal.hs

######################################################################
# TESTING

# developer environment checks

check:
	@echo sanity-check developer environment:
	@($(SHELLTEST) checks -- \
		&& echo $@ PASSED) || echo $@ FAILED



# run packdeps on each package to check for disallowed newer dependencies
packdeps:
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

test: codetest

# quick code tests - run all the time
codetest: unittest functest

# moderate pre-commit tests - run before record or before send/push, your choice
committest: hlinttest unittest doctest functest haddocktest warningstest quickcabaltest

# thorough pre-release tests - run before release
# consider hiding dev-build symlinks in Hledger/ first
releasetest: Clean unittest functest fullcabaltest haddocktest #warningstest doctest

hlinttest hlint:
	hlint --hint=hlint --report=hlint.html $(SOURCEFILES)

# run unit tests
unittest: unittest-builtin

unittest-builtin: bin/hledgerdev
	@echo unit tests:
	@(bin/hledgerdev test \
		&& echo $@ PASSED) || echo $@ FAILED

unittest-%: bin/hledgerdev
	@echo unit tests:
	@(bin/hledgerdev test $* \
		&& echo $@ PASSED) || echo $@ FAILED

# XXX doesn't rebuild on hledger source changes
unittest-standalone: tools/unittest
	@echo unit tests (standalone):
	@(tools/unittest \
		&& echo $@ PASSED) || echo $@ FAILED

# run unit tests without waiting for compilation
unittest-interpreted:
	@echo "unit tests (interpreted)":
	@(run$(GHC) $(MAIN) test \
		&& echo $@ PASSED) || echo $@ FAILED

# run functional tests, requires shelltestrunner >= 0.9 from hackage
# 16 threads sometimes gives "commitAndReleaseBuffer: resource vanished (Broken pipe)" here but seems harmless
functest: bin/hledgerdev tests/addons/hledger-addon
	@echo functional tests:
	@(COLUMNS=80 $(SHELLTEST) --execdir tests --threads=16 \
		&& echo $@ PASSED) || echo $@ FAILED

# generate dummy add-ons for testing (hledger-addon the rest)
ADDONEXTS=pl py rb sh hs lhs rkt exe com bat
tests/addons/hledger-addon:
	rm -rf tests/addons/hledger-*
	printf '#!/bin/sh\necho add-on: $$0\necho args: $$*\n' >tests/ADDONS/hledger-addon
	for E in '' $(ADDONEXTS); do \
		cp tests/ADDONS/hledger-addon tests/ADDONS/hledger-addon.$$E; done
	for F in addon. addon2 addon2.hs addon3.exe addon3.lhs addon4.exe add reg; do \
		cp tests/ADDONS/hledger-addon tests/ADDONS/hledger-$$F; done
	mkdir tests/ADDONS/hledger-addondir
	chmod +x tests/ADDONS/hledger-*

# run unit and functional tests with a specific GHC version
# some functional tests (add, include, read-csv..) have bin/hledgerdev hard coded - might need to symlink it
test-ghc-%: # bin/hledgerdev.ghc-$*
	@echo; echo testing hledger built with ghc-$*
	@(echo unit tests: \
	&& bin/hledgerdev.ghc-$* test \
	&& echo functional tests: \
	&& $(SHELLTEST) tests -w bin/hledgerdev.ghc-$* -- --threads=16 --hide-successes \
	&& echo $@ PASSED) || echo $@ FAILED

# run unit and functional tests with main supported GHC versions
test-ghcall: bin/hledgerdev.ghcall \
	test-ghc-7.6.1 \
	test-ghc-7.4.1 \
	test-ghc-7.2.2 \
	test-ghc-7.0.4 \
#	test-ghc-6.12.3 \

# run doc tests
DOCTESTFILES=\
	hledger/Hledger/Cli/Tests.hs
doctest: tools/doctest
	@for f in $(DOCTESTFILES); do \
		(tools/doctest $$f && echo $@ PASSED) || echo $@ FAILED ; done

# make sure we have no haddock errors
haddocktest:
	@(make --quiet haddock \
		&& echo $@ PASSED) || echo $@ FAILED

# needs updating
# make sure the normal build has no warnings
warningstest:
	@(make -s clean \
		&& make --no-print-directory -s hledgernowarnings \
		&& echo $@ PASSED) || echo $@ FAILED

# make sure cabal is reasonably happy
quickcabaltest:
	@(make --no-print-directory cabalclean cabalcheck cabalconfigure \
		&& echo $@ PASSED) || echo $@ FAILED

# make sure cabal is happy in all possible ways
fullcabaltest:
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

# run simple performance benchmarks without saving results
# Requires some commands defined in tests/bench.tests and some BENCHEXES defined above.
quickbench: samplejournals tests/bench.tests tools/simplebench
	tools/simplebench -v -ftests/bench.tests $(BENCHEXES)
	@rm -f benchresults.*

# run simple performance benchmarks and archive results
# Requires some commands defined in tests/bench.tests and some BENCHEXES defined above.
bench: samplejournals tests/bench.tests tools/simplebench
	tools/simplebench -v -ftests/bench.tests $(BENCHEXES) | tee doc/profs/$(TIME).bench
	@rm -f benchresults.*
	@(cd doc/profs; rm -f latest.bench; ln -s $(TIME).bench latest.bench)

# run criterion benchmark tests and save graphical results
criterionbench: samplejournals tools/criterionbench
	tools/criterionbench -t png -k png

# run progression benchmark tests and save graphical results
progressionbench: samplejournals tools/progressionbench
	tools/progressionbench -- -t png -k png

# generate and archive an execution profile
prof: samplejournals #bin/hledger-prof
	@echo "Profiling: $(PROFCMD)"
	-$(PROFCMD) +RTS $(PROFRTSFLAGS) -RTS
	mv hledger-prof.prof doc/profs/$(TIME).prof
	(cd doc/profs; rm -f latest*.prof; ln -s $(TIME).prof latest.prof)

# generate, archive, simplify and display an execution profile
viewprof: prof
	tools/simplifyprof.hs doc/profs/latest.prof

# generate and display an execution profile, don't save or simplify
quickprof: samplejournals #bin/hledger-prof
	@echo "Profiling: $(PROFCMD)"
	-$(PROFCMD) +RTS $(PROFRTSFLAGS) -RTS
	echo; cat hledger-prof.prof

# generate and archive a graphical heap profile
heap: samplejournals #bin/hledger-prof
	@echo "Profiling heap with: $(PROFCMD)"
	$(PROFCMD) +RTS -hc -RTS
	mv hledger-prof.hp doc/profs/$(TIME).hp
	(cd doc/profs; rm -f latest.hp; ln -s $(TIME).hp latest.hp; \
		hp2ps $(TIME).hp; rm -f latest.ps; ln -s $(TIME).ps latest.ps; rm -f *.aux)

viewheap: heap
	$(VIEWPS) doc/profs/latest.ps

# generate and display a graphical heap profile, don't save
quickheap: samplejournals #bin/hledger-prof
	@echo "Profiling heap with: $(PROFCMD)"
	$(PROFCMD) +RTS -hc -RTS
	hp2ps hledger-prof.hp
	$(VIEWPS) hledger.ps

# display a code coverage text report from running hledger COVCMD
quickcoverage: hledgerhpc
	@echo "Generating code coverage text report for hledger command: $(COVCMD)"
	tools/runhledgerhpc "report" $(COVCMD)

# generate a code coverage html report from running hledger COVCMD
coverage: samplejournals hledgerhpc
	@echo "Generating code coverage html report for hledger command: $(COVCMD)"
	tools/runhledgerhpc "markup --destdir=doc/profs/coverage" $(COVCMD)
	cd doc/profs/coverage; rm -f index.html; ln -s hpc_index.html index.html

# view the last html code coverage report
viewcoverage:
	$(VIEWHTML) doc/profs/coverage/index.html

# single-package debug prompts, using all cabal settings

repl-lib:
	(cd hledger-lib; cabal repl)

repl-cli repl:
	(cd hledger; cabal repl exe:hledger)

repl-web:
	(cd hledger-web; cabal repl exe:hledger-web)

# multi-package debug prompts, mimicking most cabal settings

ghci:
	cabal exec $(GHCI) -- $(WARNINGS) $(INCLUDEPATHS) $(MAIN)

ghci-web:
	cabal exec $(GHCI) -- $(BUILDFLAGS) $(WEBLANGEXTS) hledger-web/app/main.hs

# generate standard sample journals
samplejournals: data/sample.journal data/100x100x10.journal data/1000x1000x10.journal data/1000x10000x10.journal data/10000x1000x10.journal data/10000x10000x10.journal data/100000x1000x10.journal

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

######################################################################
# DOCUMENTATION

# rebuild all docs
docs: site codedocs

cleandocs: site-clean

# build some additional static bits of the hledger.org website
# Requires hakyll-std, a generic hakyll site builder
site-build site: #olddocs
	-cd doc/site; hakyll build

site-clean: #cleanolddocs
	-cd doc/site; hakyll clean
#	rm -rf doc/site/_site/*

site-preview: #doc/site/site
	cd doc/site; hakyll preview

site-view: site
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


pdf: codepdf #docspdf

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

# push latest docs etc. and update the hledger.org site
pushdocs: push
	ssh simon@joyful.com 'make -C/repos/hledger docs'

# dump all executables' command line help into files for review
EXES=hledger hledger-vty hledger-web
savehelp:
	for e in $(EXES); do $$e --help >.HELP_$$e; done

# generate api & other code docs
codedocs: haddock hscolour coverage #sourcegraph #hoogle

# cf http://www.haskell.org/haddock/doc/html/invoking.html
# --ghc-options=-optP-P is a workaround for http://trac.haskell.org/haddock/ticket/284
HADDOCKFLAGS= \
	--haddock-options='--no-warnings' \
	--ghc-options='-optP-P' \

# build per-package haddocks using cabal
haddock: haddock-lib haddock-cli haddock-web

haddock-lib:
	(cd hledger-lib; cabal haddock $(HADDOCKFLAGS))

haddock-cli:
	(cd hledger; cabal haddock $(HADDOCKFLAGS))

haddock-web:
	(cd hledger-web; cabal haddock $(HADDOCKFLAGS))

# view-haddock-cli
view-haddock-cli:
	$(VIEWHTML) hledger/dist/doc/html/hledger/index.html

# view-haddock-lib, view-haddock-web
view-haddock-%:
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
hscolour: doc/site/api/src doc/site/api/src/hscolour.css
	for f in $(HADDOCKSOURCEFILES); do \
		$(HSCOLOUR) -anchor $$f -odoc/site/api/src/`echo $$f | sed -e's%[^/]*/%%' | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
	done

doc/site/api/src/hscolour.css: doc/site/api/src
	$(HSCOLOUR) -print-css >doc/site/api/src/hscolour.css

doc/site/api/src:
	mkdir -p doc/site/api/src

sourcegraph:
	for p in $(PACKAGES); do (cd $$p; SourceGraph $$p.cabal); done

patchdeps:
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

# Build a release, tag the repo, prepare a cabal package
# First update .version. Eg:
# a normal release: echo 0.7   >.version; make release
# a bugfix release: echo 0.7.1 >.version; make release
release: releasetest setandrecordversion tagrelease

# Upload the latest cabal package and update hledger.org
upload: cabalsdist hackageupload pushdocs

releaseandupload: release upload


# update the version number in local files, and prompt to record changes
# in these files. Triggered by "make release".
setandrecordversion: setversion
	darcs record -m "bump version" $(VERSIONFILE) $(VERSIONSENSITIVEFILES)

# update the version string in local files. This should be run immediately
# after editing the VERSION file.
setversion: $(VERSIONSENSITIVEFILES)

# re-update version string even if it seems unchanged
Setversion:
	touch $(VERSIONFILE); make setversion

hledger-lib/hledger-lib.cabal: $(VERSIONFILE)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@

hledger/hledger.cabal: $(VERSIONFILE)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger(-lib)? *[>=]=) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(-DVERSION=\")[^\"]+/\$${1}$(VERSION)/" -i $@

hledger-web/hledger-web.cabal: $(VERSIONFILE)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger(-lib|-web)? *[>=]=) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(-DVERSION=\")[^\"]+/\$${1}$(VERSION)/" -i $@

doc/MANUAL.md: $(VERSIONFILE)
	perl -p -e "s/(^Version:) +[0-9.]+/\1 $(VERSION)/" -i $@

tagrelease:
	for p in $(PACKAGES); do git tag $$p-$(VERSION); done

hackageupload-dry:
	for p in $(PACKAGES); do cabal upload $$p/dist/$$p-$(VERSION).tar.gz -v2 --check; done

hackageupload:
	for p in $(PACKAGES); do cabal upload $$p/dist/$$p-$(VERSION).tar.gz -v2; done

# send unpushed patches to the mail list
send:
	darcs send http://joyful.com/repos/hledger --to=hledger@googlegroups.com --edit-description  

# push patches and anything else pending to the public server
push: pushprofs # pushlatestbinary
	darcs push simon@joyful.com:/repos/hledger

# pull anything pending from the public server
pull: pullprofs
	darcs pull -a simon@joyful.com:/repos/hledger

RSYNC=rsync

# push any new profiles and benchmark results to the public site
# beware, results will vary depending on which machine generated them
pushprofs:
	$(RSYNC) -azP doc/profs/ simon@joyful.com:/repos/hledger/doc/profs/

# fetch any new profiles and benchmark results from the public site
pullprofs:
	$(RSYNC) -azP simon@joyful.com:/repos/hledger/doc/profs/ doc/profs/

# compress the just-built platform binary. make hledgerPLATFORM first. Use
# the win variant on windows.
compressbinary:
	gzip -9 bin/$(BINARYFILENAME)
compressbinarywin:
	cd bin; zip -9 $(BINARYFILENAME).zip $(BINARYFILENAME)

# push the last-updated platform binary to the public download directory
# pushlatestbinary:
# 	cd bin; $(RSYNC) -aP `ls -t | head -2` simon@joyful.com:/repos/hledger/site/download/


# show project stats useful for release notes
showreleasestats stats: \
	showreleasedays \
	showunreleasedchangecount \
	showloc \
	showtestcount \
	showunittestcoverage \
	showreleaseauthors \
	showunreleasedcodechanges \
	showunpushedchanges
#	simplebench
#	showerrors

FROMTAG=.

showreleasedays:
	@echo Days since last release:
	@tools/dayssincetag.hs $(FROMTAG) | head -1 | cut -d' ' -f-1
	@echo

showunreleasedchangecount:
	@echo Commits since last release:
	@darcs changes --from-tag $(FROMTAG) --count
	@echo

showreleaseauthors:
	@echo Patch authors since last release:
	@darcs changes --from-tag $(FROMTAG) |grep '^\w' |cut -c 31- |sort |uniq
	@echo

showloc:
	@echo Current lines of code including tests:
	@sloccount `ls $(SOURCEFILES)` | grep haskell:
	@echo

sloc:
	@sloccount hledger-lib hledger hledger-web

cloc:
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

showtestcount:
	@echo "Unit tests:"
	@hledger test 2>&1 | cut -d' ' -f2
	@echo "Functional tests:"
	@make --no-print functest | egrep '^ Total' | awk '{print $$2}'
	@echo

showunittestcoverage:
	@echo Unit test coverage:
	@make --no-print quickcoverage | grep 'expressions'
	@echo

# showerrors:
# 	@echo Known errors:
# 	@awk '/^** errors/, /^** / && !/^** errors/' NOTES.org | grep '^\*\*\* ' | tail +1
# 	@echo

showunpushedchanges showunpushed:
	@echo "Changes not yet pushed upstream (to `darcs show repo | grep 'Default Remote' | cut -c 17-`):"
	@-darcs push simon@joyful.com:/repos/hledger --dry-run | grep '*' | tac
	@echo

showunreleasedcodechanges showunreleased showchanges:
	@echo "hledger code changes since last release:"
	@darcs changes --from-tag $(FROMTAG) --matches "not (name docs: or name doc: or name site: or name tools:)" | grep '*'
	@echo

showcodechanges:
	@echo "hledger code changes:"
	@darcs changes --matches "not (name docs: or name site: or name tools:)" | egrep '^ +(\*|tagged)'
	@echo

######################################################################
# MISCELLANEOUS

# fix permissions (eg after darcs get)
fixperms:
	chmod +x tools/* $(MAIN) hledger-*/Main.hs

tag: emacstags

emacstags:
	-@rm -f TAGS; hasktags -e $(SOURCEFILES) $(WEBFILES) $(CABALFILES) $(DOCFILES) Makefile

cleanghc:
	rm -rf `find . -name "*.o" -o -name "*.hi" -o -name "*~" | grep -vE '(virthualenv|cabal-sandbox)'`

cleancabal: cabalclean

cleanbin:
	rm -f bin/hledgerdev bin/hledgerdev.ghc*

cleantags:
	rm -f TAGS tags

clean: cleanghc cleancabal

Clean: cleanghc cleancabal cleanbin cleantags

-include Makefile.local
