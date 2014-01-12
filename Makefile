# hledger project makefile

# ghc 6.12 executables need a locale
export LANG=en_US.UTF-8

# command to run during "make prof" and "make heap"
PROFCMD=bin/hledgerprof balance -f data/1000x1000x10.journal >/dev/null
PROFCMD=bin/hledgerprof -f test-wf.csv print

#PROFRTSFLAGS=-p
PROFRTSFLAGS=-P

# command to run during "make coverage"
COVCMD=test
COVCMD=-f test-wf.csv print

# executables to run during "make simplebench". They should be on the path
# or in the current directory. hledger executables for benchmarking should
# generally be the standard optimised cabal build, constrained to parsec 2.
BENCHEXES=hledger-0.18 hledger-0.19 hledger-0.20 ledger-3.0-20130215

# misc. tools
BROWSE=open -a Firefox
VIEWHTML=$(BROWSE)
VIEWPS=$(BROWSE)
VIEWPDF=$(BROWSE)
PRINT=lpr

GHC=ghc
HADDOCK=haddock
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
MAIN=hledger/hledger-cli.hs

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

# a more careful list suitable for for haddock
HADDOCKSOURCEFILES:= \
	hledger-lib/Hledger.hs \
	hledger-lib/Hledger/*hs \
	hledger-lib/Hledger/*/*hs \
	hledger/Hledger/*hs \
	hledger/Hledger/*/*hs \
	# hledger-web/Hledger/*hs \
	# hledger-web/Hledger/*/*hs \
	# hledger-web/app/*.hs \
	# hledger-web/Settings/*.hs

CABALFILES:= \
	hledger/hledger.cabal \
	hledger-*/*.cabal

WEBFILES:= \
	hledger-web/static/*.js \
	hledger-web/static/*.css

DOCFILES:= \
	doc/*.md

# files which should be updated when the version changes
VERSIONSENSITIVEFILES=\
	$(CABALFILES) \
	doc/MANUAL.md \
#	doc/DOWNLOAD.md \

# file(s) which require recompilation for a build to have an up-to-date version string
VERSIONSOURCEFILE=hledger/Hledger/Cli/Version.hs

# master file defining the current release/build version
VERSIONFILE=VERSION

# two or three-part version string, whatever's in VERSION
VERSION:=$(shell cat $(VERSIONFILE))

# the number of commits since the last tag
PATCHLEVEL:=$(shell git describe --long | awk -F - '{print $$2}')
# the number of commits since the last_release tag
#PATCHLEVEL:=$(shell git rev-list last_release..HEAD | wc -l)

# build flags
WARNINGS:=-W -fwarn-tabs -fno-warn-unused-do-bind -fno-warn-name-shadowing #-fwarn-orphans -fwarn-simple-patterns -fwarn-monomorphism-restriction

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
# BUILDING

# cabal install all hledger PACKAGES and dependencies in the proper order
# (or, as many as possible)
EXTRAINSTALLARGS=
install:
	cabal install $(patsubst %,./%,$(PACKAGES)) $(EXTRAINSTALLARGS)

# run a cabal command in all hledger package dirs
allcabal%:
	for p in $(PACKAGES); do (echo doing cabal $* in $$p; cd $$p; cabal $*; echo); done

# # run a command in all hledger package dirs
# all%:
# 	for p in $(PACKAGES); do (echo doing $* in $$p; cd $$p; $*); done

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
.PHONY: bin/hledgerdev bin/hledgerprof bin/hledgeropt bin/hledger-webdev

# build hledger binary as quickly as possible
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

# build the fastest binary we can
bin/hledgeropt:
	$(GHC) $(MAIN) -o $@ $(BUILDFLAGS) -O2 # -fvia-C # -fexcess-precision -optc-O3 -optc-ffast-math

# build the time profiling binary. cabal install --reinstall -p some libs may be required.
bin/hledgerprof:
	$(GHC) $(BUILDFLAGS) $(PROFBUILDFLAGS) $(MAIN) -o $@

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

# work around for inconsistently-named (why ?) hledger/hledger-cli.hs
linuxbinary-hledger:
	$(GHC) hledger/hledger-cli.hs -o bin/$*$(RELEASEBINARYSUFFIX) $(LINUXRELEASEBUILDFLAGS)

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

# One way to get a wine command prompt
wine:
	wineconsole cmd.exe &

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


######################################################################
# DOCUMENTATION


######################################################################
# RELEASING

# set up this repo copy for previewing a release:
# ensure download links work
set-up-rc-repo:
	cd site/_site; ln -s ../download

checkdeps packdeps:
	for p in $(PACKAGES); do packdeps $$p/$$p.cabal; done

######################################################################
# MISCELLANEOUS



######################################################################
# OLD PRE PKG SPLIT
######################################################################

######################################################################
# TESTING

SHELLTEST=shelltest

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
functest: bin/hledgerdev
	@echo functional tests:
	@($(SHELLTEST) tests -- --threads=16 --hide-successes \
		&& echo $@ PASSED) || echo $@ FAILED

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
	@(make --no-print-directory allcabalclean allcabalcheck allcabalconfigure \
		&& echo $@ PASSED) || echo $@ FAILED

# make sure cabal is happy in all possible ways
fullcabaltest:
	(for p in $(PACKAGES); do ( \
		printf "\ntesting $$p package\n" \
		&& cd $$p \
		&& cabal clean \
		&& cabal check \
		&& cabal install \
		&& cabal sdist \
		); done \
		&& echo $@ PASSED) || echo $@ FAILED
#		&& cabal upload dist/$$p-$(VERSION).tar.gz --check -v3 \

# run simple performance benchmarks without saving results
# Requires some commands defined in bench.tests and some BENCHEXES defined above.
quickbench: samplejournals bench.tests tools/simplebench
	tools/simplebench -v -fbench.tests $(BENCHEXES)
	@rm -f benchresults.*

# run simple performance benchmarks and archive results
# Requires some commands defined in bench.tests and some BENCHEXES defined above.
bench: samplejournals bench.tests tools/simplebench
	tools/simplebench -v -fbench.tests $(BENCHEXES) | tee profs/$(TIME).bench
	@rm -f benchresults.*
	@(cd profs; rm -f latest.bench; ln -s $(TIME).bench latest.bench)

# run criterion benchmark tests and save graphical results
criterionbench: samplejournals tools/criterionbench
	tools/criterionbench -t png -k png

# run progression benchmark tests and save graphical results
progressionbench: samplejournals tools/progressionbench
	tools/progressionbench -- -t png -k png

# generate and archive an execution profile
prof: samplejournals bin/hledgerprof
	@echo "Profiling: $(PROFCMD)"
	-$(PROFCMD) +RTS $(PROFRTSFLAGS) -RTS
	mv hledgerprof.prof profs/$(TIME).prof
	(cd profs; rm -f latest*.prof; ln -s $(TIME).prof latest.prof)

# generate, archive, simplify and display an execution profile
viewprof: prof
	tools/simplifyprof.hs profs/latest.prof

# generate and display an execution profile, don't save or simplify
quickprof: samplejournals bin/hledgerprof
	@echo "Profiling: $(PROFCMD)"
	-$(PROFCMD) +RTS $(PROFRTSFLAGS) -RTS
	echo; cat hledgerprof.prof

# generate and archive a graphical heap profile
heap: samplejournals bin/hledgerprof
	@echo "Profiling heap with: $(PROFCMD)"
	$(PROFCMD) +RTS -hc -RTS
	mv hledgerprof.hp profs/$(TIME).hp
	(cd profs; rm -f latest.hp; ln -s $(TIME).hp latest.hp; \
		hp2ps $(TIME).hp; rm -f latest.ps; ln -s $(TIME).ps latest.ps; rm -f *.aux)

viewheap: heap
	$(VIEWPS) profs/latest.ps

# generate and display a graphical heap profile, don't save
quickheap: samplejournals bin/hledgerprof
	@echo "Profiling heap with: $(PROFCMD)"
	$(PROFCMD) +RTS -hc -RTS
	hp2ps hledgerprof.hp
	$(VIEWPS) hledger.ps

# display a code coverage text report from running hledger COVCMD
quickcoverage: hledgerhpc
	@echo "Generating code coverage text report for hledger command: $(COVCMD)"
	tools/runhledgerhpc "report" $(COVCMD)

# generate a code coverage html report from running hledger COVCMD
coverage: samplejournals hledgerhpc
	@echo "Generating code coverage html report for hledger command: $(COVCMD)"
	tools/runhledgerhpc "markup --destdir=profs/coverage" $(COVCMD)
	cd profs/coverage; rm -f index.html; ln -s hpc_index.html index.html

# view the last html code coverage report
viewcoverage:
	$(VIEWHTML) profs/coverage/index.html

# get a debug prompt
ghci:
	ghci $(INCLUDEPATHS) $(MAIN)

ghciweb:
	ghci $(BUILDFLAGS) $(WEBLANGEXTS) hledger-web/app/main.hs

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

# Documentation source files are UPPERCASE files in the top directory.
# site/ contains both html generated from these (UPPERCASE.html) and
# revision-controlled resource files (everything else).  site/api
# contains only generated files.

cleandocs:
	rm -rf site/[A-Z]*.html site/api/*

# rebuild all docs
docs: site codedocs

# build the hledger.org website
# Requires hakyll (cabal install hakyll)
.PHONY: site
site: site/site
	cd site; ./site build

cleansite: site/site cleanolddocs
	cd site; ./site clean

previewsite: site/site
	cd site; ./site preview

site/site: site/site.hs olddocs
	cd site; $(GHC) site.hs $(PREFERMACUSRLIBFLAGS)

autosite:
	cd site; $(AUTOBUILD) site.hs -o site $(PREFERMACUSRLIBFLAGS) --run preview

viewsite: site
	$(VIEWHTML) site/_site/index.html

# ensure some old doc versions are in place:

olddocs: site/0.22 site/0.21 #site/0.20 site/0.19 site/0.18

#site/0.23:
#	(cd doc; git archive --prefix site/0.23/ tags/0.23 '*.md') | tar xf -

site/0.22:
	git archive --prefix site/0.22/ tags/0.22 '*.md' | tar xf -

site/0.21:
	git archive --prefix site/0.21/ tags/0.21.3 '*.md' | tar xf -

site/0.20:
	git archive --prefix site/0.20/ tags/0.20 '*.md' | tar xf -

site/0.19:
	git archive --prefix site/0.19/ tags/0_19_3 '*.md' | tar xf -

site/0.18:
	git archive --prefix site/0.18/ tags/0_18_2 '*.md' | tar xf -

cleanolddocs:
	cd site; rm -rf 0.22 0.21 0.20 0.19 0.18

# generate html versions of docs (and the hledger.org website)
# work around pandoc not handling full rst image directive
# html:
# 	for d in $(DOCFILES); do $(PANDOC) --toc -s -H site/header.html -A site/footer.html -r rst $$d >site/$$d.html; done
# 	cd site && ln -sf ../SCREENSHOTS && $(RST2HTML) SCREENSHOTS >SCREENSHOTS.html && rm -f SCREENSHOTS
# 	cd site; rm -f index.html; ln -s README.html index.html; rm -f profs; ln -s ../profs


pdf: docspdf codepdf

# generate pdf versions of main docs
# docspdf:
# 	-for d in $(DOCFILES); do (cd site && ln -sf ../$$d && pandoc $$d -w pdf && rm -f $$d); done

# format all code as a pdf for offline reading
ENSCRIPT=enscript -q --header='$$n|$$D{%+}|Page $$% of $$=' --highlight=haskell --line-numbers --font=Courier6 --color -o-
codepdf:
	$(ENSCRIPT) --pretty-print=makefile hledger.cabal >cabal.ps
	$(ENSCRIPT) --pretty-print=makefile Makefile >make.ps
	$(ENSCRIPT) --pretty-print=haskell $(SOURCEFILES) >haskell.ps
	cat cabal.ps make.ps haskell.ps | ps2pdf - >code.pdf

# view all docs and code as pdf
PDFS=site/{README,README2,MANUAL,NEWS,CONTRIBUTORS,SCREENSHOTS}.pdf code.pdf
viewall: pdf
	$(VIEWPDF) $(PDFS)

# print all docs and code for offline reading
printall: pdf
	$(PRINT) $(PDFS)

# push latest docs etc. and update the hledger.org site
pushdocs: push
	ssh simon@joyful.com 'make -C/repos/hledger docs'

# dump all executables' command line help into files for review
EXES=hledger hledger-vty hledger-web
savehelp:
	for e in $(EXES); do $$e --help >.HELP_$$e; done

# generate api & other code docs
codedocs: haddock hscolour coverage #sourcegraph #hoogle

#http://www.haskell.org/haddock/doc/html/invoking.html
#$(subst -D,--optghc=-D,$(DEFINEFLAGS))
HADDOCKFLAGS= --no-warnings --prologue .haddockprologue \
	--optghc='-optP-include' --optghc='-optPhledger/dist/build/autogen/cabal_macros.h'
	#--optghc='-hide-package monads-tf' 

.haddocksynopsis: hledger/hledger.cabal
	grep synopsis $< | sed -e 's/synopsis: *//' >$@

.haddockprologue: hledger/hledger.cabal
	cat $< | perl -ne 'print if (/^description:/../^$$/)' | sed -e 's/^description: *//' >$@
	printf "\nThis haddock covers all hledger-* packages, for individual package haddocks see hackage.\n" >>$@

# generate api docs for the whole project
haddock: .haddockprologue
	$(HADDOCK) $(HADDOCKFLAGS) --title "hledger-* API docs" \
	 -o site/api \
	 --html \
	 --source-module=src/%{MODULE/./-}.html \
	 --source-entity=src/%{MODULE/./-}.html#%N \
	 $(HADDOCKSOURCEFILES)

# browse the api docs
viewhaddock:
	$(VIEWHTML) site/api/frames.html

# http://www.cs.york.ac.uk/fp/darcs/hscolour/
HSCOLOUR=HsColour -css
hscolour: site/api/src site/api/src/hscolour.css
	for f in $(HADDOCKSOURCEFILES); do \
		$(HSCOLOUR) -anchor $$f -osite/api/src/`echo $$f | sed -e's%[^/]*/%%' | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
	done

site/api/src/hscolour.css: site/api/src
	$(HSCOLOUR) -print-css >site/api/src/hscolour.css

site/api/src:
	mkdir -p site/api/src

sourcegraph:
	for p in $(PACKAGES); do (cd $$p; SourceGraph $$p.cabal); done

patchdeps:
	darcs2dot > patchdeps.dot && dot -Tpng -O patchdeps.dot

# # generate external api docs for each package
# allhaddock: allcabalhaddock\ --hyperlink-source\ --executables

# # generate internal code docs for each package
# allhaddockinternal: allcabalhaddock\ --hyperlink-source\ --executables\ --internal

# # generate hoogle indices for each package
# allhoogle: allcabalhaddock\ --hoogle\ --executables

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

# Version numbering. See also VERSION and Version.hs.
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
# - The VERSION file must be updated manually before a release.
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
# First update VERSION. Eg:
# a normal release: echo 0.7   >VERSION; make release
# a bugfix release: echo 0.7.1 >VERSION; make release
release: releasetest setandrecordversion tagrelease

# Upload the latest cabal package and update hledger.org
upload: allcabalsdist hackageupload pushdocs

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
	perl -p -e "s/(^[ ,]*hledger-lib *[>=]=) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(-DVERSION=\")[^\"]+/\$${1}$(VERSION)/" -i $@

hledger-web/hledger-web.cabal: $(VERSIONFILE)
	perl -p -e "s/(^ *version:) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger *[>=]=) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(^[ ,]*hledger-lib *[>=]=) *.*/\1 $(VERSION)/" -i $@
	perl -p -e "s/(-DVERSION=\")[^\"]+/\$${1}$(VERSION)/" -i $@

doc/MANUAL.md: $(VERSIONFILE)
	perl -p -e "s/(^Version:) +[0-9.]+/\1 $(VERSION)/" -i $@

doc/DOWNLOAD.md: $(VERSIONFILE)
	perl -p -e "s/hledger(|-chart|-web|-vty)-[0-9.]+-/hledger\1-$(VERSION)-/g" -i $@

tagrelease:
	git tag $(VERSION)

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
	$(RSYNC) -azP profs/ simon@joyful.com:/repos/hledger/profs/

# fetch any new profiles and benchmark results from the public site
pullprofs:
	$(RSYNC) -azP simon@joyful.com:/repos/hledger/profs/ profs/

# compress the just-built platform binary. make hledgerPLATFORM first. Use
# the win variant on windows.
compressbinary:
	gzip -9 bin/$(BINARYFILENAME)
compressbinarywin:
	cd bin; zip -9 $(BINARYFILENAME).zip $(BINARYFILENAME)

# push the last-updated platform binary to the public download directory
pushlatestbinary:
	cd bin; $(RSYNC) -aP `ls -t | head -2` simon@joyful.com:/repos/hledger/site/download/


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

clean:
	rm -rf `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*" | grep -v .virthualenv`

cleanbin:
	rm -f bin/hledgerdev bin/hledgerdev.ghc*

Clean: clean cleanbin cleandocs
	rm -f TAGS tags

-include Makefile.local
