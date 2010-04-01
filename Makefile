# hledger project makefile

# optional features described in MANUAL, comment out if you don't have the libs
OPTFLAGS= #-DCHART -DVTY -DWEB
#OPTFLAGS=-DCHART -DVTY -DWEBHAPPSTACK

# command to run during "make ci"
CICMD=test
CICMD=web -v --debug -f ~/personal/2009.ledger

# command to run during "make prof/heap"
PROFCMD=-f 1000x1000x10.ledger balance

# command to run during "make coverage"
COVCMD=test

# executables to run during "make benchmark". They should be on the path
# or in the current directory. hledger executables should generally be the
# standard cabal builds, also constrained to parsec 2 for now.
BENCHEXES=hledger-0.6 hledger-0.7 ledger-3pre

# misc. tools
PANDOC=pandoc
RST2HTML=rst2html
RST2PDF=rst2pdf
#VIEWHTML=open
VIEWHTML=open -a 'Google Chrome'
VIEWPS=open
VIEWPDF=open
PRINT=lpr

MAIN=hledger.hs
SOURCEFILES:= \
	$(MAIN) \
	[A-Z]*hs \
	Commands/[A-Z]*hs \
	Ledger/[A-Z]*hs
DOCFILES:=README README2 MANUAL NEWS CONTRIBUTORS SCREENSHOTS
BINARYFILENAME=`runhaskell ./hledger.hs --binary-filename`
PATCHLEVEL:=$(shell expr `darcs changes --count --from-tag=\\\\\.` - 1)
WARNINGS:=-W -fwarn-tabs #-fwarn-orphans -fwarn-simple-patterns -fwarn-monomorphism-restriction -fwarn-name-shadowing
DEFINEFLAGS:=-DMAKE -DPATCHLEVEL=$(PATCHLEVEL) $(OPTFLAGS)
BUILDFLAGS:=$(DEFINEFLAGS) $(WARNINGS)
TIME:=$(shell date +"%Y%m%d%H%M")

default: tag hledger

######################################################################
# BUILDING

# build the standard cabal binary
hledgercabal:
	cabal configure -fweb -fvty && cabal build

# build the standard developer's binary, quickly
hledger: setversion
	ghc --make hledger.hs -o hledger $(BUILDFLAGS) # -O

hledgernowarnings: setversion
	ghc --make hledger.hs -o hledger $(BUILDFLAGS) -Werror -v0

# build the profiling-enabled binary. You may need to cabal install
# --reinstall -p some libs.
hledgerp: setversion
	ghc --make hledger.hs -prof -auto-all -o hledgerp $(BUILDFLAGS) 

# build the coverage-enabled binary. coverage-enabled .o files are kept
# separate to avoid contamination.
hledgercov: setversion
	ghc --make hledger.hs -fhpc -o hledgercov -outputdir .coverageobjs $(BUILDFLAGS) 

# build the fastest binary we can
hledgeropt: setversion
	ghc --make hledger.hs -o hledgeropt $(BUILDFLAGS) -O2 # -fvia-C # -fexcess-precision -optc-O3 -optc-ffast-math

# build a deployable binary for mac, one which uses only standard osx libs
# use some trickery to link without gmp lib
hledgermac: setversion
	ghc -c --make hledger.hs -o $(BINARYFILENAME) $(BUILDFLAGS) -O2 -optl-L/usr/lib
	sudo port deactivate gmp
	-PATH=tools:$(PATH) ghc --make hledger.hs -o $(BINARYFILENAME) $(BUILDFLAGS) -O2 -optl-L/usr/lib
	sudo port activate gmp
	@echo Please check the build looks portable:
	otool -L $(BINARYFILENAME)

# build a deployable binary for gnu/linux, statically linked
hledgerlinux: setversion
	ghc --make hledger.hs -o $(BINARYFILENAME) $(BUILDFLAGS) -O2 -static -optl-static -optl-pthread
	@echo Please check the build looks portable:
	-ldd $(BINARYFILENAME)

# build a deployable binary for windows, using cygwin presumably
# hledgerwin: setversion
# 	ghc --make hledger.hs -o hledgerlinux $(BUILDFLAGS) -O2 -static -optl-static -optl-pthread

# "continuous integration" testing - auto-recompile and run hledger test
# (or some other command) whenever a module changes. sp is from
# searchpath.org , you might need the patched version from
# http://joyful.com/repos/searchpath .
continuous ci: setversion
	sp --no-exts --no-default-map -o hledger ghc --make hledger.hs $(BUILDFLAGS) --run $(CICMD)

# fix permissions (eg after darcs get)
fixperms:
	chmod +x hledger.hs tools/*

# build the standalone unit test runner. Requires test-framework, which
# may not work on windows.
tools/unittest: tools/unittest.hs
	ghc --make -threaded -O2 tools/unittest.hs

# build the doctest runner
tools/doctest: tools/doctest.hs
	ghc --make tools/doctest.hs

# build the benchmark runner. Requires tabular.
tools/bench: tools/bench.hs
	ghc --make tools/bench.hs

# build the generateledger tool
tools/generateledger: tools/generateledger.hs
	ghc --make tools/generateledger.hs

######################################################################
# TESTING

test: codetest

# quick code tests - run all the time
codetest: unittest functest

# moderate pre-commit tests - run before record or before send/push, your choice
committest: hlinttest unittest doctest functest haddocktest warningstest quickcabaltest

# thorough pre-release tests - run before release
releasetest: unittest doctest functest haddocktest warningstest fullcabaltest

hlinttest hlint:
	hlint --hint=hlint --report=hlint.html $(SOURCEFILES)

# run unit tests
unittest: unittest-builtin

unittest-builtin: hledger
	@(./hledger test -v \
		&& echo $@ passed) || echo $@ FAILED

# XXX doesn't rebuild on hledger source changes
unittest-standalone: tools/unittest
	@(tools/unittest \
		&& echo $@ passed) || echo $@ FAILED

# run unit tests without waiting for compilation
unittesths:
	@(runghc hledger.hs test \
		&& echo $@ passed) || echo $@ FAILED

# run functional tests, requires shelltestrunner from hackage
functest: hledger
	(shelltest tests --implicit=none --threads=16 \
		&& echo $@ passed) || echo $@ FAILED

# run doc tests
doctest: tools/doctest hledger
	@(tools/doctest Commands/Add.hs \
		&& tools/doctest Tests.hs \
		&& echo $@ passed) || echo $@ FAILED

# make sure we have no haddock errors
haddocktest:
	@(make --quiet haddock \
		&& echo $@ passed) || echo $@ FAILED

# make sure the normal build has no warnings
warningstest:
	@(make -s clean \
		&& make --no-print-directory -s hledgernowarnings \
		&& echo $@ passed) || echo $@ FAILED

# make sure cabal is reasonably happy
quickcabaltest: setversion
	@(cabal clean \
		&& cabal check \
		&& cabal configure -fvty -fweb \
		&& echo $@ passed) || echo $@ FAILED

# make sure cabal is happy in all possible ways
fullcabaltest: setversion
	@(cabal clean \
		&& cabal check \
		&& cabal configure -fvty -fweb \
		&& cabal build \
		&& dist/build/hledger/hledger test 2>&1 | tail -1 | grep -q 'Errors: 0  Failures: 0' \
		&& cabal sdist \
		&& cabal upload dist/hledger-$(VERSION).tar.gz --check -v3 \
		&& echo $@ passed) || echo $@ FAILED

# run performance benchmarks and save results in profs/. 
# Requires some commands defined in bench.tests and some BENCHEXES defined above.
benchmark: sampleledgers bench.tests tools/bench
	PATH=.:$(PATH) tools/bench -fbench.tests $(BENCHEXES) | tee profs/$(TIME).bench
	@rm -f benchresults.*
	@(cd profs; rm -f latest.bench; ln -s $(TIME).bench latest.bench)

# generate, save and display a standard profile
prof: sampleledgers hledgerp
	@echo "Profiling $(PROFCMD)"
	./hledgerp +RTS -p -RTS $(PROFCMD) >/dev/null
	mv hledgerp.prof profs/$(TIME)-orig.prof
	tools/simplifyprof.hs profs/$(TIME)-orig.prof >profs/$(TIME).prof
	(cd profs; rm -f latest*.prof; ln -s $(TIME)-orig.prof latest-orig.prof; ln -s $(TIME).prof latest.prof)
	echo; cat profs/latest.prof

# generate, save and display a graphical heap profile
heap: sampleledgers hledgerp
	@echo "Profiling heap with $(PROFCMD)"
	./hledgerp +RTS -hc -RTS $(PROFCMD) >/dev/null
	mv hledgerp.hp profs/$(TIME).hp
	(cd profs; rm -f latest.hp; ln -s $(TIME).hp latest.hp; \
		hp2ps $(TIME).hp; rm -f latest.ps; ln -s $(TIME).ps latest.ps; rm -f *.aux)
	$(VIEWPSCMD) profs/latest.ps

# generate and display a code coverage report
coverage: sampleledgers hledgercov
	@echo "Generating coverage report with $(COVCMD)"
	tools/coverage "markup --destdir=profs/coverage" test
	cd profs/coverage; rm -f index.html; ln -s hpc_index.html index.html
	$(VIEWHTMLCMD) profs/coverage/index.html

# get a debug prompt
ghci:
	ghci -DMAKE $(OPTFLAGS) hledger.hs

# generate standard sample ledgers
sampleledgers: sample.ledger 100x100x10.ledger 1000x1000x10.ledger 10000x1000x10.ledger 100000x1000x10.ledger

sample.ledger:
	true # XXX should probably regenerate this

100x100x10.ledger: tools/generateledger
	tools/generateledger 100 100 10 >$@

1000x1000x10.ledger: tools/generateledger
	tools/generateledger 1000 1000 10 >$@

10000x1000x10.ledger: tools/generateledger
	tools/generateledger 10000 1000 10 >$@

100000x1000x10.ledger: tools/generateledger
	tools/generateledger 100000 1000 10 >$@

######################################################################
# DOCUMENTATION

# Documentation source files are UPPERCASE files in the top directory.
# website/ contains both html generated from these (UPPERCASE.html) and
# revision-controlled resource files (everything else).  website/api-doc
# contains only generated files.

cleandocs:
	rm -rf website/[A-Z]*.html website/api-doc/*

# rebuild all docs
docs: html pdf apidocs

# generate html versions of docs (and the hledger.org website)
# work around pandoc not handling full rst image directive
html:
	for d in $(DOCFILES); do $(PANDOC) --toc -s -H website/header.html -A website/footer.html -r rst $$d >website/$$d.html; done
	cd website && ln -sf ../SCREENSHOTS && $(RST2HTML) SCREENSHOTS >SCREENSHOTS.html && rm -f SCREENSHOTS
	cd website; rm -f index.html; ln -s README.html index.html; rm -f profs; ln -s ../profs

pdf: docspdf codepdf

# generate pdf versions of main docs
# work around rst2pdf needing images in the same directory
docspdf:
	-for d in $(DOCFILES); do (cd website && ln -sf ../$$d && $(RST2PDF) $$d && rm -f $$d); done

# format all code as a pdf for offline reading
ENSCRIPT=enscript -q --header='$$n|$$D{%+}|Page $$% of $$=' --line-numbers --font=Courier6 --color -o-
codepdf:
	$(ENSCRIPT) --pretty-print=makefile hledger.cabal >cabal.ps
	$(ENSCRIPT) --pretty-print=makefile Makefile >make.ps
	$(ENSCRIPT) --pretty-print=haskell $(SOURCEFILES) >haskell.ps
	cat cabal.ps make.ps haskell.ps | ps2pdf - >code.pdf

# view all docs and code as pdf
PDFS=website/{README,README2,MANUAL,NEWS,CONTRIBUTORS,SCREENSHOTS}.pdf code.pdf
viewall: pdf
	$(VIEWPDFCMD) $(PDFS)

# print all docs and code for offline reading
printall: pdf
	$(PRINTCMD) $(PDFS)

# push latest docs etc. and update the hledger.org site
site: push
	ssh joyful.com 'make -C/repos/hledger docs'

# generate api docs
# We munge haddock and hoogle into a rough but useful framed layout.
# For this to work the hoogle cgi must be built with base target "main".
# XXX move the framed index building into haddock: ?
apidocs: haddock hscolour #sourcegraph #hoogle
	sed -i -e 's%^></HEAD%><base target="main"></HEAD%' website/api-doc/modules-index.html ; \
	cp website/api-doc-frames.html website/api-doc/index.html ; \
# 	cp website/hoogle-small.html website/api-doc

# generate and view the api docs
viewapidocs: apidocs
	$(VIEWHTMLCMD) website/api-doc/index.html

# generate code documentation with haddock
# --ignore-all-exports means we are documenting internal implementation, not library api
HADDOCK=haddock -B `ghc --print-libdir` $(subst -D,--optghc=-D,$(DEFINEFLAGS)) --ignore-all-exports --no-warnings
haddock:
	$(HADDOCK) -o website/api-doc -h --source-module=src-%{MODULE/./-}.html --source-entity=src-%{MODULE/./-}.html#%N $(MAIN) && \
		cp website/api-doc/index.html website/api-doc/modules-index.html
	cd hledger-lib; cabal haddock

HSCOLOUR=HsColour -css 
hscolour:
	for f in $(SOURCEFILES); do \
		$(HSCOLOUR) -anchor $$f -owebsite/api-doc/`echo "src/"$$f | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
	done ; \
	cp website/api-doc/src-hledger.html website/api-doc/src-Main.html ; \
	HsColour -print-css >website/api-doc/hscolour.css

sourcegraph:
	-SourceGraph hledger.cabal
	-cd hledger-lib; SourceGraph hledger-lib.cabal

#set up the hoogle web interface
#uses a hoogle source tree configured with --datadir=., patched to fix haddock urls/target frame
HOOGLESRC=/usr/local/src/hoogle
HOOGLE=$(HOOGLESRC)/dist/build/hoogle/hoogle
HOOGLEVER=`$(HOOGLE) --version |tail -n 1 | sed -e 's/Version /hoogle-/'`
hoogle: hoogleindex
	if test -f $(HOOGLE) ; then \
		cd website/api-doc && \
		rm -f $(HOOGLEVER) && \
		ln -s . $(HOOGLEVER) && \
		cp -r $(HOOGLESRC)/src/res/ . && \
		cp -p $(HOOGLE) index.cgi && \
		touch log.txt && chmod 666 log.txt ; \
	else \
		echo "Could not find $(HOOGLE) in the hoogle source tree" ; \
	fi

#generate a hoogle index
hoogleindex:
	$(HADDOCK) -o website/api-doc --hoogle $(MAIN) && \
	cd website/api-doc && \
	hoogle --convert=main.txt --output=default.hoo

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
# - "make benchmark" depends on version numbers in BENCHEXES, these also
#   must be updated manually.
#
# - "make" updates the version in most other places, and defines PATCHES.
#   Note "cabal build" should also do this but doesn't yet.
#
# - "make release" additionally records the main version number-affected
#   files, and tags the repo with the release tag.

# Build a release, tag the repo, prepare a cabal package
# Don't forget to update VERSION first. Examples:
# normal release:   echo 0.7   >VERSION; make release
# a bugfix release: echo 0.7.1 >VERSION; make release
release: releasetest setandrecordversion tagrelease sdist

# Upload the latest cabal package and update hledger.org
upload: hackageupload updatesite

releaseandupload: release upload

# file where the current release version is defined
VERSIONFILE=VERSION

# two or three-part version string
VERSION:=`grep -v '^--' $(VERSIONFILE)`

# three-part version string
ifeq ($(shell ghc -e "length (filter (=='.') \"$(VERSION)\")"), 1)
VERSION3:=$(VERSION).0
else
VERSION3:=$(VERSION)
endif

# other files containing the version string
VERSIONFILES=hledger.cabal Version.hs

hledger.cabal: $(VERSIONFILE)
	perl -p -e "s/(^version: *) .*/\1 $(VERSION)/" -i $@

Version.hs: $(VERSIONFILE)
	perl -p -e "s/(^version *= *)\".*?\"/\1\"$(VERSION3)\"/" -i $@

# update the version string in local files. Triggered by "make".
setversion: $(VERSIONFILES)

# update the version number in local files, and prompt to record changes
# in these files. Triggered by "make release".
setandrecordversion: setversion
	darcs record -m "update version" $(VERSIONFILE) $(VERSIONFILES)

tagrelease:
	darcs tag $(VERSION3)

sdist:
	cabal sdist

# display a hackage upload command reminder
hackageupload:
	cabal upload dist/hledger-$(VERSION).tar.gz -v3

# send unpushed patches to the mail list
send:
	darcs send http://joyful.com/repos/hledger --to=hledger@googlegroups.com --edit-description  

# push patches and anything else pending to the public server
push: pushprofs pushbinary
	darcs push joyful.com:/repos/hledger

# pull anything pending from the public server
pull: pullprofs
	darcs pull -a joyful.com:/repos/hledger

# push any new profiles and benchmark results to the public site
# beware, results will vary depending on which machine generated them
pushprofs:
	rsync -azP profs/ joyful.com:/repos/hledger/profs/

# fetch any new profiles and benchmark results from the public site
pullprofs:
	rsync -azP joyful.com:/repos/hledger/profs/ profs/

# push a deployable binary for this platform to the public site
# make hledgerPLATFORM first
pushbinary:
	-gzip -9 $(BINARYFILENAME)
	-rsync -aP $(BINARYFILENAME).gz joyful.com:/repos/hledger/website/binaries/

# show project stats useful for release notes
stats: showlastreleasedate showreleaseauthors showloc showcov showerrors showlocalchanges showreleasechanges benchmark

showreleaseauthors:
	@echo Patch authors since last release:
	@darcs changes --from-tag . |grep '^\w' |cut -c 31- |sort |uniq
	@echo

showloc:
	@echo Lines of non-test code:
	@sloccount `ls $(SOURCEFILES) |grep -v Tests.hs` | grep haskell:
	@echo Lines of test code:
	@sloccount Tests.hs | grep haskell:
	@echo

showcov:
	@echo Test coverage:
	@tools/coverage report test

showlastreleasedate:
	@echo Last release date:
	@darcs changes --from-tag . | tail -2
	@echo

showerrors:
	@echo Known errors:
	@awk '/^** errors/, /^** / && !/^** errors/' NOTES | grep '^\*\*\* ' | tail +1
	@echo

showlocalchanges:
	@echo Local changes:
	@-darcs push joyful.com:/repos/hledger --dry-run | grep '*' | tac
	@echo

showcodechanges:
	@echo Code changes:
	@darcs changes --matches "not (name docs: or name site: or name tools:)" | egrep '^ +(\*|tagged)'
	@echo

showreleasechanges:
	@echo "Code changes since last release: ("`darcs changes --from-tag . --count`")"
	@darcs changes --from-tag . --matches "not (name docs: or name site: or name tools:)" | grep '*'
	@echo

######################################################################
# MISCELLANEOUS

tag: emacstags

emacstags:
	@rm -f TAGS; hasktags -e $(SOURCEFILES) hledger.cabal

clean:
	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*" -o -name "*-darcs-backup*"`

Clean: clean cleandocs
	rm -f hledger TAGS tags

