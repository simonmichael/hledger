# hledger project makefile

# optional features described in README, comment out if you don't have the libs
OPTFLAGS=-DHAPPS -DVTY 

# command to run during "make ci"
CICMD=test
CICMD=web --debug -BE

# command to run during "make prof/heap"
PROFCMD=-f 1000x1000x10.ledger balance

# command to run during "make coverage"
COVCMD=test

# executables to run during "make benchmark" (prepend ./ if not in $PATH)
BENCHEXES=hledger-0.5 hledger-0.6 ledger

# document viewing commands
VIEWHTMLCMD=open
VIEWPSCMD=open

PLATFORMBINARIES=hledgermac hledgerlinux #hledgerwin
BINARYFILENAME=`runhaskell ./hledger.hs --binary-filename`
SOURCEFILES:=*hs Commands/*hs Ledger/*hs
DOCFILES:=HOME README NEWS CONTRIBUTORS SCREENSHOTS
PATCHLEVEL:=$(shell expr `darcs changes --count --from-tag=\\\\\.` - 1)
BUILDFLAGS:=-DPATCHLEVEL=$(PATCHLEVEL) $(OPTFLAGS)
TIME:=$(shell date +"%Y%m%d%H%M")

default: tag hledger

######################################################################
# BUILDING

# build the standard developer's binary, quickly
hledger: setversion
	ghc --make hledger.hs -o hledger $(BUILDFLAGS) # -O

hledgernowarnings: setversion
	ghc --make hledger.hs -o hledger $(BUILDFLAGS) -Werror -v0

# build the profiling-enabled binary. You may need to cabal install
# --reinstall -p some libs.
hledgerp: setversion
	ghc --make hledger.hs -prof -auto-all -o hledgerp $(BUILDFLAGS) 

# build the coverage-enabled binary. Warning, might need make clean
hledgercov: setversion
	ghc --make hledger.hs -fhpc -o hledgercov $(BUILDFLAGS) 

# build the fastest binary we can
hledgeropt: setversion
	ghc --make hledger.hs -o hledgeropt $(BUILDFLAGS) -O2 # -fvia-C # -fexcess-precision -optc-O3 -optc-ffast-math

# build a deployable binary for mac, one which uses only standard osx libs
hledgermac: setversion
	sudo port deactivate gmp
	ghc --make hledger.hs -o $(BINARYFILENAME) $(BUILDFLAGS) -O2 -optl-L/usr/lib #-optl-F/Library/Frameworks/GMP
	sudo port activate gmp
	@echo Please check the build depends only on standard system libraries:
	otool -L $(BINARYFILENAME)

# build a deployable binary for gnu/linux, statically linked
hledgerlinux: setversion
	ghc --make hledger.hs -o hledgerlinux $(BUILDFLAGS) -O2 -static -optl-static -optl-pthread

# build a deployable binary for windows, using cygwin presumably
# hledgerwin: setversion
# 	ghc --make hledger.hs -o hledgerlinux $(BUILDFLAGS) -O2 -static -optl-static -optl-pthread

# "continuous integration" testing - auto-recompile and run hledger test
# (or some other command) whenever a module changes. sp is from
# searchpath.org , you might need the patched version from
# http://joyful.com/repos/searchpath .
continuous ci: setversion
	sp --no-exts --no-default-map -o hledger ghc --make hledger.hs $(BUILDFLAGS) --run $(CICMD)

# build the benchmark runner. Requires tabular from hackage.
tools/bench: tools/bench.hs
	ghc --make tools/bench.hs

# build the doctest runner
tools/doctest: tools/doctest.hs
	ghc --make tools/doctest.hs

# build the generateledger tool
tools/generateledger: tools/generateledger.hs
	ghc --make tools/generateledger.hs

######################################################################
# TESTING

# quick code tests - run all the time
test: unittest doctest haddocktest

# moderate pre-commit tests
# run before recording or before send/pushing, your choice
committest: unittest doctest haddocktest warningstest
	@(cabal configure -fvty -fhapps \
		&& echo $@ passed) || echo $@ FAILED

# thorough, pre-release tests - run before release
releasetest: unittest doctest haddocktest warningstest cabaltest
	@dist/build/hledger/hledger test 2>&1 | tail -1 | grep -q 'Errors: 0  Failures: 0'

# run unit tests, without waiting for compilation
unittest:
	@(runghc hledger.hs test \
		&& echo $@ passed) || echo $@ FAILED

# run doc tests
doctest: tools/doctest
	@(tools/doctest Commands/Add.hs >/dev/null \
		&& tools/doctest Tests.hs >/dev/null \
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

# make sure cabal is happy in all ways
cabaltest: setversion
	@(cabal clean \
		&& cabal check \
		&& cabal configure -fvty -fhapps \
		&& cabal build \
		&& echo $@ passed) || echo $@ FAILED

# run performance tests and save results in profs/. 
# Requires some commands defined in bench.tests and some executables defined above.
benchmark: sampleledgers bench.tests tools/bench
	tools/bench -fbench.tests $(BENCHEXES) | tee profs/$(TIME).bench
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
		hp2ps $(TIME).hp; rm -f latest.ps; ln -s $(TIME).ps latest.ps)
	$(VIEWPSCMD) profs/latest.ps

# generate and display a code coverage report
coverage: sampleledgers hledgercov
	@echo "Generating coverage report with $(COVCMD)"
	tools/coverage "markup --destdir=profs/coverage" test
	cd profs/coverage; rm -f index.html; ln -s hpc_index.html index.html
	$(VIEWHTMLCMD) profs/coverage/index.html

# get a debug prompt
ghci:
	ghci hledger.hs

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

# website/ and website/api-doc/ always exist. website contains both
# generated files (UPPERCASE.html) and revision-controlled resource files
# (everything else).  website/api-doc contains only generated files.
cleandocs:
	rm -rf website/[A-Z]*.html website/api-doc/*

# rebuild all docs
docs: web pdf api-docs

# build the main hledger.org website
web:
	for d in $(DOCFILES); do pandoc -s -H website/header.html -A website/footer.html -r rst $$d >website/$$d.html; done
	cd website; rm -f index.html; ln -s HOME.html index.html; rm -f profs; ln -s ../profs

# generate pdf versions of main docs
pdf:
	-for d in $(DOCFILES); do rst2pdf $$d -o website/$$d.pdf; done

# generate api docs
# We munge haddock and hoogle into a rough but useful framed layout.
# For this to work the hoogle cgi must be built with base target "main".
api-docs: haddock hoogle
	sed -i -e 's%^></HEAD%><base target="main"></HEAD%' website/api-doc/modules-index.html ; \
	cp website/api-doc-frames.html website/api-doc/index.html ; \
	cp website/hoogle-small.html website/api-doc

# generate and view the api docs
view-api-docs: api-docs
	$(VIEWHTMLCMD) website/api-doc/index.html

MAIN=hledger.hs

# --ignore-all-exports means we are documenting internal implementation, not library api
HADDOCK=haddock -B `ghc --print-libdir` --no-warnings --ignore-all-exports $(subst -D,--optghc=-D,$(BUILDFLAGS))
haddock: hscolour $(MAIN)
	$(HADDOCK) -o website/api-doc -h --source-module=src-%{MODULE/./-}.html --source-entity=src-%{MODULE/./-}.html#%N $(filter-out %api-doc-dir hscolour,$^) && \
		cp website/api-doc/index.html website/api-doc/modules-index.html

HSCOLOUR=HsColour -css 
hscolour:
	for f in $(SOURCEFILES); do \
		$(HSCOLOUR) -anchor $$f -owebsite/api-doc/`echo "src/"$$f | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
	done ; \
	cp website/api-doc/src-hledger.html website/api-doc/src-Main.html ; \
	HsColour -print-css >website/api-doc/hscolour.css

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
hoogleindex: $(MAIN)
	$(HADDOCK) -o website/api-doc --hoogle $^ && \
	cd website/api-doc && \
	hoogle --convert=main.txt --output=default.hoo

######################################################################
# RELEASING

# Places where hledger's version number makes an appearance:
#  hledger --version
#  hledger's cabal file
#  darcs tags
#  hackage tarball filenames
#  hackage pages
#
# Goals and constraints for our version number system:
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
# Current plan:
# - The release version looks like major.minor[.bugfix].  bugfix is 0 (and
#   may be elided) for a normal release, or 1..n for a bugfix release, or
#   98 meaning an alpha for the forthcoming release, or 99 meaning a beta.
# - The build version looks like major.minor.bugfix.patches, where patches
#   is the number of patches applied since the last release tag.
# - Set the release version in VERSION before "make" or "make release".
# - "make" updates version strings where needed, and defines PATCHES.
#   "make release" also records the version number changes and tags the
#   repo. (Todo: make cabal build set the version and PATCHES, also)
# - hledger --version shows the build version
# - The cabal package uses the release version
# - The release tag is the non-elided release version.

# Build a cabal release, tag the repo and maybe upload to hackage.
# Don't forget to update VERSION if needed. Examples:
# releasing 0.5:          set VERSION to 0.5, make release hackageupload
# doing a bugfix release: set VERSION to 0.5.1, make release hackageupload
# building 0.6 alpha:     set VERSION to 0.5.98, make
# releasing 0.6 beta:     set VERSION to 0.5.99, make release
release: releasetest setandrecordversion tagrelease sdist

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

# update the version string in local files, and record them (and
# $VERSIONFILE) if changed.  Be careful, will record all changes in those
# files (so prompts interactively). Triggered by "make release".
setandrecordversion: setversion
	darcs record -m "bump version" $(VERSIONFILE) $(VERSIONFILES)

tagrelease:
	darcs tag $(VERSION3)

sdist:
	cabal sdist

# display a hackage upload command reminder
hackageupload:
	@echo please do: cabal upload dist/hledger-$(VERSION).tar.gz -v3

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
# make hledgerPLAT first
pushbinary:
	-gzip -9 $(BINARYFILENAME)
	rsync -aP $(BINARYFILENAME).gz joyful.com:/repos/hledger/website/binaries/

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

showreleasechanges:
	@echo "Changes since last release: ("`darcs changes --from-tag . --count`")"
	@darcs changes --from-tag . | grep '*'
	@echo

######################################################################
# MISCELLANEOUS

tag: emacstags

emacstags:
	@rm -f TAGS; hasktags -e $(SOURCEFILES) hledger.cabal

clean:
	rm -f `find . -name "*.o" -o -name "*.hi" -o -name "*~" -o -name "darcs-amend-record*"`

Clean: clean cleandocs
	rm -f hledger TAGS tags

