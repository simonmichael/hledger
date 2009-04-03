# hledger project makefile

default: tag build

# patches since last release tag (as a haskell string literal)
PATCHES:=$(shell expr `darcs changes --count --from-tag=\\\\\.` - 1)

# build the normal hledger binary
BUILD=ghc --make hledger.hs -o hledger -O
FLAGS=-DPATCHES=$(PATCHES)
# optional extras described in README, turn em on if you've got the libs
OPTFLAGS= #-DVTY -DHAPPS
BUILDFLAGS=$(FLAGS) $(OPTFLAGS)
build: setversion
	@$(BUILD) $(BUILDFLAGS)

# build the fastest binary we can, as hledgeropt
BUILDOPT=ghc --make hledger.hs -o hledgeropt -O2 -fvia-C
buildopt opt: setversion
	$(BUILDOPT) $(BUILDFLAGS)

# "continuous integration" testing - recompile and run test (or any other
# command) whenever a module changes. sp is from searchpath.org , you
# might need the patched version from http://joyful.com/repos/searchpath
CICMD=test
continuous ci: setversion
	sp --no-exts --no-default-map -o hledger ghc --make hledger.hs $(BUILDFLAGS) --run $(CICMD)

# force a full rebuild with normal optimisation
rebuild: clean build

# debug prompt
ghci:
	ghci hledger.hs

# run all tests
test: doctest unittest haddocktest

# make sure we have no haddock errors
haddocktest:
	@make --quiet haddock

# run unit tests, without waiting for compilation
unittest:
	runghc hledger.hs test

# run doc tests
doctest: tools/doctest
	tools/doctest Tests.hs

tools/doctest: tools/doctest.hs
	ghc --make tools/doctest.hs

# build profiling-enabled hledgerp and archive and show a cleaned-up profile
# you may need to rebuild some libs: sudo cabal install --reinstall -p ...
PROFBIN=hledgerp
BUILDPROF=ghc $(BUILDFLAGS) --make hledger.hs -prof -auto-all -o $(PROFBIN)
RUNPROF=./$(PROFBIN) +RTS -p -RTS
PROFCMD=-f 1000.ledger -s balance
TIME=`date +"%Y%m%d%H%M"`
buildprof prof: sampleledgers
	@echo "Profiling $(PROFCMD)"
	$(BUILDPROF)
	$(RUNPROF) $(PROFCMD) #>/dev/null
	tools/simplifyprof.hs $(PROFBIN).prof >profile.prof
	cp profile.prof profs/$(TIME).prof
	echo; cat profile.prof

# run performance benchmarks and save results in profs
# executables to test, prepend ./ to these if not in $PATH
# requires tabular from hackage
BENCHEXES=hledger-0.4 hledger-0.5 ledger
bench: buildbench sampleledgers
	./bench $(BENCHEXES) --verbose | tee profs/`date +%Y%m%d%H%M%S`.bench

# build the benchmarking tool
buildbench:
	ghc --make tools/bench.hs
	rm -f bench; ln -s tools/bench

# generate sample ledgers
sampleledgers:
	ghc -e 'putStr $$ unlines $$ replicate 1000 "!include sample.ledger"' >1000.ledger
	ghc -e 'putStr $$ unlines $$ replicate 10000 "!include sample.ledger"' >10000.ledger
	ghc -e 'putStr $$ unlines $$ replicate 100000 "!include sample.ledger"' >100000.ledger
  # XXX should also generate sample.ledger with write_sample_ledger

# send unpushed patches to the mail list
send:
	darcs send http://joyful.com/repos/hledger --to=hledger@googlegroups.com --edit-description  

# push patches to the main repo with ssh
push:
	darcs push joyful.com:/repos/hledger

# version numbering, releasing etc.
#
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

# run pre-release checks: cabal is happy, the code builds, tests pass..
check: setversion test
	cabal clean
	cabal check
	cabal configure -fvty -fhapps
	cabal build
	dist/build/hledger/hledger test 2>&1 | tail -1 | grep -q 'Errors: 0  Failures: 0'

# Build a cabal release, tag the repo and maybe upload to hackage.
# Don't forget to update VERSION if needed. Examples:
# releasing 0.5:          set VERSION to 0.5, make release hackageupload
# doing a bugfix release: set VERSION to 0.5.1, make release hackageupload
# building 0.6 alpha:     set VERSION to 0.5.98, make
# releasing 0.6 beta:     set VERSION to 0.5.99, make release
release: check setandrecordversion tagrelease sdist

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
	perl -p -e "s/(^Version: *) .*/\1 $(VERSION)/" -i $@

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

hackageupload:
	cabal upload dist/hledger-$(VERSION).tar.gz


# update emacs TAGS file
tag:
	@rm -f TAGS; hasktags -e *hs Ledger/*hs

clean:
	rm -f {,Ledger/}*{.o,.hi,~} darcs-amend-record*

Clean: clean clean-docs
	rm -f hledger TAGS tags

# docs

DOCS=README NEWS

# rebuild all docs
docs: html pdf api-docs

# rebuild html docs
html:
	for d in $(DOCS); do rst2html $$d >doc/$$d.html; done
	cd doc; ln -f -s README.html index.html

# rebuild pdf docs
pdf:
	for d in $(DOCS); do rst2pdf $$d -o doc/$$d.pdf; done

# rebuild api docs
# We munge haddock and hoogle into a rough but useful framed layout.
# For this to work the hoogle cgi must be built with base target "main".
api-docs: haddock hoogleweb
	echo "Converting api docs to frames" ; \
	sed -i -e 's%^></HEAD%><base target="main"></HEAD%' api-doc/modules-index.html ; \
	cp doc/misc/api-doc-frames.html api-doc/index.html ; \
	cp doc/misc/hoogle-small.html hoogle

# build and preview the api docs
BROWSER=open
view-api-docs: api-docs
	$(BROWSER) api-doc/index.html

api-doc-dir:
	mkdir -p api-doc

MAIN=hledger.hs

# --ignore-all-exports here means these are actually implementation docs
HADDOCK=haddock -B `ghc --print-libdir` --no-warnings --ignore-all-exports $(subst -D,--optghc=-D,$(BUILDFLAGS))
haddock: api-doc-dir hscolour $(MAIN)
	echo "Generating haddock api docs with source" ; \
	$(HADDOCK) -o api-doc -h --source-module=src-%{MODULE/./-}.html --source-entity=src-%{MODULE/./-}.html#%N $(filter-out %api-doc-dir hscolour,$^) && \
		cp api-doc/index.html api-doc/modules-index.html

HSCOLOUR=HsColour -css 
hscolour: api-doc-dir
	echo "Generating colourised source" ; \
	for f in *hs Ledger/*hs; do \
		$(HSCOLOUR) -anchor $$f -oapi-doc/`echo "src/"$$f | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
	done ; \
	cp api-doc/src-hledger.html api-doc/src-Main.html ; \
	HsColour -print-css >api-doc/hscolour.css

#set up the hoogle web interface
#uses a hoogle source tree configured with --datadir=., patched to fix haddock urls/target frame
HOOGLESRC=/usr/local/src/hoogle
HOOGLE=$(HOOGLESRC)/dist/build/hoogle/hoogle
HOOGLEVER=`$(HOOGLE) --version |tail -n 1 | sed -e 's/Version /hoogle-/'`
hoogleweb: hoogleindex
	echo "Configuring hoogle web interface" ; \
	if test -f $(HOOGLE) ; then \
		mkdir -p hoogle && \
		cd hoogle && \
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
	echo "Generating hoogle index" ; \
	mkdir -p hoogle && \
	$(HADDOCK) -o hoogle --hoogle $^ && \
	cd hoogle && \
	hoogle --convert=main.txt --output=default.hoo

clean-docs:
	rm -rf api-doc hoogle

# misc

show-changes:
	@echo Changes since last release:
	@echo
	@darcs changes --from-tag . | grep '*'

show-unpushed:
	@echo Changes not yet in the main hledger repo:
	@echo
	@darcs push joyful.com:/repos/hledger --dry-run

show-authors:
	@echo Patch authors since last release:
	@echo
	@darcs changes --from-tag . |grep '^\w' |cut -c 31- |sort |uniq

# count lines of code
sloc:
	@echo "test code:"
	@sloccount Tests.hs | grep haskell:
	@echo "non-test code:"
	@sloccount `ls {,Ledger/}*.hs |grep -v Tests.hs` | grep haskell:

