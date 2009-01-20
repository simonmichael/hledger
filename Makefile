
BUILD=ghc --make hledger.hs -o hledger -O
BUILDFLAGS=-DVTY
build: setbuildversion tag
	$(BUILD) $(BUILDFLAGS)

BUILDO2=ghc --make hledger.hs -o hledgero2 -O2 -fvia-C
buildo2:
	$(BUILDO2)

# rebuild all with normal optimisation
rebuild: clean build

# recompile and run tests whenever a module changes
# see http://searchpath.org , you may need the patched version from
# http://joyful.com/repos/searchpath
CICMD=test
continuous ci:
	sp --no-exts --no-default-map -o hledger ghc --make hledger.hs $(BUILDFLAGS) --run $(CICMD)

# run code tests
test:
	./hledger.hs test

PROFBIN=hledgerp
BUILDPROF=ghc --make hledger.hs -prof -auto-all -o $(PROFBIN)
RUNPROF=./$(PROFBIN) +RTS -p -RTS
PROFCMD=-s balance
TIME=`date +"%Y%m%d%H%M"`
profile:
	@echo "Profiling $(PROFCMD)"
	$(BUILDPROF)
	$(RUNPROF) $(PROFCMD) >/dev/null
	tools/simplifyprof.hs $(PROFBIN).prof >simple.prof
	cp simple.prof profs/$(TIME).prof
	cat simple.prof

# run performance benchmarks and save results in profs
# prepend ./ to these if not in $PATH
BENCHEXES=hledger ledger
bench: buildbench sampleledgers
	./bench $(BENCHEXES) | tee profs/`date +%Y%m%d%H%M%S`.bench

buildbench:
	ghc --make tools/bench.hs
	rm -f bench; ln -s tools/bench

sampleledgers:
	ghc -e 'putStr $$ unlines $$ replicate 1000 "!include sample.ledger"' >sample1000.ledger
#	ghc -e 'putStr $$ unlines $$ replicate 10000 "!include sample.ledger"' >sample10000.ledger
#	ghc -e 'putStr $$ unlines $$ replicate 100000 "!include sample.ledger"' >sample10000.ledger

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
#  the darcs release tag
#  the cabal file
#  the hackage pages and tarball filenames
#
# Goals and constraints for version numbering:
# 1 automation, robustness, simplicity, platform independence
# 2 cabal versions must be all-numeric
# 3 release versions should be concise
# 4 releases should have a corresponding darcs tag
# 5 development builds should have a precise version appearing in --version
# 6 development builds should generate cabal packages with non-confusing versions
# 7 would like a way to mark builds/releases as alpha or beta
# 8 would like to easily darcs get the .0 even with bugfix releases present
#
# Current plan:
# - Update the release version below, and record, before and/or after
#   "make release".
# - The release version looks like major.minor[.bugfix].  bugfix is 0 (and
#   elided) for a normal release, or 1..n for a bugfix release, or if
#   desired may be set to 98 meaning an alpha for the forthcoming release
#   or 99 meaning a beta. This is propagated during "make release".
# - The development build version is the non-elided release version plus
#   the number of patches added since the last release, ie
#   major.minor.bugfix.patches. This is propagated during "make".
# - The release tag is the non-elided release version.
RELEASE:=0.3.98

# build a cabal release, tag the repo and upload to hackage
# don't forget to first update and record RELEASE, if needed
release: check setreleaseversion tagrelease sdist #upload

ifeq ($(shell ghc -e "length (filter (=='.') \"$(RELEASE)\")"), 1)
RELEASE3:=$(RELEASE).0
else
RELEASE3:=$(RELEASE)
endif

# pre-release checks - cabal is happy, the code builds, tests pass..
check:
	cabal check
	cabal configure
	cabal build
	dist/build/hledger/hledger test 2>&1 | tail -1 | grep -q 'Errors: 0  Failures: 1'

# set the precise build version in local files, but don't record.
# This is used for development builds ("make").
setbuildversion:
	(export BUILD=$(RELEASE3).`expr \`darcs changes --count --from-tag=.\` - 1` \
		&& perl -p -e "s/(^version *= *)\".*?\"/\1\"$$BUILD\"/" -i Options.hs \
		&& perl -p -e "s/(^Version: *) .*/\1 $$BUILD/" -i hledger.cabal \
	)

# set the release version in local files (which should not have other
# pending edits!), and record.
setreleaseversion:
	perl -p -e "s/(^version *= *)\".*?\"/\1\"$(RELEASE)\"/" -i Options.hs \
	&& perl -p -e "s/(^Version: *) .*/\1 $(RELEASE)/" -i hledger.cabal \
	&& darcs record -a -m "bump version" Options.hs hledger.cabal

tagrelease:
	darcs tag $(RELEASE3)

sdist:
	cabal sdist

upload:
	cabal upload dist/hledger-$(RELEASE).tar.gz



tag:
	rm -f TAGS; hasktags -e *hs Ledger/*hs

clean:
	rm -f {,Ledger/}*{.o,.hi,~} darcs-amend-record*

Clean: clean clean-docs
	rm -f hledger TAGS tags

# docs

DOCS=README NEWS

docs: html pdf api-doc-frames

html:
	for d in $(DOCS); do rst2html $$d >doc/$$d.html; done
	cd doc; ln -f -s README.html index.html

pdf:
	for d in $(DOCS); do rst2pdf $$d -o doc/$$d.pdf; done

MAIN=hledger.hs

api-doc-dir:
	mkdir -p api-doc

HSCOLOUR=HsColour -css 
colourised-source hscolour: api-doc-dir
	echo "Generating colourised source" ; \
	for f in *hs Ledger/*hs; do \
		$(HSCOLOUR) -anchor $$f -oapi-doc/`echo "src/"$$f | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
	done ; \
	cp api-doc/src-hledger.html api-doc/src-Main.html ; \
	HsColour -print-css >api-doc/hscolour.css

# nb --ignore-all-exports means these are actually implementation docs
HADDOCK=haddock -B `ghc --print-libdir` --no-warnings --ignore-all-exports
api-doc-with-source haddock: api-doc-dir colourised-source $(MAIN)
	echo "Generating haddock api docs" ; \
	$(HADDOCK) -o api-doc -h --source-module=src-%{MODULE/./-}.html $(filter-out %api-doc-dir colourised-source,$^) ; \
	cp api-doc/index.html api-doc/modules-index.html
#--source-entity=src-%{MODULE/./-}.html#%N 

#generate a hoogle index
hoogleindex: $(MAIN)
	echo "Generating hoogle index" ; \
	mkdir -p hoogle && \
	$(HADDOCK) -o hoogle --hoogle $^ && \
	cd hoogle && \
	hoogle --convert=main.txt --output=default.hoo

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

# munge haddock and hoogle into a rough but useful framed layout
# ensure that the hoogle cgi is built with base target "main"
api-doc-frames: api-doc-with-source hoogleweb
	echo "Converting api docs to frames" ; \
	sed -i -e 's%^></HEAD%><base target="main"></HEAD%' api-doc/modules-index.html ; \
	cp doc/misc/api-doc-frames.html api-doc/index.html ; \
	cp doc/misc/hoogle-small.html hoogle

BROWSER=open
test-docs: docs
	$(BROWSER) api-doc/index.html
#	$(BROWSER) doc/README.html

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

sloc:
	@echo "test code:"
	@sloccount Tests.hs | grep haskell:
	@echo "non-test code:"
	@sloccount `ls {,Ledger/}*.hs |grep -v Tests.hs` | grep haskell:

