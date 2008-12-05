BUILD=ghc --make hledger.hs -o hledger -O2
BUILDPROF=$(BUILD) -prof -auto-all
PROFILE=./hledger -s balance +RTS -p
TIME=`date +"%Y%m%d%H%M"`

build: tag
	$(BUILD)

rebuild: clean build

# build and run whenever a module changes, see http://searchpath.org
# or the patched http://joyful.com/repos/searchpath
continuous ci:
	sp --no-exts --no-default-map -o hledger ghc --make hledger.hs --run test

profile: build
	$(PROFILE)
	mv hledger.prof profs/$(TIME).prof
	rm -f last.prof
	ln -s profs/$(TIME).prof last.prof
	head -20 profs/$(TIME).prof >simple.prof
	cat simple.prof
	./simplifyprof.hs <last.prof >>simple.prof

xprofile: build
	$(PROFILE) -x
	mv hledger.prof profs/$(TIME).xprof
	ghcprof profs/$(TIME).xprof

test:
	@./hledger.hs test
	@./regtest.py

# run performance benchmarks and save results in profs
BENCHEXES=hledger ledger
BENCHITERATIONS=2
bench:
	tools/bench.hs bench.tests $(BENCHITERATIONS) $(BENCHEXES) | tee profs/`date +%Y%m%d%H%M%S`.bench

VERSION=`grep 'versionno =' Options.hs | perl -pe 's/.*"(.*?)"/\1/'`
release:
	cabal sdist && darcs tag $(VERSION) && cabal upload dist/hledger-$(VERSION).tar.gz

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
	cd doc; ln -s README.html index.html

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
test-docs: api-doc-frames
	$(BROWSER) api-doc/index.html

clean-docs:
	rm -rf api-doc hoogle

# misc

show-changes:
	@echo Changes since last release:
	@echo
	@darcs changes --from-tag . | grep '*'

show-authors:
	@echo Patch authors since last release:
	@echo
	@darcs changes --from-tag . |grep '^\w' |cut -c 31- |sort |uniq

sloc:
	@echo "test code:"
	@sloccount Tests.hs | grep haskell:
	@echo "non-test code:"
	@sloccount `ls {,Ledger/}*.hs |grep -v Tests.hs` | grep haskell:

