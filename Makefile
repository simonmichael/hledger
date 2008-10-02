BUILD=ghc --make hledger.hs -o hledger
BUILDOPT=$(BUILD)opt -O2
BUILDPROF=$(BUILD) -prof -auto-all
PROFILE=./hledger -s balance +RTS -p
TIME=`date +"%Y%m%d%H%M"`

build: Tags
	$(BUILD)

buildopt opt: clean
	$(BUILDOPT)

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

loc:
	@darcs trackdown 'find . -name "*hs" |xargs wc -l |echo OUTPUT `tail -1`; false' |ruby -nae'puts $$F[1] if /^OUTPUT/'

Tags:
	hasktags *hs

clean:
	rm -f *.o *.hi *~ 1 2

clean-docs:
	rm -rf api-doc hoogle

Clean: clean clean-docs
	rm -f hledger hledgeropt TAGS tags

# docs

MAIN=hledger.hs

docs: haddock-with-source hoogleweb api-doc-frames

api-doc-dir:
	mkdir -p api-doc

api-doc: api-doc-dir $(MAIN)
	echo "Generating haddock api docs" ; \
	haddock --no-warnings --ignore-all-exports -B `ghc --print-libdir` -o api-doc -h $(filter-out %api-doc-dir,$^) ; \
	cp api-doc/index.html api-doc/modules-index.html

api-doc-with-source: api-doc-dir colourised-source $(MAIN)
	echo "Generating haddock api docs" ; \
	haddock --no-warnings --ignore-all-exports -B `ghc --print-libdir` -o api-doc -h --source-module=src-%{MODULE/./-}.html --source-entity=src-%{MODULE/./-}.html#%N $(filter-out %api-doc-dir colourised-source,$^) ; \
	cp api-doc/index.html api-doc/modules-index.html

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

colourised-source: api-doc-dir
	echo "Generating colourised source" ; \
	for f in *hs; do \
		HsColour -css -anchor $$f -oapi-doc/`echo "src/"$$f | sed -e's%/%-%g' | sed -e's%\.hs$$%.html%'` ; \
	done ; \
	cp api-doc/src-hledger.html api-doc/src-Main.html ; \
	HsColour -print-css >api-doc/hscolour.css

#generate a hoogle index
#uses system hoogle, works around http://code.google.com/p/ndmitchell/issues/detail?id=93
#to use: hoogle --data=hoogle/default ...
hoogleindex: $(MAIN)
	echo "Generating hoogle index" ; \
	mkdir -p hoogle && \
	haddock --no-warnings --ignore-all-exports -B `ghc --print-libdir` -o hoogle --hoogle $^ && \
	cd hoogle && \
	sed -i -e 's/^(_/-- (_/' main.txt && \
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
