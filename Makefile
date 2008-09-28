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

docs haddock:
	haddock -h -o doc *.hs

loc:
	@darcs trackdown 'find . -name "*hs" |xargs wc -l |echo OUTPUT `tail -1`; false' |ruby -nae'puts $$F[1] if /^OUTPUT/'

Tags:
	hasktags *hs

clean:
	rm -f *.o *.hi *~ 1 2

Clean: clean
	rm -f hledger hledgeropt TAGS tags

