BUILD=ghc --make hledger.hs -o hledger -prof -auto-all # -O2
TOPROFILE=hledger -s balance
TIME=`date +"%Y%m%d%H%M"`

build: Tags
	$(BUILD)


profile prof:
	$(BUILD) -prof -auto-all
	$(TOPROFILE) +RTS -p
	mv hledger.prof $(TIME).prof
	cat $(TIME).prof

xprofile xprof:
	$(BUILD) -prof -auto-all
	$(TOPROFILE) +RTS -px
	mv hledger.prof $(TIME).prof
	ghcprof $(TIME).prof

haddock:
	haddock -h -o doc *.hs

overview:
	./overview.hs hledger.hs

loc:
	@darcs trackdown 'find . -name "*hs" |xargs wc -l |echo OUTPUT `tail -1`; false' |ruby -nae'puts $$F[1] if /^OUTPUT/'

Tags:
	hasktags *hs

clean:
	rm -f *.o *.hi *~

Clean: clean
	rm -f hledger overview TAGS tags
