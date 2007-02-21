build: Tags
	ghc --make hledger.hs

profile:
	ghc --make -prof -auto-all hledger.hs
	hledger -s bal +RTS -p
	T=`date +"%Y%m%d%H%M"`
	echo $(T)
	mv hledger.prof `date +"%Y%m%d%H%M"`.prof
	hledger -s bal +RTS -px
	mv hledger.prof `date +"%Y%m%d%H%M"`.xprof
	echo $(T)
	make xprof

xprof:
	ghcprof `ls -t1 *.xprof | head -1`

haddock:
	haddock -h -o doc *.hs

update-overview:
	runhaskell ./overview.hs >.ov; mv .ov OVERVIEW
#	./overview >.ov; mv .ov OVERVIEW

Tags:
	hasktags *hs

clean:
	rm -f *.o *.hi *~

Clean: clean
	rm -f hledger TAGS
