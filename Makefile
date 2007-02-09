build:
	ghc --make -O2 hledger.hs

Tags:
	hasktags *hs

clean:
	rm -f *.o *.hi *~

Clean: clean
	rm -f hledger TAGS
