build:
	ghc --make hledger.hs

clean:
	rm -f *.o *.hi *~

Clean: clean
	rm -f hledger
