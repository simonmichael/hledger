# some hledger-related make scripts

HLEDGER=hledger

YEAR:=$(shell date +%Y)
MONTH:=$(shell date +%m)
MONTHS:=$(shell ghc -e "(putStr . unwords . map show) [1..$(MONTH)]")
MONTHS2:=$(shell ghc -e "(putStr . unwords . map show) [2..$(MONTH)]")

###############################################################################

# convert latest bank csv downloads to journal files
BANKJOURNALS = \
	mint.journal \
	WellsFargoChecking1.journal \
	WellsFargoSavings2.journal \
	WellsFargoSavings3.journal \
	WellsFargoCreditCard4.journal \
	Paypal.journal
convert-csv: move-csv $(BANKJOURNALS)

# move and rename any newly downloaded bank csv files
DOWNLOADDIR=~/Desktop
move-csv:
	@(F=$(DOWNLOADDIR)/transactions.csv; [ -e $$F ] && (mv $$F mint.csv; echo new mint.csv found) || exit 0)
	@(F=$(DOWNLOADDIR)/Checking1.csv; [ -e $$F ] && (mv $$F WellsFargoChecking1.csv; echo new WellsFargoChecking1.csv found) || exit 0)
	@(F=$(DOWNLOADDIR)/Savings2.csv; [ -e $$F ] && (mv $$F WellsFargoSavings2.csv; echo new WellsFargoSavings2.csv found) || exit 0)
	@(F=$(DOWNLOADDIR)/Savings3.csv; [ -e $$F ] && (mv $$F WellsFargoSavings3.csv; echo new WellsFargoSavings3.csv found) || exit 0)
	@(F=$(DOWNLOADDIR)/CreditCard4.csv; [ -e $$F ] && (mv $$F WellsFargoCreditCard4.csv; echo new WellsFargoCreditCard4.csv found) || exit 0)
	@(F=$(DOWNLOADDIR)/Download.csv; [ -e $$F ] && (mv $$F Paypal.csv; echo new Paypal.csv found) || exit 0)

# convert a csv to a journal using the similarly named rules file
%.journal: %.csv %.rules
	$(HLEDGER) convert $< >$@

%.rules:
	touch '$@'

